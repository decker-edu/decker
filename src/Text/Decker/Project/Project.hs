{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.Project
  ( scanTargetsToFile,
    setProjectDirectory,
    -- , dachdeckerFromMeta
    unusedResources,
    scanTargets,
    excludeDirs,
    staticDirs,
    static,
    sources,
    resources,
    decks,
    decksPdf,
    pages,
    pagesPdf,
    handouts,
    handoutsPdf,
    questions,
    annotations,
    recordings,
    times,
    captions,
    css,
    Targets (..),
    fromMetaValue,
    toMetaValue,
    readTargetsFile,
  )
where

-- import Text.Decker.Internal.Flags

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml
import Development.Shake hiding (Resource)
import Relude
import qualified System.Directory as Directory
import qualified System.FilePath as FP
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
  ( FromMetaValue (..),
    globalMetaFileName,
    lookupMetaOrElse,
  )
import Text.Decker.Project.Glob
import Text.Decker.Resource.Resource
import Text.Pandoc.Builder hiding (lookupMeta)
import Text.Regex.TDFA

data Targets = Targets
  { _sources :: [FilePath],
    _resources :: Map FilePath Source,
    _static :: [FilePath],
    _decks :: [FilePath],
    _decksPdf :: [FilePath],
    _pages :: [FilePath],
    _pagesPdf :: [FilePath],
    _handouts :: [FilePath],
    _handoutsPdf :: [FilePath],
    _questions :: [FilePath],
    _annotations :: [FilePath],
    _times :: [FilePath],
    _captions :: [FilePath],
    _recordings :: [FilePath],
    _css :: [FilePath]
  }
  deriving (Show)

makeLenses ''Targets

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = drop 1,
         constructorTagModifier = map toLower
       }
     ''Targets
 )

readTargetsFile :: FilePath -> Action Targets
readTargetsFile targetFile = do
  need [targetFile]
  liftIO (Yaml.decodeFileThrow targetFile)

-- data Resource = Resource
--   { -- | Absolute Path to source file
--     sourceFile :: FilePath,
--     -- | Absolute path to file in public folder
--     publicFile :: FilePath,
--     -- | Relative URL to served file from base
--     publicUrl :: FilePath
--   }
--   deriving (Eq, Show, Generic)

-- instance ToJSON Resource where
--   toJSON (Resource source target url) =
--     object ["source" .= source, "target" .= target, "url" .= url]

-- instance FromJSON Resource where
--   parseJSON =
--     withObject "Resource" $ \v ->
--       Resource <$> v .: "source" <*> v .: "target" <*> v .: "url"

-- instance {-# OVERLAPS #-} ToMetaValue a => ToMetaValue [(Text, a)] where
--   toMetaValue = MetaMap . Map.fromList . map (second toMetaValue)

-- instance ToMetaValue Resource where
--   toMetaValue (Resource source target url) =
--     toMetaValue
--       [ ("source" :: Text, source),
--         ("target" :: Text, target),
--         ("url" :: Text, url)
--       ]

-- instance {-# OVERLAPS #-} FromMetaValue a => FromMetaValue [(Text, a)] where
--   fromMetaValue (MetaMap object) =
--     let kes :: Map Text (Maybe a) =
--           Map.filter isJust $ Map.map fromMetaValue object
--      in Just $ zip (Map.keys kes) (map fromJust (Map.elems kes))
--   fromMetaValue _ = Nothing

-- instance FromMetaValue Resource where
--   fromMetaValue (MetaMap object) = do
--     source <- Map.lookup "source" object >>= fromMetaValue
--     target <- Map.lookup "target" object >>= fromMetaValue
--     url <- Map.lookup "url" object >>= fromMetaValue
--     return $ Resource source target url
--   fromMetaValue _ = Nothing

-- | Find the project directory.
-- 1. First upwards directory containing `decker.yaml`
-- 2. First upwards directory containing `.git`
-- 3. The current working directory
findProjectRoot :: IO FilePath
findProjectRoot = do
  cwd <- Directory.getCurrentDirectory
  search cwd cwd
  where
    search :: FilePath -> FilePath -> IO FilePath
    search dir start = do
      hasYaml <- Directory.doesFileExist (dir </> globalMetaFileName)
      hasGit <- Directory.doesDirectoryExist (dir </> ".git")
      if
          | hasYaml || hasGit -> return dir
          | FP.isDrive dir -> return start
          | otherwise -> search (FP.takeDirectory dir) start

-- Move CWD to the project directory.
setProjectDirectory :: IO ()
setProjectDirectory = do
  projectDir <- findProjectRoot
  Directory.setCurrentDirectory projectDir
  putStrLn $ "# Running decker in: " <> projectDir

deckSuffix = "-deck.md"

deckHTMLSuffix = "-deck.html"

deckPDFSuffix = "-deck.pdf"

pageSuffix = "-page.md"

pageHTMLSuffix = "-page.html"

pagePDFSuffix = "-page.pdf"

handoutHTMLSuffix = "-handout.html"

handoutPDFSuffix = "-handout.pdf"

annotationSuffix = "-annot.json"

timesSuffix = "-times.json"

recordingSuffix1 = "-recording.webm"

recordingSuffix2 = "-recording.mp4"

recordingTargetSuffix = "-recording.mp4"

captionsSuffix = "-recording.vtt"

sourceRegexes :: [String] =
  [ "-deck.md\\'",
    "-page.md\\'",
    "-deck-index.yaml\\'",
    "-quest.yaml\\'",
    "-recording.webm\\'",
    "-recording.mp4\\'",
    "-recording.vtt\\'",
    "-times.json\\'",
    "-annot.json\\'",
    "\\`(^_).*\\.scss\\'"
  ]

alwaysExclude = [publicDir, transientDir, "dist", ".git", ".vscode"]

questSuffix = "-quest.yaml"

questHTMLSuffix = "-quest.html"

excludeDirs :: Meta -> [String]
excludeDirs meta =
  map normalise $
    alwaysExclude <> lookupMetaOrElse [] "exclude-directories*" meta

staticDirs = lookupMetaOrElse [] "static-resource-dirs*"

unusedResources :: Meta -> IO [FilePath]
unusedResources meta = do
  srcs <- Set.fromList <$> fastGlobFiles (excludeDirs meta) [] projectDir
  live <- Set.fromList <$> String.lines <$> readFile liveFile
  return $ Set.toList $ Set.difference srcs live

scanTargetsToFile :: Meta -> FilePath -> Action ()
scanTargetsToFile meta file = do
  targets <- liftIO $ scanTargets meta
  writeFileChanged file $ decodeUtf8 $ Yaml.encodePretty Yaml.defConfig targets

anySource :: FilePath -> Bool
anySource file = any (file =~) sourceRegexes

scanTargets :: Meta -> IO Targets
scanTargets meta = do
  -- srcs <- globFiles (excludeDirs meta) sourceSuffixes projectDir
  srcs <- fastGlobFiles' (excludeDirs meta) anySource projectDir
  supportFiles <- Map.mapKeys ((publicDir </> "support") </>) <$> publicSupportFiles meta
  staticSrc <-
    concat <$> mapM (fastGlobFiles [] [] . normalise) (staticDirs meta)
  return
    Targets
      { _sources = sort srcs,
        _resources = supportFiles,
        _static = sort $ map (publicDir </>) staticSrc,
        _decks = sort $ calcTargets deckSuffix deckHTMLSuffix srcs,
        _decksPdf = sort $ calcTargets deckSuffix deckPDFSuffix srcs,
        _pages = sort $ calcTargets pageSuffix pageHTMLSuffix srcs,
        _pagesPdf = sort $ calcTargets pageSuffix pagePDFSuffix srcs,
        _handouts = sort $ calcTargets deckSuffix handoutHTMLSuffix srcs,
        _handoutsPdf = sort $ calcTargets deckSuffix handoutPDFSuffix srcs,
        _questions = sort $ calcTargets questSuffix questHTMLSuffix srcs,
        _annotations = sort $ calcTargets annotationSuffix annotationSuffix srcs,
        _times = sort $ calcTargets timesSuffix timesSuffix srcs,
        _captions = sort $ calcTargets captionsSuffix captionsSuffix srcs,
        _recordings =
          List.nub $
            sort $
              calcTargets recordingSuffix1 recordingTargetSuffix srcs
                <> calcTargets recordingSuffix2 recordingTargetSuffix srcs,
        _css = sort $ calcTargets ".scss" ".css" srcs
      }
  where
    calcTarget :: String -> String -> FilePath -> FilePath
    calcTarget srcSuffix targetSuffix source =
      publicDir
        </> replaceSuffix srcSuffix targetSuffix source
    calcTargets :: String -> String -> [FilePath] -> [FilePath]
    calcTargets srcSuffix targetSuffix sources =
      map (calcTarget srcSuffix targetSuffix) $
        filter (srcSuffix `List.isSuffixOf`) sources
