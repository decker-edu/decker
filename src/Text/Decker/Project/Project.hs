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
    unusedResources,
    scanTargets,
    excludeDirs,
    excludeGlob,
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
    Dependencies,
    Targets (..),
    lookupSource,
    fromMetaValue,
    toMetaValue,
    readTargetsFile,
    alwaysExclude,
    calcTargets
  )
where

-- import Text.Decker.Internal.Flags

import Control.Exception.Extra
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String qualified as String
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as Yaml
import Development.Shake hiding (Resource)
import Relude
import System.Directory qualified as Directory
import System.FilePath qualified as FP
import System.FilePath.Glob
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
  ( FromMetaValue (..),
    lookupMetaOrElse,
  )
import Text.Decker.Project.Glob
import Text.Decker.Resource.Resource
import Text.Pandoc.Builder hiding (lookupMeta)
import Text.Regex.TDFA

-- | target and source path
type Dependencies = Map FilePath FilePath

data Targets = Targets
  { _sources :: [FilePath],
    _resources :: Map FilePath Source,
    _static :: Dependencies,
    _decks :: Dependencies,
    _decksPdf :: Dependencies,
    _pages :: Dependencies,
    _pagesPdf :: Dependencies,
    _handouts :: Dependencies,
    _handoutsPdf :: Dependencies,
    _questions :: Dependencies
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
  -- we do not really have to track the dependency here, it just needs to exist
  -- need [targetFile]
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
      hasYaml <- Directory.doesFileExist (dir </> deckerMetaFile)
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

sourceRegexes :: [String] =
  [ "-deck.md\\'",
    "-page.md\\'",
    "-deck-index.yaml\\'",
    "-quest.yaml\\'",
    "\\`(^_).*\\.scss\\'"
  ]

alwaysExclude = [publicDir, "dist", ".git", ".vscode", ".stack-work"]

questSuffix = "-quest.yaml"

questHTMLSuffix = "-quest.html"

excludeDirs :: Meta -> [String]
excludeDirs meta =
  map normalise
    $ alwaysExclude
    <> lookupMetaOrElse [] "exclude-directories" meta

-- glob patterns used to exclude paths from watching
excludeGlob :: Meta -> [Pattern]
excludeGlob = map compile . lookupMetaOrElse [] "watch.exclude"

staticResources meta =
  lookupMetaOrElse [] "static-resource-dirs" meta
    <> lookupMetaOrElse [] "static-resources" meta

unusedResources :: Meta -> IO [FilePath]
unusedResources meta = do
  live <- liveFile
  srcs <- Set.fromList <$> fastGlobFiles (excludeDirs meta) [] projectDir
  live <- Set.fromList . String.lines . decodeUtf8 <$> readFileBS live
  return $ Set.toList $ Set.difference srcs live

scanTargetsToFile :: (MonadIO m, Partial) => Meta -> FilePath -> m ()
scanTargetsToFile meta file = do
  targets <- liftIO $ scanTargets meta
  -- liftIO $ putStrLn $ "# scanned targets to " <> file
  writeFileChanged file $ decodeUtf8 $ Yaml.encodePretty Yaml.defConfig targets

anySource :: FilePath -> Bool
anySource file = any (file =~) sourceRegexes

lookupSource :: Getting Dependencies Targets Dependencies -> FilePath -> Targets -> FilePath
lookupSource which path targets =
  fromMaybe
    (error $ "No source known for target: " <> toText path)
    (Map.lookup path (targets ^. which))

scanTargets :: Meta -> IO Targets
scanTargets meta = do
  -- srcs <- globFiles (excludeDirs meta) sourceSuffixes projectDir
  srcs <- fastGlobFiles' (excludeDirs meta) anySource projectDir
  supportFiles <- Map.mapKeys ((publicDir </> "support") </>) <$> publicSupportFiles meta
  staticSrc <- concat <$> mapM (fastGlobFiles [] [] . normalise) (staticResources meta)
  return
    Targets
      { _sources = sort srcs,
        _resources = supportFiles,
        _static = Map.fromList $ map publicDep staticSrc,
        _decks = calcTargets deckSuffix deckHTMLSuffix srcs,
        _decksPdf = calcTargets deckSuffix deckPDFSuffix srcs,
        _pages = calcTargets pageSuffix pageHTMLSuffix srcs,
        _pagesPdf = calcTargets pageSuffix pagePDFSuffix srcs,
        _handouts = calcTargets deckSuffix handoutHTMLSuffix srcs,
        _handoutsPdf = calcTargets deckSuffix handoutPDFSuffix srcs,
        _questions = calcPrivateTargets questSuffix questHTMLSuffix srcs
      }
  where
    publicDep src = (publicDir </> src, src)

calcTargets = calcTargets' publicDir

calcPrivateTargets = calcTargets' privateDir

calcTarget baseDir srcSuffix targetSuffix source =
    baseDir </> replaceSuffix srcSuffix targetSuffix source

calcTargets' baseDir srcSuffix targetSuffix sources =
    Map.fromList
    $ map (\s -> (calcTarget baseDir srcSuffix targetSuffix s, s))
    $ filter (srcSuffix `List.isSuffixOf`) sources
