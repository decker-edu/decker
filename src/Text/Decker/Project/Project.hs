{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.Project
  ( resourcePaths
  , scanTargetsToFile
  , deckerResourceDir
  , oldResourcePaths
  -- , linkResource
  , relRefResource
  , absRefResource
  , removeCommonPrefix
  , isPrefix
  , makeRelativeTo
  , findProjectDirectory
  , projectDirectories
  , provisioningFromMeta
  , dachdeckerFromMeta
  , invertPath
  , scanTargets
  , isDevelopmentRun
  , excludeDirs
  , staticDirs
  -- * Types
  , sources
  , decks
  , decksPdf
  , pages
  , pagesPdf
  , handouts
  , handoutsPdf
  , projectDir
  , publicDir
  , project
  , public
  , support
  , transient
  , getDachdeckerUrl
  , Targets(..)
  , Resource(..)
  , ProjectDirs(..)
  , fromMetaValue
  , readTargetsFile
  ) where

import Text.Decker.Internal.Common

-- import Text.Decker.Internal.Flags
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Glob
import Text.Decker.Project.Version

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import Development.Shake hiding (Resource)
import Network.URI
import Relude
import qualified System.Directory as D
import System.Environment
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Read
import Text.Regex.TDFA

data Targets = Targets
  { _sources :: [FilePath]
  , _static :: [FilePath]
  , _decks :: [FilePath]
  , _decksPdf :: [FilePath]
  , _pages :: [FilePath]
  , _pagesPdf :: [FilePath]
  , _handouts :: [FilePath]
  , _handoutsPdf :: [FilePath]
  , _annotations :: [FilePath]
  } deriving (Show)

makeLenses ''Targets

$(deriveJSON
    defaultOptions
      {fieldLabelModifier = drop 1, constructorTagModifier = map toLower}
    ''Targets)

readTargetsFile :: FilePath -> Action Targets
readTargetsFile targetFile = do
  need [targetFile]
  liftIO (Yaml.decodeFileThrow targetFile)

data Resource = Resource
  { sourceFile :: FilePath -- ^ Absolute Path to source file
  , publicFile :: FilePath -- ^ Absolute path to file in public folder
  , publicUrl :: FilePath -- ^ Relative URL to served file from base
  } deriving (Eq, Show, Generic)

instance ToJSON Resource where
  toJSON (Resource source target url) =
    object ["source" .= source, "target" .= target, "url" .= url]

instance FromJSON Resource where
  parseJSON =
    withObject "Resource" $ \v ->
      Resource <$> v .: "source" <*> v .: "target" <*> v .: "url"

class ToMetaValue a where
  toMetaValue :: a -> MetaValue

instance ToMetaValue MetaValue where
  toMetaValue = id

instance {-# OVERLAPS #-} ToMetaValue a => ToMetaValue [a] where
  toMetaValue = MetaList . map toMetaValue

instance ToMetaValue a => ToMetaValue [(Text, a)] where
  toMetaValue = MetaMap . Map.fromList . map (second toMetaValue)

instance ToMetaValue Text where
  toMetaValue = MetaString

instance ToMetaValue String where
  toMetaValue = MetaString . Text.pack

instance ToMetaValue Resource where
  toMetaValue (Resource source target url) =
    toMetaValue
      [ ("source" :: Text, source)
      , ("target" :: Text, target)
      , ("url" :: Text, url)
      ]

class FromMetaValue a where
  fromMetaValue :: MetaValue -> Maybe a

instance {-# OVERLAPS #-} FromMetaValue a => FromMetaValue [a] where
  fromMetaValue (MetaList list) = Just $ mapMaybe fromMetaValue list
  fromMetaValue _ = Nothing

instance FromMetaValue a => FromMetaValue [(Text, a)] where
  fromMetaValue (MetaMap object) =
    let kes :: Map Text (Maybe a) =
          Map.filter isJust $ Map.map fromMetaValue object
     in Just $ zip (Map.keys kes) (map fromJust (Map.elems kes))
  fromMetaValue _ = Nothing

instance FromMetaValue Text where
  fromMetaValue (MetaString text) = Just text
  fromMetaValue (MetaInlines inlines) = Just $ stringify inlines
  fromMetaValue _ = Nothing

instance FromMetaValue String where
  fromMetaValue v = Text.unpack <$> fromMetaValue v

rightToMaybe (Right r) = Just r

rightRoMaybe (Left l) = Nothing

instance FromMetaValue Resource where
  fromMetaValue (MetaMap object) = do
    source <- Map.lookup "source" object >>= fromMetaValue
    target <- Map.lookup "target" object >>= fromMetaValue
    url <- Map.lookup "url" object >>= fromMetaValue
    return $ Resource source target url
  fromMetaValue _ = Nothing

data ProjectDirs = ProjectDirs
  { _project :: FilePath
  , _public :: FilePath
  , _support :: FilePath
  , _transient :: FilePath
  } deriving (Eq, Show)

makeLenses ''ProjectDirs

$(deriveJSON
    defaultOptions
      {fieldLabelModifier = drop 1, constructorTagModifier = map toLower}
    ''ProjectDirs)

provisioningFromMeta :: Meta -> Provisioning
provisioningFromMeta meta =
  fromMaybe SymLink $ getMetaString "provisioning" meta >>= readMaybe

dachdeckerFromMeta :: Meta -> Maybe String
dachdeckerFromMeta = getMetaString "dachdecker"

provisioningClasses :: [(String, Provisioning)]
provisioningClasses =
  [ ("copy", Copy)
  , ("symlink", SymLink)
  , ("absolute", Absolute)
  , ("relative", Relative)
  ]

absRefResource :: Resource -> IO FilePath
absRefResource resource =
  return $ show $ URI "file" Nothing (sourceFile resource) "" ""

relRefResource :: FilePath -> Resource -> IO FilePath
relRefResource base resource = do
  let relPath = makeRelativeTo base (sourceFile resource)
  return $ show $ URI "file" Nothing relPath "" ""

-- | Find the project directory. 
-- 1. First upwards directory containing `decker.yaml`
-- 2. First upwards directory containing `.git`
-- 3. The current working directory
findProjectDirectory :: IO FilePath
findProjectDirectory = do
  cwd <- D.getCurrentDirectory
  searchRoot cwd Nothing
  where
    searchRoot :: FilePath -> Maybe FilePath -> IO FilePath
    searchRoot start gitRoot = do
      let parent = takeDirectory start
      hasYaml <- D.doesFileExist (start </> globalMetaFileName)
      hasGit <- D.doesDirectoryExist (start </> ".git")
      if hasYaml
        then D.makeAbsolute start
        else if isDrive start
               then case gitRoot of
                      Just g -> D.makeAbsolute g
                      Nothing -> D.makeAbsolute "."
               else case (hasGit, gitRoot) of
                      (_, Just g) -> searchRoot parent gitRoot
                      (True, Nothing) -> searchRoot parent (Just start)
                      _ -> searchRoot parent Nothing

-- Calculate important absolute project directory pathes
projectDirectories :: IO ProjectDirs
projectDirectories = do
  projectDir <- findProjectDirectory
  let publicDir = projectDir </> "public"
  let supportDir = publicDir </> "support"
  let transientDir = projectDir </> deckerFiles
  return (ProjectDirs projectDir publicDir supportDir transientDir)

deckerResourceDir :: IO FilePath
deckerResourceDir =
  D.getXdgDirectory
    D.XdgData
    ("decker" ++
     "-" ++ deckerVersion ++ "-" ++ deckerGitBranch ++ "-" ++ deckerGitCommitId)

-- | Get the absolute paths of resource folders 
-- with version numbers older than the current one
oldResourcePaths :: IO [FilePath]
oldResourcePaths = do
  dir <- D.getXdgDirectory D.XdgData []
  files <- D.listDirectory dir
  return $ map (dir </>) $ filter oldVersion files
  where
    convert = map (read :: String -> Int)
    currentVersion = convert (splitOn "." deckerVersion)
    deckerRegex = "decker-([0-9]+)[.]([0-9]+)[.]([0-9]+)-" :: String
    oldVersion name =
      case getAllTextSubmatches (name =~ deckerRegex) :: [String] of
        [] -> False
        _:x:y:z:_ -> convert [x, y, z] < currentVersion

resourcePaths :: ProjectDirs -> FilePath -> URI -> Resource
resourcePaths dirs base uri =
  Resource
    { sourceFile = uriPath uri
    , publicFile =
        dirs ^. public </> makeRelativeTo (dirs ^. project) (uriPath uri)
    , publicUrl =
        show $
        URI
          ""
          Nothing
          (makeRelativeTo base (uriPath uri))
          (uriQuery uri)
          (uriFragment uri)
    }

-- | Express the second path argument as relative to the first. 
-- Both arguments are expected to be absolute pathes. 
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
  let (d, f) = removeCommonPrefix (normalise dir, normalise file)
   in normalise $ invertPath d </> f

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

removeCommonPrefix :: (FilePath, FilePath) -> (FilePath, FilePath)
removeCommonPrefix =
  mapTuple joinPath . removeCommonPrefix_ . mapTuple splitDirectories
  where
    removeCommonPrefix_ :: ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
    removeCommonPrefix_ (al@(a:as), bl@(b:bs))
      | a == b = removeCommonPrefix_ (as, bs)
      | otherwise = (al, bl)
    removeCommonPrefix_ pathes = pathes

isPrefix :: FilePath -> FilePath -> Bool
isPrefix prefix whole = isPrefix_ (splitPath prefix) (splitPath whole)
  where
    isPrefix_ :: Eq a => [a] -> [a] -> Bool
    isPrefix_ (a:as) (b:bs)
      | a == b = isPrefix_ as bs
      | otherwise = False
    isPrefix_ [] _ = True
    isPrefix_ _ _ = False

mapTuple :: (t1 -> t) -> (t1, t1) -> (t, t)
mapTuple f (a, b) = (f a, f b)

deckSuffix = "-deck.md"

deckHTMLSuffix = "-deck.html"

deckPDFSuffix = "-deck.pdf"

pageSuffix = "-page.md"

pageHTMLSuffix = "-page.html"

pagePDFSuffix = "-page.pdf"

handoutSuffix = "-deck.md"

handoutHTMLSuffix = "-handout.html"

handoutPDFSuffix = "-handout.pdf"

annotationSuffix = "-annot.json"

indexSuffix = "-deck-index.yaml"

sourceSuffixes = [deckSuffix, pageSuffix, annotationSuffix, indexSuffix]

alwaysExclude = ["public", deckerFiles, "dist", ".git", ".vscode"]

excludeDirs :: Meta -> [String]
excludeDirs meta =
  alwaysExclude <> getMetaStringListOrElse "exclude-directories" [] meta

staticDirs = getMetaStringListOrElse "static-resource-dirs" []

scanTargetsToFile :: Meta -> ProjectDirs -> FilePath -> Action ()
scanTargetsToFile meta dirs file = do
  targets <- liftIO $ scanTargets meta dirs
  writeFileChanged file $ decodeUtf8 $ encode targets

scanTargets :: Meta -> ProjectDirs -> IO Targets
scanTargets meta dirs = do
  let exclude = excludeDirs meta
  srcs <- globFiles (excludeDirs meta) sourceSuffixes projectDir
  let static = map (dirs ^. project </>) (staticDirs meta)
  staticSrc <- concat <$> mapM (fastGlobFiles [] []) static
  let staticTargets =
        map ((dirs ^. public </>) . makeRelative (dirs ^. project)) staticSrc
  return
    Targets
      { _sources = sort $ concatMap snd srcs
      , _static = staticTargets
      , _decks = sort $ calcTargets deckSuffix deckHTMLSuffix srcs
      , _decksPdf = sort $ calcTargets deckSuffix deckPDFSuffix srcs
      , _pages = sort $ calcTargets pageSuffix pageHTMLSuffix srcs
      , _pagesPdf = sort $ calcTargets pageSuffix pagePDFSuffix srcs
      , _handouts = sort $ calcTargets deckSuffix handoutHTMLSuffix srcs
      , _handoutsPdf = sort $ calcTargets deckSuffix handoutPDFSuffix srcs
      , _annotations = sort $ calcTargets annotationSuffix annotationSuffix srcs
      }
  where
    projectDir = dirs ^. project
    calcTargets :: String -> String -> [(String, [FilePath])] -> [FilePath]
    calcTargets srcSuffix targetSuffix sources =
      map
        (replaceSuffix srcSuffix targetSuffix .
         combine (dirs ^. public) . makeRelative (dirs ^. project))
        (fromMaybe [] $ List.lookup srcSuffix sources)

getDachdeckerUrl :: IO String
getDachdeckerUrl = do
  env <- System.Environment.lookupEnv "DACHDECKER_SERVER"
  let url =
        case env of
          Just val -> val
          Nothing -> "https://dach.decker.informatik.uni-wuerzburg.de"
  return url

projectDir :: Meta -> FilePath
projectDir = getMetaStringOrElse "decker.directories.project" "."

publicDir :: Meta -> FilePath
publicDir = getMetaStringOrElse "decker.directories.public" "."
