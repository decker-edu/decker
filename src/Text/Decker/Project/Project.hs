{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
-- | Providing an interface for the paths used in decker
-- 
module Text.Decker.Project.Project
  ( resourcePaths
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
  , project
  , public
  , cache
  , support
  , appData
  , logging
  , getDachdeckerUrl
  , Targets(..)
  , Resource(..)
  , ProjectDirs(..)
  ) where

import System.Decker.OS
import Text.Decker.Internal.Common
import Text.Decker.Internal.Flags
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Glob
import Text.Decker.Project.Version

import Control.Lens
import Data.Aeson as Json
import Data.Aeson.Lens
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text.Lens
import Network.URI
import qualified System.Directory as D
import System.Environment
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc.Shared
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
  , _indices :: [FilePath]
  } deriving (Show)

makeLenses ''Targets

data Resource = Resource
  { sourceFile :: FilePath -- ^ Absolute Path to source file
  , publicFile :: FilePath -- ^ Absolute path to file in public folder
  , publicUrl :: FilePath -- ^ Relative URL to served file from base
  } deriving (Eq, Show)

data ProjectDirs = ProjectDirs
  { _project :: FilePath
  , _public :: FilePath
  , _cache :: FilePath
  , _support :: FilePath
  , _appData :: FilePath
  , _logging :: FilePath
  } deriving (Eq, Show)

makeLenses ''ProjectDirs

provisioningFromMeta :: Meta -> Provisioning
provisioningFromMeta meta =
  case lookupMeta "provisioning" meta of
    Just (MetaString s) -> read s
    Just (MetaInlines i) -> read $ stringify i
    _ -> SymLink

dachdeckerFromMeta :: Meta -> Maybe String
dachdeckerFromMeta meta =
  case lookupMeta "dachdecker" meta of
    Just (MetaString s) -> Just s
    Just (MetaInlines i) -> Just $ stringify i
    _ -> Nothing

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

-- | Find the project directory. The project directory is the first upwards
-- directory that contains a .git directory entry.
findProjectDirectory :: IO FilePath
findProjectDirectory = do
  cwd <- D.getCurrentDirectory
  searchGitRoot cwd
  where
    searchGitRoot :: FilePath -> IO FilePath
    searchGitRoot start =
      if isDrive start
        then D.makeAbsolute "."
        else do
          hasGit <- D.doesDirectoryExist (start </> ".git")
          if hasGit
            then D.makeAbsolute start
            else searchGitRoot $ takeDirectory start

-- Calculate important absolute project directory pathes
projectDirectories :: IO ProjectDirs
projectDirectories = do
  projectDir <- findProjectDirectory
  let publicDir = projectDir </> "public"
  let cacheDir = publicDir </> "cache"
  let supportDir = publicDir </> "support"
  appDataDir <- deckerResourceDir
  let logDir = projectDir </> "log"
  return
    (ProjectDirs projectDir publicDir cacheDir supportDir appDataDir logDir)

deckerResourceDir :: IO FilePath
deckerResourceDir =
  if hasPreextractedResources
    then preextractedResourceFolder
    else D.getXdgDirectory
           D.XdgData
           ("decker" ++
            "-" ++
            deckerVersion ++ "-" ++ deckerGitBranch ++ "-" ++ deckerGitCommitId)

-- | Find out if the decker executable is located below the current directory.
-- This means most probably that decker was started in the decker development
-- project using `stack run decker`.
isDevelopmentRun :: IO Bool
isDevelopmentRun = do
  cwd <- D.getCurrentDirectory
  exePath <- getExecutablePath
  return $ cwd `isPrefixOf` exePath

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

metaSuffix = "-meta.yaml"

indexSuffix = "-deck-index.yaml"

sourceSuffixes = [deckSuffix, pageSuffix, indexSuffix]

alwaysExclude = ["public", "log", "dist", "code", ".shake", ".git", ".vscode"]

-- excludeDirs :: Value -> [String]
-- excludeDirs meta =
--   let metaExclude =
--         meta ^.. key "exclude-directories" . values . _String . unpacked
--    in alwaysExclude ++ metaExclude
-- staticDirs meta =
--   meta ^.. key "static-resource-dirs" . values . _String . unpacked
excludeDirs :: Meta -> [String]
excludeDirs meta =
  let metaExclude = getMetaStringList "exclude-directories" meta
   in case metaExclude of
        Just dirs -> alwaysExclude ++ dirs
        _ -> alwaysExclude

staticDirs meta =
  let metaStatic = getMetaStringList "static-resource-dirs" meta
   in case metaStatic of
        Just dirs -> dirs
        _ -> []

scanTargets :: Meta -> ProjectDirs -> IO Targets
scanTargets meta dirs = do
  let exclude = excludeDirs meta
  srcs <- globFiles (excludeDirs meta) sourceSuffixes (dirs ^. project)
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
      , _indices = sort $ calcTargets deckSuffix indexSuffix srcs
      }
  where
    calcTargets :: String -> String -> [(String, [FilePath])] -> [FilePath]
    calcTargets srcSuffix targetSuffix sources =
      map
        (replaceSuffix srcSuffix targetSuffix .
         combine (dirs ^. public) . makeRelative (dirs ^. project))
        (fromMaybe [] $ lookup srcSuffix sources)

getDachdeckerUrl :: IO String
getDachdeckerUrl = do
  env <- System.Environment.lookupEnv "DACHDECKER_SERVER"
  let url =
        case env of
          Just val -> val
          Nothing -> "https://dach.decker.informatik.uni-wuerzburg.de"
  return url
