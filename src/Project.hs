{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Project
  ( resourcePathes
  , copyResource
  , linkResource
  , relRefResource
  , absRefResource
  , removeCommonPrefix
  , isPrefix
  , makeRelativeTo
  , findProjectDirectory
  , projectDirectories
  , provisioningFromMeta
  , templateFromMeta
  , provisioningFromClasses
  , invertPath
  , fastGlob
  , scanTargets
  , glob
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
  , Targets(..)
  , Resource(..)
  , ProjectDirs(..)
  ) where

import Common
import Control.Lens
import Control.Monad.Extra
import Data.List
import Data.Maybe
import qualified Data.Yaml as Yaml
import Exception
import Network.URI
import Resources
import qualified System.Directory as D
import System.Directory
  ( createFileLink
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath
import Text.Pandoc.Definition
import Text.Pandoc.Shared

data Targets = Targets
  { _sources :: [FilePath]
  , _decks :: [FilePath]
  , _decksPdf :: [FilePath]
  , _pages :: [FilePath]
  , _pagesPdf :: [FilePath]
  , _handouts :: [FilePath]
  , _handoutsPdf :: [FilePath]
  } deriving (Show)

makeLenses ''Targets

data Resource = Resource
  { sourceFile :: FilePath -- Absolute Path to source file
  , publicFile :: FilePath -- Absolute path to file in public folder
  , publicUrl :: FilePath -- Relative URL to served file from base
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

templateFromMeta :: Meta -> Maybe String
templateFromMeta meta =
  case lookupMeta "template" meta of
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

provisioningFromClasses :: Provisioning -> [String] -> Provisioning
provisioningFromClasses defaultP cls =
  fromMaybe defaultP $
  listToMaybe $ map snd $ filter (flip elem cls . fst) provisioningClasses

copyResource :: Resource -> IO FilePath
copyResource resource = do
  copyFileIfNewer (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

linkResource :: Resource -> IO FilePath
linkResource resource = do
  whenM
    (D.doesFileExist (publicFile resource))
    (D.removeFile (publicFile resource))
  D.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  createFileLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

absRefResource :: Resource -> IO FilePath
absRefResource resource =
  return $ show $ URI "file" Nothing (sourceFile resource) "" ""

relRefResource :: FilePath -> Resource -> IO FilePath
relRefResource base resource = do
  let relPath = makeRelativeTo base (sourceFile resource)
  return $ show $ URI "file" Nothing relPath "" ""

-- Find the project directory. The project directory is the first upwards
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
  let supportDir = publicDir </> ("support" ++ "-" ++ deckerVersion)
  appDataDir <- deckerResourceDir
  let logDir = projectDir </> "log"
  return
    (ProjectDirs projectDir publicDir cacheDir supportDir appDataDir logDir)

resourcePathes :: ProjectDirs -> FilePath -> URI -> Resource
resourcePathes dirs base uri =
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

-- | Copies the src to dst if src is newer or dst does not exist. Creates
-- missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst =
  whenM (fileIsNewer src dst) $ do
    D.createDirectoryIfMissing True (takeDirectory dst)
    D.copyFile src dst

fileIsNewer :: FilePath -> FilePath -> IO Bool
fileIsNewer a b = do
  aexists <- D.doesFileExist a
  bexists <- D.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- D.getModificationTime a
             bt <- D.getModificationTime b
             return (at > bt)
           else return False
    else return aexists

-- | Express the second path argument as relative to the first. 
-- Both arguments are expected to be absolute pathes. 
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
  let (d, f) = removeCommonPrefix (dir, file)
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

-- | Glob a little more efficiently. 'exclude' contains a list of directories
-- that will be culled from the traversal. 'suffixes' is the list of file
-- suffixes that are included in the glob.
fastGlob :: [String] -> [String] -> FilePath -> IO [FilePath]
fastGlob exclude suffixes = glob
  where
    glob root = do
      dirExists <- doesDirectoryExist root
      fileExists <- doesFileExist root
      if | dirExists -> globDir root
         | fileExists -> globFile root
         | otherwise -> return []
    globFile file =
      if (not ("." `isPrefixOf` file)) && any (`isSuffixOf` file) suffixes
        then return [file]
        else return []
    globDir dir =
      if "." `isPrefixOf` dir || dir `elem` exclude
        then return []
        else concat <$> ((map (dir </>) <$> listDirectory dir) >>= mapM glob)

glob :: [String] -> [String] -> FilePath -> IO [(String, [FilePath])]
glob exclude suffixes root = do
  scanned <- fastGlob exclude suffixes root
  return $
    foldl
      (\alist suffix -> (suffix, filter (isSuffixOf suffix) scanned) : alist)
      []
      suffixes

scanTargets :: [String] -> [String] -> ProjectDirs -> IO Targets
scanTargets exclude suffixes dirs = do
  srcs <- glob exclude suffixes (dirs ^. project)
  return $
    Targets
      { _sources = concatMap snd srcs
      , _decks = calcTargets deckSuffix deckHTMLSuffix srcs
      , _decksPdf = calcTargets deckSuffix deckPDFSuffix srcs
      , _pages = calcTargets pageSuffix pageHTMLSuffix srcs
      , _pagesPdf = calcTargets pageSuffix pagePDFSuffix srcs
      , _handouts = calcTargets deckSuffix handoutHTMLSuffix srcs
      , _handoutsPdf = calcTargets deckSuffix handoutPDFSuffix srcs
      }
  where
    calcTargets :: String -> String -> [(String, [FilePath])] -> [FilePath]
    calcTargets srcSuffix targetSuffix sources =
      map
        (replaceSuffix srcSuffix targetSuffix .
         combine (dirs ^. public) . makeRelative (dirs ^. project))
        (fromMaybe [] $ lookup srcSuffix sources)
