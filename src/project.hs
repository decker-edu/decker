module Project
  ( provisionResource
  , copyResource
  , linkResource
  , refResource
  , removeCommonPrefix
  , isPrefix
  , makeRelativeTo
  , findProjectDirectory
  , projectDirectories
  , resolve
  , Resource(..)
  , Provisioning(..)
  , ProjectDirs(..)
  ) where

import Common
import Control.Monad
import Control.Exception
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Debug.Trace
import Extra
import Network.URI
import System.Directory
import System.FilePath
import System.Posix.Files

data Provisioning
  = Copy -- Copy to public and relative link
  | SymbolicLink -- Link to public and relative link
  | Reference -- Absolute local link
  deriving (Eq, Show)

data Resource = Resource
  { sourceFile :: FilePath -- Absolute Path to source file
  , publicFile :: FilePath -- Absolute path to file in public folder
  , publicUrl :: FilePath -- Relative URL to served file from base
  } deriving (Eq, Show)

copyResource :: Resource -> IO (FilePath)
copyResource resource
  -- copyFileIfNewer (sourceFile resource) (publicFile resource)
 = do
  createDirectoryIfMissing True (takeDirectory (publicFile resource))
  copyFile (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

linkResource :: Resource -> IO (FilePath)
linkResource resource = do
  whenM (doesFileExist (publicFile resource)) (removeFile (publicFile resource))
  createSymbolicLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

refResource :: Resource -> IO (FilePath)
refResource resource = do
  return $ show $ URI "file" Nothing (sourceFile resource) "" ""

data ProjectDirs = ProjectDirs
  { project :: FilePath
  , public :: FilePath
  , cache :: FilePath
  , support :: FilePath
  } deriving (Eq, Show)

-- Find the project directory.  
-- The project directory is the first upwards directory that contains a .git directory entry.
findProjectDirectory :: IO FilePath
findProjectDirectory = do
  cwd <- getCurrentDirectory
  searchGitRoot cwd
  where
    searchGitRoot :: FilePath -> IO FilePath
    searchGitRoot path =
      if isDrive path
        then makeAbsolute "."
        else do
          hasGit <- doesDirectoryExist (path </> ".git")
          if hasGit
            then makeAbsolute path
            else searchGitRoot $ takeDirectory path

-- Calculate important absolute project directory pathes
projectDirectories :: IO ProjectDirs
projectDirectories = do
  projectDir <- findProjectDirectory
  let publicDir = projectDir </> "public"
  let cacheDir = publicDir </> "cache"
  let supportDir = publicDir </> "support"
  return (ProjectDirs projectDir publicDir cacheDir supportDir)

-- Resolves a file path to a concrete verified file system path, or
-- returns Nothing if no file can be found.
resolve :: ProjectDirs -> FilePath -> FilePath -> IO (Maybe Resource)
resolve dirs base path = do
  let candidates =
        if isAbsolute path
          then [(project dirs) </> makeRelative "/" path, path]
          else [base </> path, (project dirs) </> path]
  (listToMaybe . map resolveResource) <$> filterM doesFileExist candidates
  where
    resolveResource absolute =
      let relative = makeRelativeTo (project dirs) absolute
      in Resource
         { sourceFile = absolute
         , publicFile = (public dirs) </> makeRelativeTo (project dirs) absolute
         , publicUrl = makeRelativeTo base absolute
         }

resolveUrl :: ProjectDirs -> FilePath -> FilePath -> IO (Maybe Resource)
resolveUrl dirs base url = do
  case parseURI url >>= fileOrRelativeUrl of
    Nothing -> return Nothing
    Just path -> resolve dirs base path

fileOrRelativeUrl :: URI -> Maybe FilePath
fileOrRelativeUrl (URI "file:" Nothing path _ _) = Just path
fileOrRelativeUrl (URI "" Nothing path _ _) = Just path
fileOrRelativeUrl _ = Nothing

-- | Determines if a URL can be resolved to a local file. Absolute file URLs 
-- are resolved against and copied or linked from 
--    1. the project root 
--    2. the local filesystem root 
-- Relative file URLs are resolved against and copied or linked from 
--    1. the directory path of the referencing file 
--    2. the project root
-- Copy and link operations target the public directory in the project root
-- and recreate the source directory structure.
provisionResource ::
     Provisioning -> ProjectDirs -> FilePath -> FilePath -> IO FilePath
provisionResource provisioning dirs base path = do
  resolved <- resolve dirs base path
  case resolved of
    Nothing -> throw $ ResourceException $ "Cannot find local resource: " ++ path
    Just resource -> do
      case provisioning of
        Copy -> copyResource resource
        SymbolicLink -> linkResource resource
        Reference -> refResource resource

-- | Copies the src to dst if src is newer or dst does not exist. 
-- Creates missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst = do
  newer <- fileIsNewer src dst
  if newer
    then do
      createDirectoryIfMissing True (takeDirectory dst)
      copyFile src dst
    else return ()

fileIsNewer a b = do
  aexists <- doesFileExist a
  bexists <- doesFileExist b
  if bexists
    then if aexists
           then do
             at <- getModificationTime a
             bt <- getModificationTime b
             return ((traceShowId at) > (traceShowId bt))
           else return True
    else return False

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

isPrefix a b = isPrefix_ (splitPath a) (splitPath b)
  where
    isPrefix_ :: Eq a => [a] -> [a] -> Bool
    isPrefix_ al@(a:as) bl@(b:bs)
      | a == b = isPrefix_ as bs
      | otherwise = False
    isPrefix_ [] _ = True
    isPrefix_ _ _ = False

mapTuple f (a, b) = (f a, f b)
