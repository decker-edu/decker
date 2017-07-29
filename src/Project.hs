{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Project
  ( findFile
  , readResource
  , provisionResource
  , copyResource
  , linkResource
  , refResource
  , removeCommonPrefix
  , isPrefix
  , makeRelativeTo
  , findProjectDirectory
  , projectDirectories
  , resolveLocally
  , provisioningFromMeta
  , Resource(..)
  , Provisioning(..)
  , ProjectDirs(..)
  ) where

import Common
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import Debug.Trace
import Embed
import Extra
import Network.URI
import qualified System.Directory as D
import System.FilePath
import System.Posix.Files
import Text.Pandoc.Definition
import Text.Pandoc.Shared

data Provisioning
  = Copy -- Copy to public and relative path
  | SymbolicLink -- Symbolic link to public and relative path
  | Reference -- Absolute local path
  deriving (Eq, Show, Read)

provisioningFromMeta :: Meta -> Provisioning
provisioningFromMeta meta =
  case lookupMeta "provisioning" meta of
    Just (MetaString s) -> read s
    Just (MetaInlines i) -> read $ stringify i
    otherwise -> Copy

data Resource = Resource
  { sourceFile :: FilePath -- Absolute Path to source file
  , publicFile :: FilePath -- Absolute path to file in public folder
  , publicUrl :: FilePath -- Relative URL to served file from base
  } deriving (Eq, Show)

copyResource :: Resource -> IO FilePath
copyResource resource
  -- TODO: Not working
  -- copyFileIfNewer (sourceFile resource) (publicFile resource)
 = do
  D.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  D.copyFile (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

linkResource :: Resource -> IO FilePath
linkResource resource = do
  whenM (D.doesFileExist (publicFile resource)) (D.removeFile (publicFile resource))
  createSymbolicLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

refResource :: Resource -> IO FilePath
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
  cwd <- D.getCurrentDirectory
  searchGitRoot cwd
  where
    searchGitRoot :: FilePath -> IO FilePath
    searchGitRoot path =
      if isDrive path
        then D.makeAbsolute "."
        else do
          hasGit <- D.doesDirectoryExist (path </> ".git")
          if hasGit
            then D.makeAbsolute path
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
resolveLocally :: FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
resolveLocally root base path = do
  absRoot <- D.makeAbsolute root
  absBase <- D.makeAbsolute base
  let candidates =
        if isAbsolute path
          then [absRoot </> makeRelative "/" path, path]
          else [absBase </> path, absRoot </> path]
  listToMaybe <$> filterM D.doesFileExist candidates

resourcePathes :: ProjectDirs -> FilePath -> FilePath -> Resource
resourcePathes dirs base absolute =
  Resource
  { sourceFile = absolute
  , publicFile = (public dirs) </> makeRelativeTo (project dirs) absolute
  , publicUrl = makeRelativeTo base absolute
  }

-- resolveUrl :: ProjectDirs -> FilePath -> FilePath -> IO (Maybe Resource)
-- resolveUrl dirs base url = do
--   case parseURI url >>= fileOrRelativeUrl of
--     Nothing -> return Nothing
--     Just path -> resourcePathes dirs base <$> resolve dirs base path
fileOrRelativeUrl :: URI -> Maybe FilePath
fileOrRelativeUrl (URI "file:" Nothing path _ _) = Just path
fileOrRelativeUrl (URI "" Nothing path _ _) = Just path
fileOrRelativeUrl _ = Nothing

-- | Determines if a URL can be resolved to a local file. Absolute file URLs 
-- are resolved against and copied or linked to public from 
--    1. the project root 
--    2. the local filesystem root 
-- Relative file URLs are resolved against and copied or linked to public from 
--    1. the directory path of the referencing file 
--    2. the project root
-- Copy and link operations target the public directory in the project root
-- and recreate the source directory structure.
-- This function is used to provision resources that are used at presentation time.
provisionResource ::
     Provisioning -> ProjectDirs -> FilePath -> FilePath -> IO FilePath
provisionResource provisioning dirs base path = do
  resource <- resourcePathes dirs base <$> findFile (project dirs) base path
  case provisioning of
    Copy -> copyResource resource
    SymbolicLink -> linkResource resource
    Reference -> refResource resource

-- Finds local file system files that sre needed at compile time. 
-- Throws if the resource cannot be found. Use mainly for include files.
findFile :: FilePath -> FilePath -> FilePath -> IO FilePath
findFile root base path = do
  resolved <- resolveLocally root base path
  case resolved of
    Nothing ->
      throw $ ResourceException $ "Cannot find local file system resource: " ++ path
    Just resource -> return resource

-- Finds and reads a resource at compile time. If the resource can not be found in the
-- file system, the built-in resource map is searched. If that fails, an error os thrown.
-- The resource is searched for in a directory name `template`.
readResource :: FilePath -> FilePath -> FilePath -> IO B.ByteString
readResource root base path = do
  let searchPath = "template" </> path
  resolved <- resolveLocally root base path
  case resolved of
    Just resource -> B.readFile resource
    Nothing ->
      case find (\(k, b) -> k == path) deckerTemplateDir of
        Nothing ->
          throw $ ResourceException $ "Cannot find built-in resource: " ++ path
        Just entry -> return $ snd entry

-- | Copies the src to dst if src is newer or dst does not exist. 
-- Creates missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst = do
  newer <- fileIsNewer src dst
  if newer
    then do
      D.createDirectoryIfMissing True (takeDirectory dst)
      D.copyFile src dst
    else return ()

fileIsNewer a b = do
  aexists <- D.doesFileExist a
  bexists <- D.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- D.getModificationTime a
             bt <- D.getModificationTime b
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
