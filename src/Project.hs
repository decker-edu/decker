{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Project
  ( findFile
  , findLocalFile
  , readResource
  , provisionResource
  , provisionExistingResource
  , copyResource
  , linkResource
  , relRefResource
  , absRefResource
  , removeCommonPrefix
  , isPrefix
  , makeRelativeTo
  , findProjectDirectory
  , projectDirectories
  , resolveLocally
  , provisioningFromMeta
  , provisioningFromClasses
  , Resource(..)
  , Provisioning(..)
  , ProjectDirs(..)
  ) where

import Common
import Control.Exception
import Control.Monad
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
  = Copy -- Copy to public and relative URL
  | SymLink -- Symbolic link to public and relative URL
  | Absolute -- Absolute local URL
  | Relative -- Relative local URL
  deriving (Eq, Show, Read)

provisioningFromMeta :: Meta -> Provisioning
provisioningFromMeta meta =
  case lookupMeta "provisioning" meta of
    Just (MetaString s) -> read s
    Just (MetaInlines i) -> read $ stringify i
    _ -> SymLink

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
  whenM
    (D.doesFileExist (publicFile resource))
    (D.removeFile (publicFile resource))
  D.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  createSymbolicLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

absRefResource :: Resource -> IO FilePath
absRefResource resource =
  return $ show $ URI "file" Nothing (sourceFile resource) "" ""

relRefResource :: FilePath -> Resource -> IO FilePath
relRefResource base resource = do
  let relPath = makeRelativeTo base (sourceFile resource)
  return $ show $ URI "file" Nothing relPath "" ""

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
resolveLocally :: ProjectDirs -> FilePath -> FilePath -> IO (Maybe FilePath)
resolveLocally dirs base path = do
  absBase <- D.makeAbsolute base
  let absRoot = project dirs
  let candidates =
        if isAbsolute path
          then [absRoot </> makeRelative "/" path, path]
          else [absBase </> path, absRoot </> path]
  listToMaybe <$> filterM D.doesFileExist candidates

resourcePathes :: ProjectDirs -> FilePath -> FilePath -> Resource
resourcePathes dirs base absolute =
  Resource
  { sourceFile = absolute
  , publicFile = public dirs </> makeRelativeTo (project dirs) absolute
  , publicUrl = makeRelativeTo base absolute
  }

isLocalURI :: String -> Bool
isLocalURI url = isNothing (parseURI url)

isRemoteURI :: String -> Bool
isRemoteURI = not . isLocalURI

-- | Determines if a URL can be resolved to a local file. Absolute file URLs are
-- resolved against and copied or linked to public from 
--    1. the project root 
--    2. the local filesystem root 
-- 
-- Relative file URLs are resolved against and copied or linked to public from 
--
--    1. the directory path of the referencing file 
--    2. the project root Copy and link operations target the public directory
--       in the project root and recreate the source directory structure. This
--       function is used to provision resources that are used at presentation
--       time.
--
-- Returns a public URL relative to base
provisionResource ::
     Provisioning -> ProjectDirs -> FilePath -> FilePath -> IO FilePath
provisionResource provisioning dirs base path = do
  print ("provisionResource", dirs, base, path)
  if path == "" || isRemoteURI path
    then return path
    else findFile dirs base path >>=
         provisionExistingResource provisioning dirs base

provisionExistingResource ::
     Provisioning -> ProjectDirs -> FilePath -> FilePath -> IO FilePath
provisionExistingResource provisioning dirs base path =
  if path == "" || isRemoteURI path
    then return path
    else do
      let resource = resourcePathes dirs base path
      case provisioning of
        Copy -> copyResource resource
        SymLink -> linkResource resource
        Absolute -> absRefResource resource
        Relative -> relRefResource base resource

-- Finds local file system files that sre needed at compile time. 
-- Throws if the resource cannot be found. Used mainly for include files.
findFile :: ProjectDirs -> FilePath -> FilePath -> IO FilePath
findFile dirs base path = do
  resolved <- resolveLocally dirs base path
  case resolved of
    Nothing ->
      throw $
      ResourceException $ "Cannot find local file system resource: " ++ path
    Just resource -> return resource

-- Finds local file system files that sre needed at compile time. If
-- path is a remote URL, leave it alone.
findLocalFile :: ProjectDirs -> FilePath -> FilePath -> IO FilePath
findLocalFile dirs base path =
  if path == "" || isRemoteURI path
    then return path
    else findFile dirs base path

-- Finds local file system files that are needed at compile time. 
-- Returns the original path if the resource cannot be found.
maybeFindFile :: ProjectDirs -> FilePath -> FilePath -> IO FilePath
maybeFindFile dirs base path = do
  resolved <- resolveLocally dirs base path
  case resolved of
    Nothing -> return path
    Just resource -> return resource

-- Finds and reads a resource at compile time. If the resource can not be found in the
-- file system, the built-in resource map is searched. If that fails, an error os thrown.
-- The resource is searched for in a directory name `template`.
readResource :: ProjectDirs -> FilePath -> FilePath -> IO B.ByteString
readResource dirs base path = do
  let searchPath = "template" </> path
  resolved <- resolveLocally dirs base path
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
  when newer $ do
    D.createDirectoryIfMissing True (takeDirectory dst)
    D.copyFile src dst

fileIsNewer a b = do
  aexists <- D.doesFileExist a
  bexists <- D.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- D.getModificationTime a
             bt <- D.getModificationTime b
             return (traceShowId at > traceShowId bt)
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
