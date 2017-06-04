module Project
  ( provisionResource
  , removeCommonPrefix
  , isPrefix
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Network.URI
import System.Directory
import System.FilePath

data Provisioning
  = Copy -- Copy to public and relative link
  | SymbolicLink -- Link to public and relative link
  | Reference -- Absolute local link

data ResolvedUrl = ResolvedUrl
  { sourceFile :: FilePath -- Absolute Path to source file
  , publicFile :: FilePath -- Absolute path to file in public folder
  , publicUrl :: FilePath -- Relative URL to served file from base
  }

-- Find the project directory.  
-- The project directory is the first upwards directory that contains a .git directory entry.
calcProjectDirectory :: IO FilePath
calcProjectDirectory = do
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

-- Resolves a file path to a concrete verified file system path, or
-- returns Nothing if no file can be found.
resolve :: FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
resolve project base path = do
  let pathes =
        if isAbsolute path
          then [project </> makeRelative "/" path, path]
          else [base </> path, project </> path]
  listToMaybe <$> filterM doesFileExist pathes

resolveUrl :: FilePath -> FilePath -> FilePath -> IO (Maybe ResolvedUrl)
resolveUrl project base url = do
  case parseURI url >>= fileOrRelativeUrl of
    Nothing -> return Nothing
    Just path -> do
      file <- resolve project base path
      case file of
        Nothing -> return Nothing
        Just file -> return $ Just $ ResolvedUrl file "" ""

fileOrRelativeUrl :: URI -> Maybe FilePath
fileOrRelativeUrl (URI "file:" Nothing path _ _) = Just path
fileOrRelativeUrl (URI "" Nothing path _ _) = Just path
fileOrRelativeUrl _ = Nothing

-- | Determines if a URL can be resolved to a local file. Absolute file URLs 
-- are resolved against and copied or linked from 
--    - the project root 
--    - the local filesystem root 
-- Relative file URLs are resolved against and copied or linked from 
--    - the directory path of the referencing file 
--    - the project root
-- Copy and link operations target the public directory in the project root
-- and recreate the source directory structure.
provisionResource ::
     Provisioning -> FilePath -> FilePath -> FilePath -> IO FilePath
provisionResource provisioning project base path = do
  let pathes =
        if isAbsolute path
          then [project </> makeRelative "/" path, path]
          else [base </> path, project </> path]
  resource <- listToMaybe <$> filterM doesFileExist pathes
  case resource of
    Nothing -> return path
    Just resolved -> do
      case provisioning of
        Copy -> return resolved
        SymbolicLink -> return resolved
        Reference -> return resolved

copyFileChanged :: FilePath -> FilePath -> IO ()
copyFileChanged src dst = do
  newer <- fileIsNewer src dst
  if newer
    then do
      createDirectoryIfMissing True (takeDirectory dst)
      copyFile src dst
    else return ()

fileIsNewer a b = do
  exists <- doesFileExist b
  if exists
    then do
      at <- getModificationTime a
      bt <- getModificationTime b
      return (at > bt)
    else return True

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
