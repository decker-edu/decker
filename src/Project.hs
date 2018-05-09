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
  , provisioningFromClasses
  , invertPath
  , Resource(..)
  , ProjectDirs(..)
  ) where

import Common
import Control.Monad.Extra
import Data.Maybe
import Network.URI
import Resources
import qualified System.Directory as D
import System.FilePath
import System.Directory (createFileLink)
import Text.Pandoc.Definition
import Text.Pandoc.Shared

provisioningFromMeta :: Meta -> Provisioning
provisioningFromMeta meta =
  case lookupMeta "provisioning" meta of
    Just (MetaString s) -> read s
    Just (MetaInlines i) -> read $ stringify i
    _ -> SymLink

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

data Resource = Resource
  { sourceFile :: FilePath -- Absolute Path to source file
  , publicFile :: FilePath -- Absolute path to file in public folder
  , publicUrl :: FilePath -- Relative URL to served file from base
  } deriving (Eq, Show)

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

data ProjectDirs = ProjectDirs
  { project :: FilePath
  , public :: FilePath
  , cache :: FilePath
  , support :: FilePath
  , appData :: FilePath
  , log :: FilePath
  } deriving (Eq, Show)

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
  , publicFile = public dirs </> makeRelativeTo (project dirs) (uriPath uri)
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
