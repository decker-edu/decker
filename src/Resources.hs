{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Resources
  ( extractResources
  , getResourceString
  , deckerResourceDir
  , writeExampleProject
  , copyDir
  , getOldResources
  ) where

import Common
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.List.Split as Split
import Exception
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Regex.TDFA

deckerResourceDir :: IO FilePath
deckerResourceDir =
  getXdgDirectory
    XdgData
    ("decker" ++
     "-" ++ deckerVersion ++ "-" ++ deckerGitBranch ++ "-" ++ deckerGitCommitId)

getOldResources :: IO [FilePath]
getOldResources = do
  dir <- getXdgDirectory XdgData []
  files <- listDirectory dir
  return $ map (dir </>) $ filter oldVersion files
  where
    deckerRegex = "decker-([0-9]+)[.]([0-9]+)[.]([0-9]+)-" :: String
    convert = map (read :: String -> Int)
    current = convert (splitOn "." deckerVersion)
    oldVersion name =
      case getAllTextSubmatches (name =~ deckerRegex) :: [String] of
        [] -> False
        _:x:y:z:vs -> convert [x, y, z] < current

getResourceString :: FilePath -> IO String
getResourceString path = do
  dataDir <- deckerResourceDir
  readFile (dataDir </> path)

-- Extract resources from the executable into the XDG data directory.
extractResources :: IO ()
extractResources = do
  deckerExecutable <- getExecutablePath
  dataDir <- deckerResourceDir
  exists <- doesDirectoryExist dataDir
  unless exists $ do
    unlessM (Resources.unzip ["-l", deckerExecutable]) $
      throw $ ResourceException "No resource zip found in decker executable."
    createDirectoryIfMissing True dataDir
    unlessM (Resources.unzip ["-qq", "-o", "-d", dataDir, deckerExecutable]) $
      throw $
      ResourceException "Unable to extract resources from decker executable"
    putStrLn $ "# resources extracted to " ++ dataDir

unzip :: [String] -> IO Bool
unzip args = do
  (exitCode, _, _) <- readProcessWithExitCode "unzip" args ""
  return $
    case exitCode of
      ExitSuccess -> True
      ExitFailure 1 -> True
      _ -> False

-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = writeResourceFiles "example" "."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  copyDir src destDir

-- | Copy a file to a file location or to a directory
cp :: FilePath -> FilePath -> IO ()
cp src dst = do
  unlessM (doesFileExist src) $
    throw (userError "src does not exist or is not a file")
  unlessM (doesFileExist dst) $ do
    destIsDir <- doesDirectoryExist dst
    if destIsDir
      then copyFile src (dst </> takeFileName src)
      else copyFile src dst

-- | Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (doesDirectoryExist src) $
    throw (userError "src does not exist or is not a directory")
  dstExists <- doesDirectoryExist dst
  if dstExists && (last (splitPath src) /= last (splitPath dst))
    then copyDir src (dst </> last (splitPath src))
    else do
      createDirectoryIfMissing True dst
      contents <- listDirectory src
      forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else cp srcPath dstPath
