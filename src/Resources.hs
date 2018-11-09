{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Resources
  ( extractResources
  , getResourceString
  , deckerResourceDir
  , writeResourceFiles
  ) where

import Common
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.Text as T
import Exception
import Shelly as Sh (cp_r, fromText, shelly)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

deckerResourceDir :: IO FilePath
deckerResourceDir =
  getXdgDirectory
    XdgData
    ("decker" ++ "-" ++ deckerVersion ++ "-" ++ deckerGitBranch)

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

-- Using the Shelly Package to get native Haskell shell commands (cp_r)
-- hackage.haskell.org/package/shelly-1.8.1/docs/Shelly.html
writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  exists <- doesDirectoryExist (destDir </> prefix)
  unless exists $
    Sh.shelly $
    Sh.cp_r (Sh.fromText $ T.pack src) (Sh.fromText $ T.pack destDir)
