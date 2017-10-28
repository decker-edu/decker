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
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

deckerResourceDir :: IO FilePath
deckerResourceDir = getXdgDirectory XdgData ("decker" ++ "-" ++ deckerVersion)

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
    createDirectoryIfMissing True dataDir
    (exitCode, _, _) <-
      readProcessWithExitCode
        "unzip"
        ["-qq", "-d", dataDir, deckerExecutable]
        ""
    case exitCode of
      ExitSuccess -> putStrLn $ "# resources extracted to " ++ dataDir
      ExitFailure 1 -> putStrLn $ "# resources extracted to " ++ dataDir
      _ ->
        throw $ ResourceException "No resource zip found in decker executable."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  exists <- doesDirectoryExist (destDir </> prefix)
  unless exists $ callProcess "cp" ["-R", src, destDir]
