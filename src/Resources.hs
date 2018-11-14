{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Resources
  ( extractResources
  , getResourceString
  , deckerResourceDir
  , writeExampleProject
  , writeResourceFiles
  , cp_r
  ) where

import Common
import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Exception
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

writeExampleProject :: IO ()
writeExampleProject =
  getCurrentDirectory >>= \x -> writeResourceFiles "example" x

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  exists <- doesDirectoryExist (destDir </> prefix)
  unless exists $ cp_r src destDir
  -- Using the Shelly Package to get native Haskell shell commands (cp_r)
  -- hackage.haskell.org/package/shelly-1.8.1/docs/Shelly.html
    -- Sh.shelly $
    -- Sh.cp_r (Sh.fromText $ T.pack src) (Sh.fromText $ T.pack destDir)
    -- unless exists $ callProcess "cp" ["-R", src, destDir]

-- Problem: bei writeResourceFiles wird nur "." als dest angegeben
-- es soll dann der example ordner neu erstellt werden im aktuellen ordner
-- copyDir will den dest ordner IMMER neu erstellen und zwar nur dann wenn er noch nciht existiert
-- wie cp machen: if ordner existiert: schreibe den src ordner IN den existierenden
-- else: erstelle neuen Ordner mit namen dst
cp_r :: FilePath -> FilePath -> IO ()
cp_r src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  doesDirectoryExist dst >>= \b ->
    if b
      then copyDir src (dst </> takeBaseName src)
      else copyDir src dst
  -- whenM (doesDirectoryExist dst) $ copyDir src (dst </> takeBaseName src)
  putStrLn src
  putStrLn dst
  putStrLn (takeBaseName src)
  where
    whenM s r = s >>= flip when r

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (doesDirectoryExist dst) $
    throw (userError "destination already exists")
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath
  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r
