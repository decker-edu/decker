{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>

-- | This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
module Text.Decker.Resource.Resource
  ( writeExampleProject,
    writeSupportFilesToPublic,
    deckerResources,
    readResource,
    Source (..),
    Resources (..),
  )
where

import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Char
import Development.Shake hiding (Resource)
import GHC.Generics hiding (Meta)
import Network.URI
import Relude
import System.Directory
import System.Environment
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Version
import Text.Decker.Resource.Zip
import Text.Pandoc hiding (lookupMeta)

-- | Defines the interface to resource sources that can be selected at runtime.
data Source
  = DeckerExecutable FilePath
  | LocalDir FilePath
  | LocalZip FilePath
  | None
  deriving (Ord, Eq, Show, Read, Generic)

instance FromJSON Source

data Resources = Resources
  { decker :: Source,
    pack :: Source
  }
  deriving (Ord, Eq, Show, Read, Generic)

instance FromJSON Resources

deckerResources :: Meta -> IO Resources
deckerResources meta = do
  dr <- deckerResource
  pr <- packResource meta
  return (Resources dr pr)

deckerResource = do
  devRun <- isDevelopmentRun
  if devRun
    then return $ LocalDir "resource/decker"
    else return $ DeckerExecutable "decker"

packResource meta = do
  devRun <- isDevelopmentRun
  let source =
        lookupMeta "resource-pack" meta
          >>= parseURIReference
          >>= parseSourceURI
  case source of
    Just (DeckerExecutable path)
      | devRun ->
        return $ LocalDir $ "resource" </> path
    Just source -> return source
    Nothing -> return None

readResource :: FilePath -> Source -> IO (Maybe ByteString)
readResource path (DeckerExecutable epath) = do
  deckerExecutable <- getExecutablePath
  -- putStrLn $ "# read: " <> path <> " from: " <> (deckerExecutable <> ":" <> epath)
  tryRead $ Just <$> extractEntry (epath <> path) deckerExecutable
readResource path (LocalZip zipPath) = do
  -- putStrLn $ "# read: " <> path <> " from: " <> zipPath
  tryRead $ Just <$> extractEntry path zipPath
readResource path (LocalDir baseDir) = do
  -- putStrLn $ "# read: " <> path <> " from: " <> baseDir
  tryRead $ Just <$> BS.readFile (baseDir </> path)
readResource path None = return Nothing

tryRead :: IO (Maybe ByteString) -> IO (Maybe ByteString)
tryRead = handle $ \(SomeException e) -> do
  -- putStrLn $ "# cannot read: " <> show e
  return Nothing

copySupportFiles :: Source -> FilePath -> IO ()
copySupportFiles (DeckerExecutable path) destination = do
  deckerExecutable <- getExecutablePath
  putStrLn $ "# copy from: " <> (deckerExecutable <> ":" <> (path </> "support")) <> " to: " <> destination
  extractSubEntries (path </> "support") deckerExecutable destination
copySupportFiles (LocalZip zipPath) destination = do
  putStrLn $ "# copy from: " <> (zipPath </> "support") <> " to: " <> destination
  extractSubEntries "support" zipPath (takeDirectory destination)
copySupportFiles (LocalDir baseDir) destination = do
  putStrLn $ "# copy from: " <> (baseDir </> "support") <> " to: " <> destination
  copyDir (baseDir </> "support") destination
copySupportFiles None destination = return ()

copyAllSupportFiles :: Resources -> FilePath -> IO ()
copyAllSupportFiles (Resources decker pack) destination = do
  putStrLn $ "# creating: " <> destination
  createDirectoryIfMissing True destination
  copySupportFiles decker destination
  copySupportFiles pack destination

writeSupportFilesToPublic :: Meta -> IO ()
writeSupportFilesToPublic meta = do
  resources <- liftIO $ deckerResources meta
  putStrLn $ "# resources: " <> show resources
  correct <- correctSupportInstalled resources
  if correct
    then putStrLn "# support files up to date"
    else do
      putStrLn $ "# copy support files from: " <> show resources
      removeSupport
      extractSupport resources

supportId resources = show (resources, deckerGitCommitId)

extractSupport :: Resources -> IO ()
extractSupport resources = do
  -- handleAll (\_ -> return ()) $ do
  copyAllSupportFiles resources supportDir
  writeFile (supportDir </> ".origin") $ supportId resources

correctSupportInstalled :: Resources -> IO Bool
correctSupportInstalled templateSource = do
  handleAll (\_ -> return False) $ do
    installed <- readFile (supportDir </> ".origin")
    return (installed == supportId templateSource)

removeSupport :: IO ()
removeSupport = do
  handleAll (\_ -> return ()) $ removeDirectoryRecursive supportDir

{--
fileList :: Source -> FilePath -> IO [FilePath]
fileList (DeckerExecutable path) at = subEntries (path </> at) <$> (getExecutablePath >>= listEntries)
fileList (LocalZip path) at = subEntries at <$> listEntries path
fileList (LocalDir path) at = subEntries (path </> at) <$> fastGlobFiles [] [] (path </> at)
fileList None _ = return []

subEntries :: FilePath -> [FilePath] -> [FilePath]
subEntries dir pathes =
  let dirS = splitDirectories dir
   in map joinPath $ map (drop (length dirS)) $ filter (isPrefixOf dirS) $ map splitDirectories pathes

copyFile :: Source -> FilePath -> FilePath -> IO ()
copyFile (DeckerExecutable path) list destination = return ()
copyFile (LocalZip path) list destination = return ()
copyFile (LocalDir path) list destination = return ()
copyFile None list destination = return ()

needFiles :: Source -> [FilePath] -> FilePath -> Action ()
needFiles (DeckerExecutable path) list destination = return ()
needFiles (LocalZip path) list destination = return ()
needFiles (LocalDir path) list destination = return ()
needFiles None list destination = return ()

publicSupportFiles :: Meta -> IO [FilePath]
publicSupportFiles meta = do
  decker <- deckerResource
  pack <- packResource meta
  case (decker, pack) of
    (decker, pack) -> do
      deckerFiles <- fromList <$> fileList decker "support"
      packFiles <- fromList <$> fileList pack "support"
      -- print deckerFiles
      -- print packFiles
      return $ toList $ Set.union deckerFiles packFiles

--}

parseSourceURI :: URI -> Maybe Source
parseSourceURI uri =
  let scheme = uriScheme uri
      path = uriPath uri
      ext = map toLower $ takeExtension path
   in if
          | scheme == "exe:" -> Just $ DeckerExecutable path
          | null scheme && ext == ".zip" -> Just $ LocalZip path
          | null scheme -> Just $ LocalDir path
          | otherwise -> Just None

-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = do
  dir <- getCurrentDirectory
  warnVersion
  putStrLn $ "# Extracting example project to " ++ dir ++ "."
  extractResourceEntries "example" dir
