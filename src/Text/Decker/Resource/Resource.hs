{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
    deckerResources,
    deckerResource,
    readResource,
    publicSupportFiles,
    needResource,
    fileList,
    Source (..),
    Resources (..),
  )
where

import Control.Monad.Catch
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Char
import qualified Data.HashSet as Set
import qualified Data.Map as Map
import Data.Maybe
import Development.Shake hiding (Resource)
import GHC.Generics hiding (Meta)
import Network.URI
import Relude
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.FilePath.Posix
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.Glob
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

instance ToJSON Source

data Resource = Resource
  { source :: Source,
    path :: FilePath
  }
  deriving (Ord, Eq, Show, Read, Generic)

instance FromJSON Resource

instance ToJSON Resource

-- |  Resources consist of two parts:
--
--  1. Default resources distributed with decker
--  2. Resources from an optional resource pack
data Resources = Resources
  { decker :: Source,
    pack :: Source
  }
  deriving (Ord, Eq, Show, Read, Generic)

instance FromJSON Resources

-- | Determine resource locations for the current run.
deckerResources :: Meta -> IO Resources
deckerResources meta = do
  dr <- deckerResource
  pr <- packResource meta
  return (Resources dr pr)

-- |  Default decker resources are read from different locations based on the
--  type of program invocation. If started inside the decker development
--  repository with `stack run -- decker`, resources are read from the local
--  directory `resource/decker`. During normal operation resources are read from
--  the executable.
deckerResource :: IO Source
deckerResource = do
  devRun <- isDevelopmentRun
  if devRun
    then return $ LocalDir "resource/decker"
    else return $ DeckerExecutable "decker"

-- |  If the resource pack is located inside the executable during a development
--  run, use the local resource dir.
packResource :: Meta -> IO Source
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
  tryRead $ Just <$> extractEntry (epath </> path) deckerExecutable
readResource path (LocalZip zipPath) = do
  -- putStrLn $ "# read: " <> path <> " from: " <> zipPath
  tryRead $ Just <$> extractEntry path zipPath
readResource path (LocalDir baseDir) = do
  -- putStrLn $ "# read: " <> path <> " from: " <> baseDir
  tryRead $ Just <$> BS.readFile (baseDir </> path)
readResource path None = return Nothing

-- |  Don't let exceptions escape here.
tryRead :: IO (Maybe ByteString) -> IO (Maybe ByteString)
tryRead = handle $ \(SomeException e) -> do
  -- putStrLn $ "# cannot read: " <> show e
  return Nothing

-- |  Call need for a specific resource. If it is inside a container call need
-- on the container.
needResource :: Source -> FilePath -> Action ()
needResource (DeckerExecutable _) _ = do
  deckerExecutable <- liftIO getExecutablePath
  need [deckerExecutable]
needResource (LocalZip zipPath) _ = do
  need [zipPath]
needResource (LocalDir baseDir) path = do
  need [baseDir </> path]
needResource None _ = pure ()

-- | Extracts a list of all resource pathes from the given source.
fileList :: Source -> FilePath -> IO [FilePath]
fileList (DeckerExecutable path) at = subEntries (path </> at) <$> (getExecutablePath >>= listEntries)
fileList (LocalZip path) at = subEntries at <$> listEntries path
fileList (LocalDir path) at = subEntries (path </> at) <$> fastGlobFiles [] [] (path </> at)
fileList None _ = return []

subEntries :: FilePath -> [FilePath] -> [FilePath]
subEntries dir pathes =
  let dirS = splitDirectories (normalise dir)
   in map (joinPath . drop (length dirS)) $ filter (isPrefixOf dirS) $ map splitDirectories pathes

-- | Calculates a map of all support file pathes to their source location. Make
-- sure pack resources override default decker resources.
publicSupportFiles :: Meta -> IO (Map FilePath Source)
publicSupportFiles meta = resourceFiles meta "support"

resourceFiles :: Meta -> FilePath -> IO (Map FilePath Source)
resourceFiles meta below = do
  decker <- deckerResource
  pack <- packResource meta
  case (decker, pack) of
    (decker, pack) -> do
      allDeckerFiles <- fromList <$> fileList decker below
      packFiles <- fromList <$> fileList pack below
      let deckerFiles = Set.difference allDeckerFiles packFiles
      return $
        fromList $
          zip (toList deckerFiles) (repeat decker)
            <> zip (toList packFiles) (repeat pack)

-- |  Parses a URI into a Source.
parseSourceURI :: URI -> Maybe Source
parseSourceURI uri =
  let scheme = uriScheme uri
      path = normalise $ uriPath uri
      ext = map toLower $ takeExtension path
   in if
          | scheme == "exe:" -> Just $ DeckerExecutable path
          | null scheme && ext == ".zip" -> Just $ LocalZip path
          | null scheme -> Just $ LocalDir path
          | otherwise -> Just None

-- | Writes the example project to the current folder
writeExampleProject :: Meta -> IO ()
writeExampleProject meta = do
  putStrLn "# write example project"
  files <- Map.toList <$> resourceFiles meta "example"
  mapM_ extract files
  where
    extract (path, source) = do
      let out = "example" </> path
      content <- fromJust <$> liftIO (readResource out source)
      createDirectoryIfMissing True (takeDirectory out)
      liftIO $ BS.writeFile out content
      putStrLn $ "#   - " <> out
