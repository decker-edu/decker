-- | Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
-- This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module Text.Decker.Resource.Resource
  ( deckerResourceDir
  , writeExampleProject
  , writeTutorialProject
  , copyResource
  , linkResource
  , urlToFilePathIfLocal
  ) where

import Text.Decker.Internal.Helper
import Text.Decker.Internal.URI
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Resource.Zip

import Control.Monad.Extra
import Control.Monad.State
import qualified Data.Text as Text
import Development.Shake hiding (Resource)
import qualified Network.URI as URI
import qualified System.Directory as Dir
import System.FilePath
import Text.Pandoc.Shared

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes :: [Text.Text]
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

runtimeMetaKeys :: [Text.Text]
runtimeMetaKeys = ["css", "base-css"]

compiletimeMetaKeys :: [Text.Text]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [Text.Text]
metaKeys = runtimeMetaKeys <> compiletimeMetaKeys

-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = do
  cwd <- Dir.getCurrentDirectory
  putStrLn $ "Extracting example project to " ++ cwd ++ "."
  extractResourceEntries "example" cwd

-- | Write the tutorial project to the current folder
writeTutorialProject :: IO ()
writeTutorialProject = do
  cwd <- Dir.getCurrentDirectory
  putStrLn $ "Extracting tutorial project to " ++ cwd ++ "."
  extractResourceEntries "tutorial" cwd

-- | Copies single Resource file and returns Filepath
copyResource :: Resource -> IO FilePath
copyResource resource = do
  copyFileIfNewer (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

-- | Creates SymLink to single resource file and returns Filepath
linkResource :: Resource -> IO FilePath
linkResource resource = do
  whenM
    (Dir.doesFileExist (publicFile resource))
    (Dir.removeFile (publicFile resource))
  Dir.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  Dir.createFileLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

needMetaResource :: FilePath -> (Text.Text, FilePath) -> Action FilePath
needMetaResource base (key, url)
  | key `elem` compiletimeMetaKeys = do
    project <- projectA
    absolutePath <- liftIO $ absolutePathIfLocal project base (Text.pack url)
    case absolutePath of
      Just path -> need [toString path] >> return (toString path)
      Nothing -> return url
needMetaResource base (key, url) = do
  project <- projectA
  public <- publicA
  absolutePath <- liftIO $ absolutePathIfLocal project base (Text.pack url)
  case absolutePath of
    Just path ->
      need [public </> makeRelative project (toString path)] >>
      return (toString path)
    Nothing -> return url

urlToFilePathIfLocal :: FilePath -> FilePath -> Action FilePath
urlToFilePathIfLocal base uri =
  case URI.parseRelativeReference uri of
    Nothing -> return uri
    Just relativeUri -> do
      let filePath = URI.uriPath relativeUri
      absBase <- liftIO $ Dir.makeAbsolute base
      absRoot <- projectA
      let absPath =
            if isAbsolute filePath
              then absRoot </> makeRelative "/" filePath
              else absBase </> filePath
      return $ show $ relativeUri {URI.uriPath = absPath}
