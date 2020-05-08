-- | Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
-- This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module Text.Decker.Resource.Resource
  ( writeExampleProject
  , writeTutorialProject
  , urlToFilePathIfLocal
  ) where

import Text.Decker.Project.Shake
import Text.Decker.Resource.Zip

import Control.Monad.State
import Development.Shake hiding (Resource)
import qualified Network.URI as URI
import qualified System.Directory as Dir
import System.FilePath

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
