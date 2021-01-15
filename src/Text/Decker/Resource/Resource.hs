-- | Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
-- This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module Text.Decker.Resource.Resource
  ( writeExampleProject
  , urlToFilePathIfLocal
  ) where

import Text.Decker.Internal.Helper (warnVersion)
import Text.Decker.Resource.Zip

import Development.Shake hiding (Resource)
import qualified Network.URI as URI
import System.FilePath.Posix

-- | Write the example project to the current folder
writeExampleProject :: FilePath -> IO ()
writeExampleProject dir = do
  warnVersion
  putStrLn $ "# Extracting example project to " ++ dir ++ "."
  extractResourceEntries "example" dir

urlToFilePathIfLocal :: FilePath -> FilePath -> Action FilePath
urlToFilePathIfLocal base uri =
  case URI.parseRelativeReference uri of
    Nothing -> return uri
    Just relativeUri -> do
      let filePath = URI.uriPath relativeUri
      let path =
            if hasDrive filePath
              then dropDrive filePath
              else base </> filePath
      return $ show $ relativeUri {URI.uriPath = path}
