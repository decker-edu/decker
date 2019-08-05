{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  , preextractedResourceFolder
  , chrome
  ) where

import Text.Decker.Internal.Common
import Data.List
import System.Environment
import System.FilePath

defaultProvisioning :: Provisioning
defaultProvisioning = Copy

urlPath :: FilePath -> FilePath
urlPath path = intercalate "/" (splitDirectories path)

preextractedResourceFolder :: IO FilePath
preextractedResourceFolder = do
  exep <- getExecutablePath
  return $ joinPath [(takeDirectory exep), "..", "resource"]

-- start chrome from cmd
chrome :: IO (Either String String)
chrome = return $ Right "start chrome"
