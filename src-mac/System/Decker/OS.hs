{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  , preextractedResourceFolder
  ) where

import Common
import System.Environment
import System.FilePath

defaultProvisioning :: Provisioning
defaultProvisioning = SymLink

urlPath :: FilePath -> FilePath
urlPath path = path

preextractedResourceFolder :: IO FilePath
preextractedResourceFolder = do
  exep <- getExecutablePath
  return $ joinPath [(takeDirectory exep), "..", "Resources", "resource"]
