{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  ) where

import Common

defaultProvisioning :: Provisioning
defaultProvisioning = SymLink

urlPath :: FilePath -> FilePath
urlPath path = path
