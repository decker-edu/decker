{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  ) where

import Common

defaultProvisioning :: Provisioning
defaultProvisioning = Copy

urlPath :: FilePath -> FilePath
urlPath path = intercalate "/" (splitDirectories path)
