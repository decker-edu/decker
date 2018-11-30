{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  ) where

import Common

defaultProvisioning :: Provisioning
defaultProvisioning = SymLink
