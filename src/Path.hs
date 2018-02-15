module Path
  ( Path
  , publicURIRelativeTo
  , makePath
  ) where

import Common
import Network.URI

data Path = Path
  { absoluteFile :: FilePath
  , absolutePublicFile :: FilePath
  } deriving (Eq, Show)

publicURIRelativeTo :: FilePath -> Path -> Decker FilePath
makePath :: FilePath -> Decker Path