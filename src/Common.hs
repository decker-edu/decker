{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , Provisioning(..)
  , Script(..)
  , Decker
  , doIO
  , needFile
  , needFiles
  , deckerVersion
  , deckerGitBranch
  , isDevelopmentVersion
  , addScript
  ) where

import CompileTime
import Control.Monad.State
import Data.Version (showVersion, versionBranch)
import Development.Shake (Action, need)
import Network.URI as U
import Paths_decker (version)

-- | The version from the cabal file
deckerVersion :: String
deckerVersion = showVersion version

-- | Determine the git branch at compile time 
deckerGitBranch :: String
deckerGitBranch = $(lookupGitBranch)

-- | Is this a development or a production branch?
-- All branches are identified by three digits.
-- If the last digit is a zero, it is a production branch.
isDevelopmentVersion :: Bool
isDevelopmentVersion =
  deckerGitBranch /= "master"

type Decker = StateT DeckerState Action

doIO :: IO a -> Decker a
doIO = lift . liftIO

needFile :: FilePath -> Decker ()
needFile file = lift $ need [file]

needFiles :: [FilePath] -> Decker ()
needFiles pathes = lift $ need pathes

addScript :: Script -> Decker ()
addScript script = modify (\s -> s {scripts = scripts s ++ [script]})

data DeckerState = DeckerState
  { basePath :: String
  , disposition :: Disposition
  , provisioning :: Provisioning
  , slideCount :: Int
  , externalReferences :: [U.URI]
  , scripts :: [Script]
  } deriving (Eq, Show)

data Script
  = ScriptURI { scriptLang :: String
              , scriptUri :: String }
  | ScriptSource { scriptLang :: String
                 , scriptSource :: String }
  deriving (Eq, Show, Ord)

data Layout
  = Deck
  | Page
  | Handout
  deriving (Eq, Show)

data OutputFormat
  = Html
  | Pdf
  deriving (Eq, Show)

data Disposition = Disposition
  { layout :: Layout
  , format :: OutputFormat
  } deriving (Eq, Show)

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia

data Provisioning
  = Copy -- Copy to public and relative URL
  | SymLink -- Symbolic link to public and relative URL
  | Absolute -- Absolute local URL
  | Relative -- Relative local URL
  deriving (Eq, Show, Read)
