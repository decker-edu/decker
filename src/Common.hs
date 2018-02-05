{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( DeckerException(..)
  , DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , Script(..)
  , Decker
  , needFile
  , needFiles
  , deckerVersion
  , isDevelopmentVersion
  ) where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Data.Version (showVersion, versionBranch)
import Development.Shake (Action, need)
import Network.URI as U
import Paths_decker (version)

-- | The version from the cabal file
deckerVersion :: String
deckerVersion = showVersion version

-- | Is this a developement version? Development versions have 4 branches, and
-- the 4th branch number is always 0. Release branches have only three.
isDevelopmentVersion :: Bool
isDevelopmentVersion =
  (length $ versionBranch version) == 4 && (versionBranch version) !! 3 == 0

-- | Tool specific exceptions
data DeckerException
  = MustacheException String
  | ResourceException String
  | GitException String
  | PandocException String
  | YamlException String
  | HttpException String
  | RsyncUrlException
  | DecktapeException String
  | ExternalException String
  | SassException String
  deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (ResourceException e) = e
  show (GitException e) = e
  show (HttpException e) = e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
  show (ExternalException e) = e
  show (SassException e) = e

type Decker = StateT DeckerState Action

needFile :: FilePath -> Decker ()
needFile path = lift $ need [path]

needFiles :: [FilePath] -> Decker ()
needFiles pathes = lift $ need pathes

addScript :: Script -> Decker ()
addScript script = do
  modify (\s -> s {scripts = scripts s ++ [script]})

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
              , scriptUri :: U.URI }
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
