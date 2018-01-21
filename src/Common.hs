{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( DeckerException(..)
  , DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , Decker
  , needFile
  , needFiles
  , deckerVersion
  ) where

import Control.Exception
import Control.Monad.State
import Data.Typeable
import Data.Version (showVersion)
import Development.Shake (Action, need)
import Network.URI as U
import Paths_decker (version)

-- | The version from the cabal file
deckerVersion :: String
deckerVersion = showVersion version

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
  deriving (Typeable)

instance Exception DeckerException

instance Show DeckerException where
  show (MustacheException e) = e
  show (ResourceException e) = e
  show (GitException e) = e
  show (HttpException e) = e
  show (PandocException e) = e
  show (YamlException e) = e
  show (DecktapeException e) = "decktape.sh failed for reason: " ++ e
  show RsyncUrlException =
    "attributes 'destinationRsyncHost' or 'destinationRsyncPath' not defined in meta data"

type Decker = StateT DeckerState Action

needFile :: FilePath -> Decker ()
needFile path = lift $ need [path]

needFiles :: [FilePath] -> Decker ()
needFiles pathes = lift $ need pathes

data DeckerState = DeckerState
  { disposition :: Disposition
  , slideCount :: Int
  , externalReferences :: [U.URI]
  , scripts :: [Script]
  } deriving (Eq, Show)

data Script
  = ScriptURI { scriptLang :: String
              , scriptUri :: U.URI }
  | ScriptSource { scriptLang :: String
                 , scriptSource :: String }
  deriving (Eq, Show)

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
