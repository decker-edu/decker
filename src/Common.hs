{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Common
  ( addScript
  -- * Types
  , DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , Provisioning(..)
  , Script(..)
  , Decker
  -- *
  , doIO
  , needFile
  , needFiles
  -- * Dealing with file suffixes 
  , dropSuffix
  , replaceSuffix
  , repeatIfTrue
  , whenTrue
  -- *
  , unique
  , time
  , elementAttributes
  -- * keys
  , runtimeMetaKeys
  , compiletimeMetaKeys
  , templateOverrideMetaKeys
  , metaKeys
  ) where

import Text.Decker.Internal.CompileTime
import Control.Monad.State
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Development.Shake (Action, need)
import Network.URI as U
import System.CPUTime
import Text.Printf
import Text.Read (readMaybe)
import Text.Regex.TDFA

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
  = Reveal
  | Html
  | Latex
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
  = Copy -- ^ Copy to public and relative URL
  | SymLink -- ^ Symbolic link to public and relative URL
  | Absolute -- ^ Absolute local URL
  | Relative -- ^ Relative local URL
  deriving (Eq, Show, Read)

repeatIfTrue :: Monad m => m Bool -> m ()
repeatIfTrue action = do
  again <- action
  when again $ repeatIfTrue action

whenTrue :: Monad m => m Bool -> m () -> m ()
whenTrue bool action = do
  true <- bool
  when true action

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

time :: String -> IO a -> IO a
time name action = do
  start <- getCPUTime
  result <- action
  stop <- getCPUTime
  let diff = fromIntegral (stop - start) / (10 ^ 12)
  printf "%s: %0.5f sec\n" name (diff :: Double)
  return result

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes :: [String]
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

-- | Resources in meta data that are needed at compile time. They have to be
-- specified as local URLs and must exist.
runtimeMetaKeys :: [String]
runtimeMetaKeys = ["css"]

templateOverrideMetaKeys :: [String]
templateOverrideMetaKeys = ["template"]

compiletimeMetaKeys :: [String]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [String]
metaKeys = runtimeMetaKeys ++ compiletimeMetaKeys ++ templateOverrideMetaKeys
