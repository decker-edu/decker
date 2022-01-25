{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Internal.Common where

import Control.Monad.State
import Development.Shake (Action)
import System.FilePath
import Text.Pandoc

type Decker = StateT DeckerState Action

doIO :: IO a -> Decker a
doIO = lift . liftIO

data DeckerState = DeckerState
  { basePath :: String,
    disposition :: Disposition,
    provisioning :: Provisioning,
    emptyCount :: Int
  }
  deriving (Eq, Show)

data Layout
  = Deck
  | Page
  | Handout
  | Notebook
  deriving (Ord, Eq, Show)

data OutputFormat
  = Reveal
  | Html
  | Latex
  | Markdown
  deriving (Ord, Eq, Show)

data Disposition = Disposition
  { layout :: Layout,
    format :: OutputFormat
  }
  deriving (Ord, Eq, Show)

htmlDeck = Disposition {layout = Deck, format = Html}

htmlPage = Disposition {layout = Page, format = Html}

htmlHandout = Disposition {layout = Handout, format = Html}

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia
  | SvgMedia
  | StreamMedia

data Provisioning
  = -- | Copy to public and relative URL
    Copy
  | -- | Symbolic link to public and relative URL
    SymLink
  | -- | Absolute local URL
    Absolute
  | -- | Relative local URL
    Relative
  deriving (Eq, Show, Read)

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  def
    { writerExtensions =
        disableExtension Ext_smart $
          enableExtension Ext_emoji pandocExtensions,
      writerSectionDivs = False
    }

-- | Standard Pandoc + Emoji support
pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  def
    { readerExtensions =
        disableExtension Ext_smart $
          enableExtension Ext_emoji pandocExtensions
    }

projectDir = "."

publicDir = "public"

privateDir = "private"

supportDir = "public/support"

devSupportDir = "resource/decker/support"

supportPath = "/support"

transientDir = ".decker"

liveFile = transientDir </> "live.txt"

deckerMetaFile = "decker.yaml"

targetsFile = transientDir </> "targets.yaml"

metaArgsFile = transientDir </> "meta-args.yaml"

externalStatusFile = transientDir </> "external-programs.json"