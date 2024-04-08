{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Common where

import Data.List.Extra (replace)
import Development.Shake (Action)
import Relude
import System.Directory (getCurrentDirectory)
import System.Directory.Extra (getTemporaryDirectory)
import System.FilePath
import Text.Decker.Filter.Util (hash9String)
import Text.Pandoc

type Decker = StateT DeckerState Action

doIO :: IO a -> Decker a
doIO = lift . liftIO

data DeckerState = DeckerState
  { basePath :: String,
    disposition :: Disposition,
    emptyCount :: Int
  }
  deriving (Eq, Show)

data Layout
  = Deck
  | Page
  | Handout
  | Index
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

htmlIndex = Disposition {layout = Index, format = Html}

htmlHandout = Disposition {layout = Handout, format = Html}

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia
  | SvgMedia
  | StreamMedia

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  def
    { writerExtensions =
        disableExtension Ext_implicit_figures
          $ enableExtension Ext_emoji pandocExtensions,
      writerSectionDivs = False,
      writerReferenceLocation = EndOfBlock
    }

-- | Standard Pandoc + Emoji support
pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  def
    { readerExtensions =
        disableExtension Ext_implicit_figures
          $ enableExtension Ext_emoji pandocExtensions,
      readerColumns = 999
    }

projectDir = "."

publicDir = "public"

privateDir = "private"

supportDir = "public/support"

devSupportDir = "resource/decker/support"

supportPath = "/support"

-- transientDir = ".decker"

transientDir :: IO FilePath
transientDir = do
  tmp <- getTemporaryDirectory
  cd <- getCurrentDirectory
  return $ tmp </> "decker-" <> hash9String cd <> foldr (\c s -> replace c "-" s) cd ["/", "\\", ":"]

renderedCodeDir = ".rendered-code"

liveFile = (</> "live.txt") <$> transientDir

deckerMetaFile = "decker.yaml"

targetsFile = (</> "targets.yaml") <$> transientDir

metaArgsFile = (</> "meta-args.yaml") <$> transientDir

externalStatusFile = (</> "external-programs.json") <$> transientDir

indexSource = "index.md"
