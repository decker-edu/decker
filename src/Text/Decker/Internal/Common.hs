{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

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

transientDir :: IO FilePath
transientDir = do
  tmp <- getTemporaryDirectory
  cwd <- getCurrentDirectory
  return $ mkTmpDirName tmp cwd

mkTmpDirName tmp cwd =
  tmp </> "decker-" <> hash9String cwd <> foldr (\c s -> replace c "-" s) cwd ["/", "\\", ":"]

renderedCodeDir = ".rendered-code"

liveFile = (</> "live.txt") <$> transientDir

deckerMetaFile = "decker.yaml"

targetsFile = (</> "targets.yaml") <$> transientDir

metaArgsFile = (</> "meta-args.yaml") <$> transientDir

externalStatusFile = (</> "external-programs.json") <$> transientDir

indexSource = "index.md"
