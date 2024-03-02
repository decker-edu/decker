{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Internal.Common where

import Development.Shake (Action)
import Relude
import System.FilePath
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

-- | Standard Pandoc + Emoji support
commonmarkReaderOpts :: ReaderOptions
commonmarkReaderOpts =
  def
    { readerExtensions =
        disableExtension Ext_implicit_figures
          $ enableExtension Ext_yaml_metadata_block
          -- \$ enableExtension Ext_tex_math_gfm
          $ enableExtension Ext_sourcepos
          $ enableExtension Ext_hard_line_breaks
          $ enableExtension Ext_smart
          $ enableExtension Ext_strikeout
          $ enableExtension Ext_superscript
          $ enableExtension Ext_subscript
          $ enableExtension Ext_tex_math_dollars
          $ enableExtension Ext_fancy_lists
          $ enableExtension Ext_fenced_divs
          $ enableExtension Ext_bracketed_spans
          $ enableExtension Ext_raw_attribute
          $ enableExtension Ext_attributes
          -- \$ enableExtension Ext_alerts
          $ enableExtension Ext_pipe_tables
          $ enableExtension Ext_autolink_bare_uris
          $ enableExtension Ext_emoji
          $ enableExtension Ext_gfm_auto_identifiers
          $ enableExtension Ext_ascii_identifiers
          $ enableExtension Ext_gfm_auto_identifiers
          $ enableExtension Ext_ascii_identifiers
          $ enableExtension Ext_implicit_header_references
          $ enableExtension Ext_footnotes
          $ enableExtension Ext_definition_lists
          $ enableExtension Ext_task_lists
          $ enableExtension Ext_wikilinks_title_after_pipe
          $ enableExtension Ext_wikilinks_title_before_pipe
          $ enableExtension Ext_rebase_relative_paths
          $ enableExtension Ext_sourcepos
          $ enableExtension Ext_emoji pandocExtensions,
      readerColumns = 999
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

indexSource = "index.md"
