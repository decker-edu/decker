{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Internal.Common
  ( DeckerState(..)
  , Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , MediaType(..)
  , Provisioning(..)
  , Decker
  , doIO
  , needFile
  , needFiles
  , pandocReaderOpts
  , pandocWriterOpts
  , deckerFiles
  ) where

import Control.Monad.State
import Development.Shake (Action, need)
import Text.Pandoc

-- | Decker temporary build files are stored here.
deckerFiles = ".decker"

type Decker = StateT DeckerState Action

doIO :: IO a -> Decker a
doIO = lift . liftIO

needFile :: FilePath -> Decker ()
needFile file = lift $ need [file]

needFiles :: [FilePath] -> Decker ()
needFiles pathes = lift $ need pathes

data DeckerState = DeckerState
  { basePath :: String
  , disposition :: Disposition
  , provisioning :: Provisioning
  } deriving (Eq, Show)

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
  { layout :: Layout
  , format :: OutputFormat
  } deriving (Ord, Eq, Show)

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia
  | SvgMedia
  | StreamMedia

data Provisioning
  = Copy -- ^ Copy to public and relative URL
  | SymLink -- ^ Symbolic link to public and relative URL
  | Absolute -- ^ Absolute local URL
  | Relative -- ^ Relative local URL
  deriving (Eq, Show, Read)

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  def {readerExtensions = (enableExtension Ext_emoji) pandocExtensions}

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  def
    { writerExtensions =
        (enableExtension Ext_auto_identifiers .
         disableExtension Ext_simple_tables .
         disableExtension Ext_multiline_tables . enableExtension Ext_emoji)
          pandocExtensions
    }
