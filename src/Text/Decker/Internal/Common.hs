{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Internal.Common
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
  , pandocReaderOpts
  , pandocWriterOpts
  , templateFileName
  ) where

import Control.Exception
import Control.Monad.State
import Development.Shake (Action, need)
import Network.URI as U
import Text.Decker.Internal.Exception
import Text.Pandoc

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

templateFileName :: Disposition -> String
templateFileName (Disposition Deck Html) = "template/deck.html"
templateFileName (Disposition Deck Latex) = "template/deck.tex"
templateFileName (Disposition Page Html) = "template/page.html"
templateFileName (Disposition Page Latex) = "template/page.tex"
templateFileName (Disposition Handout Html) = "template/handout.html"
templateFileName (Disposition Handout Latex) = "template/handout.tex"
templateFileName (Disposition Notebook Html) =
  throw $ InternalException "No template for Notebook"
templateFileName (Disposition Notebook Latex) =
  throw $ InternalException "No template for Notebook"

data MediaType
  = ImageMedia
  | AudioMedia
  | VideoMedia
  | IframeMedia
  | MeshMedia
  | SvgMedia
  | StreamMedia

data Provisioning
  = Copy -- ^ Copy to public and relative URL
  | SymLink -- ^ Symbolic link to public and relative URL
  | Absolute -- ^ Absolute local URL
  | Relative -- ^ Relative local URL
  deriving (Eq, Show, Read)

-- Remove automatic identifier creation for headers. It does not work well with
-- the current include mechanism if slides have duplicate titles in separate
-- include files. Switched back on for now.
deckerPandocExtensions :: Extensions
deckerPandocExtensions =
  (enableExtension Ext_auto_identifiers .
   disableExtension Ext_simple_tables .
   disableExtension Ext_multiline_tables . enableExtension Ext_emoji)
    pandocExtensions

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


