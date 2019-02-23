module Format
  ( formatMarkdown
  ) where

import Control.Exception
import Control.Monad
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.Pandoc
import Text.Pandoc.Shared

import Markdown
import Resources
import Utilities
import Meta

formatMarkdown :: IO ()
formatMarkdown =
  handle (\(SomeException e) -> hPutStr stderr (show e) >> exitFailure) $ do
    result <- T.hGetContents stdin >>= runIO . readMarkdown pandocReaderOpts
    case result of
      Right pandoc@(Pandoc meta _) -> do
        template <- getResourceString $ "template" </> "deck.md"
        let extensions =
              (disableExtension Ext_simple_tables .
               disableExtension Ext_multiline_tables .
               enableExtension Ext_auto_identifiers)
                pandocExtensions
        let columns = lookupInt "format.line-columns" 80 meta
        let wrapOpt "none" = WrapNone
            wrapOpt "preserve" = WrapPreserve
            wrapOpt _ = WrapAuto
        let wrap = lookupString "format.line-wrap" "auto" meta
        let options =
              def
                { writerTemplate = Just template
                , writerExtensions = extensions
                , writerColumns = columns
                , writerWrapText = wrapOpt wrap
                , writerSetextHeaders = False
                }
        result <- runIO (Markdown.writeMarkdown options pandoc)
        case result of
          Right markdown -> T.hPutStr stdout markdown
          Left errMsg -> throw $ PandocException (show errMsg)
      Left errMsg -> throw $ PandocException (show errMsg)
