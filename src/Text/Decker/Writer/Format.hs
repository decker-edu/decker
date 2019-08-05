module Text.Decker.Writer.Format
  ( formatMarkdown
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Writer.Markdown

import Control.Exception
import qualified Data.Text.IO as T
import System.Exit
import System.FilePath
import System.IO
import Text.Pandoc hiding (writeMarkdown)

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
        result <- runIO (writeMarkdown options pandoc)
        case result of
          Right markdown -> T.hPutStr stdout markdown
          Left errMsg -> throw $ PandocException (show errMsg)
      Left errMsg -> throw $ PandocException (show errMsg)
