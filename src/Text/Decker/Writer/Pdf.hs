module Text.Decker.Writer.Pdf
  ( markdownToPdfHandout,
    markdownToPdfPage,
  )
where

import Control.Exception
import Data.ByteString.Lazy qualified as LB
import Development.Shake
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.Pandoc hiding (getTemplate)
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF
import System.Exit
import System.Process (readProcessWithExitCode)

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToPdfPage meta getTemplate markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Page Latex
  pandoc <- readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerHighlightStyle = Just pygments,
            writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc

pandocMakePdf :: WriterOptions -> FilePath -> Pandoc -> Action ()
pandocMakePdf options out pandoc =
  liftIO $ do
    result <-
      runIOQuietly (makePDF "xelatex" [] writeLaTeX options pandoc)
        >>= handleError
    case result of
      Left errMsg -> throw $ PandocException (show errMsg)
      Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout ::
  Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToPdfHandout meta getTemplate markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Handout Latex
  pandoc <- readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerHighlightStyle = Just pygments,
            writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc
