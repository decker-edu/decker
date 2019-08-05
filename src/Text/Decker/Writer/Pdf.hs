module Text.Decker.Writer.Pdf
  ( launchChrome
  , markdownToPdfHandout
  , markdownToPdfPage
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template

import Control.Exception
import qualified Data.ByteString.Lazy as LB
import Development.Shake
import System.Decker.OS
import System.Exit
import System.Process
import Text.Pandoc
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF

chromeOptions :: FilePath -> FilePath -> [String]
chromeOptions src out =
  [ "--headless"
  , "--virtual-time-budget=5000"
  , "--disable-gpu"
  , pdfOption out
  , modifySrc src
  ]
  where
    modifySrc path = path ++ "?print-pdf#/"
    pdfOption path = "--print-to-pdf=" ++ path

launchChrome :: FilePath -> FilePath -> IO (Either String String)
launchChrome src out = do
  command <- chrome
  let options = unwords (chromeOptions src out)
  case command of
    Left msg -> return $ Left msg
    Right cmd -> do
      (_, _, _, ph) <-
        createProcess (shell $ cmd ++ " " ++ options) {std_err = CreatePipe}
      code <- waitForProcess ph
      case code of
        ExitFailure _ ->
          return $
          Left
            ("Google Chrome is most likely not installed. " ++
             "Please install Google Chrome to use 'decker pdf' or 'decker pdf-decks'")
        ExitSuccess -> return $ Right ("Completed: " ++ src ++ " -> " ++ out)

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfPage :: FilePath -> FilePath -> Action ()
markdownToPdfPage markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Page Latex
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc

pandocMakePdf :: WriterOptions -> FilePath -> Pandoc -> Action ()
pandocMakePdf options out pandoc =
  liftIO $ do
    result <-
      runIOQuietly (makePDF "xelatex" [] writeLaTeX options pandoc) >>=
      handleError
    case result of
      Left errMsg -> throw $ PandocException (show errMsg)
      Right pdf -> liftIO $ LB.writeFile out pdf

-- | Write a markdown file to a PDF file using the handout template.
markdownToPdfHandout :: FilePath -> FilePath -> Action ()
markdownToPdfHandout markdownFile out = do
  putCurrentDocument out
  let disp = Disposition Handout Latex
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerCiteMethod = Citeproc
          }
  pandocMakePdf options out pandoc
