module Text.Decker.Writer.Pdf
  ( launchChrome,
    markdownToPdfHandout,
    markdownToPdfPage,
  )
where

import Control.Exception
import Data.ByteString.Lazy qualified as LB
import Development.Shake
import System.Decker.OS
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.Pandoc hiding (getTemplate)
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF

chromeUserDataDir = (</> "chrome") <$> transientDir

chromeOptions :: FilePath -> FilePath -> IO [String]
chromeOptions src out = do
  dataDir <- chromeUserDataDir
  return
    [ "--headless",
      "--virtual-time-budget=5001",
      "--disable-gpu",
      "--print-to-pdf-no-header",
      "--user-data-dir=" <> dataDir,
      pdfOption out,
      modifySrc src
    ]
  where
    modifySrc path = path ++ "?print-pdf#/"
    pdfOption path = "--print-to-pdf=" ++ path

launchChrome :: FilePath -> FilePath -> IO (Either String String)
launchChrome src out = do
  dataDir <- chromeUserDataDir
  command <- chrome
  options <- chromeOptions src out
  case command of
    Left msg -> return $ Left msg
    Right cmd -> do
      -- putStrLn (cmd <> " " <> unwords options)
      createDirectoryIfMissing True dataDir
      (exitCode, stdOut, stdErr) <-
        readProcessWithExitCode cmd options ""
      return $
        case exitCode of
          ExitSuccess -> Right ("Completed: " ++ src ++ " -> " ++ out)
          ExitFailure code ->
            Left ("Error " <> show code <> ": " <> stdOut <> "\n" <> stdErr)

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
