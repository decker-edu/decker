{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Utilities
  ( runDecker
  , writeIndexLists
  , markdownToHtmlDeck
  , markdownToHtmlHandout
  , markdownToPdfHandout
  , markdownToHtmlPage
  , markdownToPdfPage
  , metaValueAsString
  , pandocMakePdf
  , pandocReaderOpts
  , toPandocMeta
  , DeckerException(..)
  ) where

import System.Decker.OS
import Text.Decker.Filter.Filter
import Text.Decker.Filter.Macro
import Text.Decker.Filter.Quiz
import Text.Decker.Filter.Render
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Project.Sketch
import Text.Decker.Project.Version
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Resource
import Text.Decker.Server.Server
import Text.Pandoc.Lens

import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Lens ((.~), (^.), (^?), at, set)
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Dynamic
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef
import Data.List as List
import Data.List.Extra as List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Development.Shake
import Development.Shake.FilePath as SFP
import Network.URI
import qualified System.Directory as Dir
import System.FilePath.Glob
import Text.CSL.Pandoc
import qualified Text.Mustache as M
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Highlighting
import Text.Pandoc.PDF
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Read (readMaybe)

-- | Generates an index.md file with links to all generated files of interest.
writeIndexLists :: FilePath -> FilePath -> Action ()
writeIndexLists out baseUrl = do
  dirs <- projectDirsA
  ts <- targetsA
  let decks = (zip (_decks ts) (_decksPdf ts))
  let handouts = (zip (_handouts ts) (_handoutsPdf ts))
  let pages = (zip (_pages ts) (_pagesPdf ts))
  decksLinks <- mapM makeLink decks
  handoutsLinks <- mapM makeLink handouts
  pagesLinks <- mapM makeLink pages
  liftIO $
    writeFile out $
    unlines
      [ "---"
      , "title: Generated Index"
      , "subtitle: " ++ dirs ^. project
      , "---"
      , "# Slide decks"
      , unlines $ decksLinks
      , "# Handouts"
      , unlines $ handoutsLinks
      , "# Supporting Documents"
      , unlines $ pagesLinks
      ]
  where
    makeLink (html, pdf) = do
      pdfExists <- doesFileExist pdf
      if pdfExists
        then return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)
               (makeRelative baseUrl pdf)
        else return $
             printf
               "-    [%s <i class='fab fa-html5'></i>](%s)"
               (takeFileName html)
               (makeRelative baseUrl html)

getTemplate :: Meta -> Disposition -> Action String
getTemplate meta disp = do
  let templateOverridePath =
        case templateFromMeta meta of
          Just template -> Just $ template </> getTemplateFileName disp
          Nothing -> Nothing
  if isJust templateOverridePath
    then do
      let templateOverridePath' = fromJust templateOverridePath
      need [templateOverridePath']
      liftIO $ readFile templateOverridePath'
    else liftIO $ getResourceString ("template" </> (getTemplateFileName disp))

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc@(Pandoc meta body) = do
  liftIO $
    runIOQuietly (writeNative pandocWriterOpts doc) >>= handleError >>=
    T.writeFile (out -<.> mod <.> ".hs")

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck :: FilePath -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck markdownFile out index = do
  putCurrentDocument out
  supportDir <- _support <$> projectDirsA
  supportDirRel <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Deck Html
  pandoc@(Pandoc meta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate meta disp
  templateSupportDir <- getSupportDir meta out supportDirRel
  dachdeckerUrl' <- liftIO getDachdeckerUrl
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1
          , writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (supportDirRel </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables =
              [ ( "revealjs-url"
                , supportDirRel </> "node_modules" </> "reveal.js")
              , ("decker-support-dir", templateSupportDir)
              , ("dachdecker-url", dachdeckerUrl')
              ]
          , writerCiteMethod = Citeproc
          }
  writeDeckIndex markdownFile index pandoc >>=
    writePandocFile "revealjs" options out
  writeNativeWhileDebugging out "filtered" pandoc

runIOQuietly :: PandocIO a -> IO (Either PandocError a)
runIOQuietly act = runIO (setVerbosity ERROR >> act)

writePandocFile :: String -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile fmt options out pandoc =
  liftIO $
  case getWriter fmt of
    Right (TextWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      B.writeFile out . E.encodeUtf8
    Right (ByteStringWriter writePandoc, _) ->
      runIOQuietly (writePandoc options pandoc) >>= handleError >>=
      LB.writeFile out
    Left e -> throw $ PandocException e

-- | Determines which template file name to use
-- for a certain disposition type
getTemplateFileName :: Disposition -> String
getTemplateFileName (Disposition Deck Html) = "deck.html"
getTemplateFileName (Disposition Deck Latex) = "deck.tex"
getTemplateFileName (Disposition Page Html) = "page.html"
getTemplateFileName (Disposition Page Latex) = "page.tex"
getTemplateFileName (Disposition Handout Html) = "handout.html"
getTemplateFileName (Disposition Handout Latex) = "handout.tex"

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = do
  public <- publicA
  let rel = makeRelative public out
  putNormal $ "# pandoc (for " ++ rel ++ ")"

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: FilePath -> FilePath -> Action ()
markdownToHtmlPage markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Page Html
  pandoc@(Pandoc docMeta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate docMeta disp
  templateSupportDir <- getSupportDir docMeta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupBool "show-toc" False docMeta
          , writerTOCDepth = lookupInt "toc-depth" 1 docMeta
          }
  writePandocFile "html5" options out pandoc

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

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout :: FilePath -> FilePath -> Action ()
markdownToHtmlHandout markdownFile out = do
  putCurrentDocument out
  supportDir <- getRelativeSupportDir (takeDirectory out)
  let disp = Disposition Handout Html
  pandoc@(Pandoc docMeta _) <- readAndProcessMarkdown markdownFile disp
  template <- getTemplate docMeta disp
  templateSupportDir <- getSupportDir docMeta out supportDir
  let options =
        pandocWriterOpts
          { writerTemplate = Just template
          , writerHighlightStyle = Just pygments
          , writerHTMLMathMethod =
              MathJax
                (urlPath $
                 supportDir </> "node_modules" </> "mathjax" </>
                 "MathJax.js?config=TeX-AMS_HTML")
          , writerVariables = [("decker-support-dir", templateSupportDir)]
          , writerCiteMethod = Citeproc
          , writerTableOfContents = lookupBool "show-toc" False docMeta
          , writerTOCDepth = lookupInt "toc-depth" 1 docMeta
          }
  writePandocFile "html5" options out pandoc

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

lookupValue :: String -> Y.Value -> Maybe Y.Value
lookupValue key (Y.Object hashTable) = HashMap.lookup (T.pack key) hashTable
lookupValue _ _ = Nothing

-- TODO: move to Meta.hs?
-- used in Decker.hs
metaValueAsString :: String -> Y.Value -> Maybe String
metaValueAsString key meta =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (lookupValue k meta)
  where
    lookup' :: [String] -> Maybe Y.Value -> Maybe String
    lookup' [] (Just (Y.String s)) = Just (T.unpack s)
    lookup' [] (Just (Y.Number n)) = Just (show n)
    lookup' [] (Just (Y.Bool b)) = Just (show b)
    lookup' (k:ks) (Just obj@(Y.Object _)) = lookup' ks (lookupValue k obj)
    lookup' _ _ = Nothing
