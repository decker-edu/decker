{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Writer.Html
  ( writeIndexLists,
    markdownToHtmlDeck,
    markdownToHtmlHandout,
    markdownToHtmlPage,
  )
where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.Shake
import qualified System.Directory as Dir
import System.FilePath.Posix
import Text.Decker.Filter.Filter
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import qualified Text.Decker.Writer.Layout as Layout
import Text.DocTemplates
import Text.Pandoc hiding (getTemplate, lookupMeta)
import Text.Pandoc.Highlighting
import Text.Printf

-- | Generates an index.md file with links to all generated files of interest.
writeIndexLists :: Meta -> Targets -> FilePath -> FilePath -> Action ()
writeIndexLists meta targets out baseUrl = do
  let decks = zip (_decks targets) (_decksPdf targets)
  let handouts = zip (_handouts targets) (_handoutsPdf targets)
  let pages = zip (_pages targets) (_pagesPdf targets)
  let questions = zip (_questions targets) (_questions targets)
  decksLinks <- makeGroupedLinks decks
  handoutsLinks <- makeGroupedLinks handouts
  pagesLinks <- makeGroupedLinks pages
  questLinks <- makeGroupedLinks questions
  cwd <- liftIO Dir.getCurrentDirectory
  liftIO $
    writeFile
      out
      [i|
---
title: Generated Index
subtitle: #{cwd}
---
``` {.javascript .run}
import("./" + Decker.meta.supportPath + "/fuzzySearch/search.js")
    .then(module => console.log(module.default(anchor, 0.6)));
```
\# Slide decks
#{unlines decksLinks}
\# Handouts
#{unlines handoutsLinks}
\# Supporting Documents
#{unlines pagesLinks}
\# Questions
#{unlines questLinks}
        |]
  where
    makeLink (html, pdf) = do
      pdfExists <- liftIO $ Dir.doesFileExist pdf
      if pdfExists
        then
          return $
            printf
              "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
              (takeFileName html)
              (makeRelative baseUrl html)
              (makeRelative baseUrl pdf)
        else
          return $
            printf
              "-    [%s <i class='fab fa-html5'></i>](%s)"
              (takeFileName html)
              (makeRelative baseUrl html)
    makeGroupedLinks :: [(FilePath, FilePath)] -> Action [String]
    makeGroupedLinks files =
      let grouped = MM.fromList (zip (map (takeDirectory . fst) files) files)
          renderGroup :: FilePath -> Action [String]
          renderGroup key =
            (printf "\n## %s:" key :) <$> mapM makeLink (MM.lookup key grouped)
       in concat <$> mapM renderGroup (MM.keys grouped)

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc =
  liftIO $
    runIO (writeNative pandocWriterOpts doc) >>= handleError
      >>= T.writeFile (out -<.> mod <.> ".hs")

markdownToHtmlDeck :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck meta getTemplate markdownFile out = do
  if lookupMetaOrElse False "experiment.slide-layout" meta
    then Layout.markdownToHtml (Disposition Deck Html) meta getTemplate markdownFile out
    else markdownToHtmlDeck' meta getTemplate markdownFile out

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlDeck' :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlDeck' meta getTemplate markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let disp = Disposition Deck Html
  pandoc@(Pandoc meta _) <- readAndFilterMarkdownFile disp meta markdownFile
  let highlightStyle =
        case lookupMeta "highlightjs" meta of
          Nothing -> Just pygments
          Just (_ :: T.Text) -> Nothing
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerSlideLevel = Just 1,
            writerSectionDivs = False,
            writerTemplate = Just template,
            writerHighlightStyle = highlightStyle,
            writerHTMLMathMethod =
              MathJax (lookupMetaOrElse "" "mathjax-url" meta),
            writerVariables =
              Context $
                M.fromList
                  [ ( "decker-support-dir",
                      SimpleVal $ Text 0 $ T.pack relSupportDir
                    )
                  ],
            writerCiteMethod = Citeproc
          }
  writePandocFile "revealjs" options out pandoc
  when (lookupMetaOrElse False "write-notebook" meta) $
    markdownToNotebook meta markdownFile (out -<.> ".ipynb")
  writeNativeWhileDebugging out "filtered" pandoc

writePandocFile :: T.Text -> WriterOptions -> FilePath -> Pandoc -> Action ()
writePandocFile fmt options out pandoc =
  liftIO $
    runIO (writeRevealJs options (embedMetaMeta pandoc))
      >>= handleError
      >>= T.writeFile out

-- | Write a markdown file to a HTML file using the page template.
markdownToHtmlPage :: Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlPage meta getTemplate markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let disp = Disposition Page Html
  pandoc@(Pandoc docMeta _) <- readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerSectionDivs = False,
            writerHighlightStyle = Just pygments,
            writerHTMLMathMethod =
              MathJax (lookupMetaOrElse "" "mathjax-url" meta),
            writerVariables =
              Context $
                M.fromList
                  [ ( "decker-support-dir",
                      SimpleVal $ Text 0 $ T.pack relSupportDir
                    )
                  ],
            writerCiteMethod = Citeproc,
            writerTableOfContents = lookupMetaOrElse False "show-toc" docMeta,
            writerTOCDepth = lookupMetaOrElse 1 "toc-depth" docMeta
          }
  Layout.writePandocFile options out pandoc

-- | Write a markdown file to a HTML file using the handout template.
markdownToHtmlHandout ::
  Meta -> TemplateCache -> FilePath -> FilePath -> Action ()
markdownToHtmlHandout meta getTemplate markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let disp = Disposition Handout Html
  pandoc@(Pandoc docMeta _) <-
    wrapSlidesinDivs <$> readAndFilterMarkdownFile disp meta markdownFile
  template <- getTemplate (templateFile disp)
  let options =
        pandocWriterOpts
          { writerTemplate = Just template,
            writerSectionDivs = False,
            writerHighlightStyle = Just pygments,
            writerHTMLMathMethod =
              MathJax (lookupMetaOrElse "" "mathjax-url" meta),
            writerVariables =
              Context $
                M.fromList
                  [ ( "decker-support-dir",
                      SimpleVal $ Text 0 $ T.pack relSupportDir
                    )
                  ],
            writerCiteMethod = Citeproc,
            writerTableOfContents = lookupMetaOrElse False "show-toc" docMeta,
            writerTOCDepth = lookupMetaOrElse 1 "toc-depth" docMeta
          }
  Layout.writePandocFile options out pandoc

-- | Write a markdown file to a HTML file using the page template.
markdownToNotebook :: Meta -> FilePath -> FilePath -> Action ()
markdownToNotebook meta markdownFile out = do
  putCurrentDocument out
  let relSupportDir = relativeSupportDir (takeDirectory out)
  let disp = Disposition Notebook Html
  pandoc@(Pandoc docMeta _) <-
    filterNotebookSlides <$> readAndFilterMarkdownFile disp meta markdownFile
  let options =
        pandocWriterOpts
          { writerTemplate = Nothing,
            writerHighlightStyle = Just pygments,
            writerVariables =
              Context $
                M.fromList
                  [ ( "decker-support-dir",
                      SimpleVal $ Text 0 $ T.pack relSupportDir
                    )
                  ]
          }
  writePandocFile "ipynb" options out pandoc
