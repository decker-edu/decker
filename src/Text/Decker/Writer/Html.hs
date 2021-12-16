{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Writer.Html
  ( writeIndexLists,
    writeNativeWhileDebugging,
  )
where

import Control.Monad.State
import qualified Data.MultiMap as MM
import Data.String.Interpolate (i)
import qualified Data.Text.IO as T
import Development.Shake
import qualified System.Directory as Dir
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Project.Project
import Text.Pandoc hiding (getTemplate, lookupMeta)
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
