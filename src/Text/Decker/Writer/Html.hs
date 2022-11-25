{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Writer.Html
  ( writeIndexLists,
    writeNativeWhileDebugging,
  )
where

import Control.Lens ((^.))
import Control.Monad.State
import Data.List (sort)
import qualified Data.Map.Strict as Map
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
writeIndexLists :: Meta -> Targets -> FilePath -> Action ()
writeIndexLists meta targets out = do
  let get field = sort $ Map.keys $ (targets ^. field)
  let decks' = zip (get decks) (get decksPdf)
  let handouts' = zip (get handouts) (get handoutsPdf)
  let pages' = zip (get pages) (get pagesPdf)
  let questions' = zip (get questions) (get questions)
  decksLinks <- makeGroupedLinks publicDir decks'
  handoutsLinks <- makeGroupedLinks publicDir handouts'
  pagesLinks <- makeGroupedLinks publicDir pages'
  questLinks <- makeGroupedLinks privateDir questions'
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
    .then((module) => {
      anchor.classList.add("search");
      anchor.innerHTML = `
        <p>
          <i class="fa-solid fa-magnifying-glass"></i>
          <input class="search" placeholder="In Folien suchen" type="text">
        </p>
        <table class="search">
        <thead><tr><th>Wort</th><th>Foliensatz</th><th>Folie</th><th>Treffer</th></tr></thead>
        <tbody></tbody>
        </table>
      `;
      module.default(anchor, 0.6);
    });
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
    makeLink baseDir (html, pdf) = do
      pdfExists <- liftIO $ Dir.doesFileExist pdf
      if pdfExists
        then
          return $
            printf
              "-    [%s <i class='fab fa-html5'></i>](%s) [<i class='fas fa-file-pdf'></i>](%s)"
              (takeFileName html)
              (makeRelative baseDir html)
              (makeRelative baseDir pdf)
        else
          return $
            printf
              "-    [%s <i class='fab fa-html5'></i>](%s)"
              (takeFileName html)
              (makeRelative baseDir html)
    makeGroupedLinks :: FilePath -> [(FilePath, FilePath)] -> Action [String]
    makeGroupedLinks baseDir files =
      let grouped = MM.fromList (zip (map (takeDirectory . fst) files) files)
          renderGroup :: FilePath -> Action [String]
          renderGroup key =
            (printf "\n## %s:" key :) <$> mapM (makeLink baseDir) (MM.lookup key grouped)
       in concat <$> mapM renderGroup (MM.keys grouped)

-- | Write Pandoc in native format right next to the output file
writeNativeWhileDebugging :: FilePath -> String -> Pandoc -> Action ()
writeNativeWhileDebugging out mod doc =
  liftIO $
    runIO (writeNative pandocWriterOpts doc) >>= handleError
      >>= T.writeFile (out -<.> mod <.> ".hs")
