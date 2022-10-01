{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Writer.Html
  ( writeIndexLists,
    generateIndex,
    writeNativeWhileDebugging,
  )
where

import Control.Monad.State
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.MultiMap as MM
import Data.String.Interpolate (i)
import qualified Data.Text.IO as T
import Development.Shake
import qualified System.Directory as Dir
import System.FilePath.Posix
import Text.Decker.Filter.Index (DeckInfo (deckId, deckSubtitle, deckUrl), Index (..))
import Text.Decker.Filter.Index as Index
import Text.Decker.Internal.Common
import Text.Decker.Project.Project
import Text.Pandoc hiding (getTemplate, lookupMeta)
import Text.Printf

generateIndex :: Meta -> Index -> FilePath -> FilePath -> Action ()
generateIndex meta index out baseUrl = do
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

::: decks

| Title | DeckId | Draft |
|-------|--------|-------|
#{makeDeckTableRows index}

:::
      |]


makeDeckTableRows index =
  let decks = Map.elems $ Index.decks index
      grouped = sortBy (\a b -> compare (Index.deckUrl a) (Index.deckUrl b)) decks
   in unlines $ map makeDeckTableRow grouped

makeDeckTableRow deck =
  let title = fromMaybe "<untitled>" $ Index.deckTitle deck
      url = Index.deckUrl deck
      subtitle = fromMaybe "" $ Index.deckSubtitle deck
      deckid = fromMaybe "" $ Index.deckId deck
      draft = Index.deckDraft deck
   in if draft
        then [i|| [#{title}](#{url}){.title .draft} [#{subtitle}]{.subtitle .draft} | [#{deckid}]{.deck-id .draft} | [DRAFT]{.draft} ||]
        else [i|| [#{title}](#{url}){.title} [#{subtitle}]{.subtitle} | [#{deckid}]{.deck-id} | []{.no-draft} ||]

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
