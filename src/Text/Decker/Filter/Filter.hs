{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Filter
  ( OutputFormat (..),
    Disposition (..),
    processPandoc,
    processSlides,
    escapeToFilePath,
    filterNotebookSlides,
    wrapSlidesinDivs,
    runDynamicFilters,
    FilterPosition (..),
  )
where

import Control.Lens
import Control.Monad.Loops as Loop
import Data.Default ()
import Data.List qualified as List
import Data.List.Split
import Data.Text qualified as Text
import Development.Shake
import Relude
import System.FilePath
import Text.Decker.Filter.Detail
import Text.Decker.Filter.Div
import Text.Decker.Filter.Incremental
import Text.Decker.Filter.Layout (layoutSlide)
import Text.Decker.Filter.MarioCols
import Text.Decker.Filter.Notes
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Definition ()
import Text.Pandoc.Filter
import Text.Pandoc.Lens
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Decker.Filter.Select (dropSolutionBlocks)

data FilterPosition = Before | After deriving (Show, Eq)

runDynamicFilters :: FilterPosition -> FilePath -> Pandoc -> Action Pandoc
runDynamicFilters position baseDir pandoc@(Pandoc meta blocks) = do
  let paths :: [Text] = lookupMetaOrElse [] (key position) meta
  let filters = map (mkFilter . makeProjectPath baseDir . toString) paths
  if not $ null filters
    then liftIO $ runIOorExplode $ applyFilters env filters ["html"] pandoc
    else return pandoc
  where
    env = Environment pandocReaderOpts pandocWriterOpts
    key Before = "pandoc.filters.before"
    key After = "pandoc.filters.after"
    mkFilter path =
      if takeExtension path == ".lua"
        then LuaFilter path
        else JSONFilter path

processPandoc ::
  (Pandoc -> Decker Pandoc) ->
  FilePath ->
  Disposition ->
  Pandoc ->
  Action Pandoc
processPandoc transform base disp pandoc =
  evalStateT (transform pandoc) (DeckerState base disp 0)

-- | Split join columns with CSS3. Must be performed after `wrapBoxes`.
splitJoinColumns :: Slide -> Decker Slide
splitJoinColumns slide@(Slide header body dir) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html -> return $ Slide header (concatMap wrapRow rowBlocks) dir
      where
        rowBlocks =
          split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
        wrapRow row@(first : _)
          | hasClass "split" first = [Div ("", ["css-columns"], []) row]
        wrapRow row = row
    Disposition Handout Html ->
      return $ Slide header (concatMap wrapRow rowBlocks) dir
      where
        rowBlocks =
          split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
        wrapRow row@(first : _)
          | hasClass "split" first = [Div ("", ["css-columns"], []) row]
        wrapRow row = row
    Disposition _ _ -> return slide

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.
fragmentRelated :: [Text.Text]
fragmentRelated =
  [ "fragment",
    "grow",
    "shrink",
    "roll-in",
    "fade-in",
    "fade-out",
    "current-visible",
    "highlight-current-blue",
    "highlight-red",
    "highlight-green",
    "highlight-blu"
  ]

deFragment :: [Text.Text] -> [Text.Text]
deFragment = filter (`notElem` fragmentRelated)

-- | Wrap DIVs around top-level H2 headers and the following content. All
-- attributes are promoted from the H2 header to the enclosing DIV.
wrapBoxes :: Slide -> Decker Slide
wrapBoxes slide@(Slide header body dir) = do
  disp <- gets disposition
  case disp of
    Disposition _ Html -> return $ Slide header (concatMap wrap boxes) dir
    Disposition _ _ -> return slide
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap [] = []
    wrap ((Header 2 attr@(id, cls, kvs) text) : blocks)
      | "details" `elem` cls = [makeFramedDetail attr text blocks]
    wrap ((Header 2 (id_, cls, kvs) text) : blocks)
      | "notes" `elem` cls =
          [ tag "aside" $
              Div
                (id_, ["notes"], [])
                (Header 2 (id_, deFragment cls, kvs) text : blocks)
          ]
    wrap ((Header 2 (id_, cls, kvs) text) : blocks) =
      [ Div
          (id_, ["box", "block"] ++ cls, mangle kvs)
          (Header 2 (id_, deFragment cls, kvs) text : blocks)
      ]
    wrap blocks = [Div ("", ["box", "block"], []) blocks]
    mangle kvs =
      case List.lookup "width" kvs of
        Just w -> ("style", "width:" <> w <> ";") : List.filter ((/=) "width" . fst) kvs
        Nothing -> kvs

-- | Map over all active slides in a deck.
mapSlides :: (Slide -> Decker Slide) -> Pandoc -> Decker Pandoc
mapSlides action (Pandoc meta blocks) = do
  slides <-
    selectActiveContent (toSlides blocks)
      >>= processNotesSlides
      >>= mapM action
      >>= fromSlidesD
  return $ Pandoc meta slides

filterNotebookSlides :: Pandoc -> Pandoc
filterNotebookSlides (Pandoc meta blocks) =
  let notebook slide = "notebook" `elem` view (attributes . attrClasses) slide
      inNotebook = fromSlides $ filter notebook (toSlides blocks)
      stripped = walk strip inNotebook
   in Pandoc meta (deDiv stripped)
  where
    strip (Header level _ inlines) = Header level nullAttr inlines
    strip (CodeBlock (_, classes, _) code)
      | "code" `notElem` classes = CodeBlock nullAttr code
    strip block = block

wrapSlidesinDivs :: Pandoc -> Pandoc
wrapSlidesinDivs (Pandoc meta blocks) =
  Pandoc meta $ fromSlidesWrapped $ toSlides blocks

selectActiveSlideContent :: Slide -> Decker Slide
selectActiveSlideContent (Slide header body dir) = do
  body <- selectActiveContent body
  return $ Slide header body dir

-- Splice all the Divs back into the stream of Blocks
deDiv :: [Block] -> [Block]
deDiv = foldr flatten []
  where
    flatten (Div attr blocks) result = blocks ++ result
    flatten block result = block : result

-- | Slide specific processing.
processSlides :: Pandoc -> Decker Pandoc
processSlides pandoc@(Pandoc meta _) = mapSlides (concatM actions) pandoc
  where
    actions :: [Slide -> Decker Slide]
    actions =
      [ marioCols,
        divBasedLayout,
        processNotes,
        pauseDots,
        wrapBoxes,
        dropSolutionBlocks meta, 
        processNotes,
        incrementalBlocks,
        selectActiveSlideContent,
        splitJoinColumns,
        layoutSlide
      ]

pauseDots :: Slide -> Decker Slide
pauseDots (Slide header body dir) = do
  let fragments = split (dropDelims $ whenElt ((== ". . .") . stringify)) body
  let blocks = case fragments of
        bls : blss ->
          concat $
            bls : map ((: []) . Div ("", ["fragment"], [])) blss
        blss -> concat blss
  return (Slide header blocks dir)

selectActiveContent :: HasAttr a => [a] -> Decker [a]
selectActiveContent fragments = do
  disp <- gets disposition
  return $
    case disp of
      Disposition Deck _ -> dropByClass ["comment", "handout"] fragments
      Disposition Handout _ ->
        dropByClass ["comment", "deck", "notes"] fragments
      _ ->
        dropByClass ["comment", "notes", "deck", "handout"] fragments

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where
    repl c =
      if c `elem` [':', '!', '/']
        then '|'
        else c
