{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Filter.Filter
  ( OutputFormat(..)
  , Disposition(..)
  , processPandoc
  , processSlides
  , escapeToFilePath
  , filterNotebookSlides
  , wrapSlidesinDivs
  ) where

import Control.Lens
import Control.Monad.Loops as Loop
import Control.Monad.State
import Data.Default ()
import Data.List.Split
import qualified Data.Text as Text
import Development.Shake (Action)
import Text.Decker.Filter.Layout
import Text.Decker.Filter.MarioCols
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Definition ()
import Text.Pandoc.Lens
import Text.Pandoc.Walk

processPandoc ::
     (Pandoc -> Decker Pandoc)
  -> FilePath
  -> Disposition
  -> Provisioning
  -> Pandoc
  -> Action Pandoc
processPandoc transform base disp prov pandoc =
  evalStateT (transform pandoc) (DeckerState base disp prov)

-- | Split join columns with CSS3. Must be performed after `wrapBoxes`.
splitJoinColumns :: Slide -> Decker Slide
splitJoinColumns slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html -> return $ Slide header $ concatMap wrapRow rowBlocks
      where rowBlocks =
              split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
            wrapRow row@(first:_)
              | hasClass "split" first = [Div ("", ["css-columns"], []) row]
            wrapRow row = row
    Disposition Handout Html ->
      return $ Slide header $ concatMap wrapRow rowBlocks
      where rowBlocks =
              split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
            wrapRow row@(first:_)
              | hasClass "split" first = [Div ("", ["css-columns"], []) row]
            wrapRow row = row
    Disposition _ _ -> return slide

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.
fragmentRelated :: [Text.Text]
fragmentRelated =
  [ "fragment"
  , "grow"
  , "shrink"
  , "roll-in"
  , "fade-in"
  , "fade-out"
  , "current-visible"
  , "highlight-current-blue"
  , "highlight-red"
  , "highlight-green"
  , "highlight-blu"
  ]

deFragment :: [Text.Text] -> [Text.Text]
deFragment = filter (`notElem` fragmentRelated)

-- | Wrap boxes around H2 headers and the following content. All attributes are
-- promoted from the H2 header to the enclosing DIV. Since Pandoc 2.9 the class
-- "column" needs to be added to boxes to prevent sectioning by the Pandoc
-- writer (see `Text.Pandoc.Shared.makeSections`). This must only be done for
-- slide decks, not for handouts or pages.
wrapBoxes :: Slide -> Decker Slide
wrapBoxes slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html -> return $ Slide header $ concatMap (wrap True) boxes
    Disposition _ Html -> return $ Slide header $ concatMap (wrap False) boxes
    Disposition _ _ -> return slide
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap isDeck ((Header 2 (id_, cls, kvs) text):blocks) =
      let tags =
            if isDeck
              then ["box", "columns"]
              else ["box"]
       in [ Div
              ("", tags ++ cls, kvs)
              (Header 2 (id_, deFragment cls, kvs) text : blocks)
          ]
    wrap _ box = box

-- | Map over all active slides in a deck. 
mapSlides :: (Slide -> Decker Slide) -> Pandoc -> Decker Pandoc
mapSlides action (Pandoc meta blocks) = do
  slides <- selectActiveContent (toSlides blocks)
  Pandoc meta . fromSlides <$> mapM action slides

filterNotebookSlides :: Pandoc -> Pandoc
filterNotebookSlides (Pandoc meta blocks) =
  let inNotebook = fromSlides $ filter notebook (toSlides blocks)
      stripped = walk strip inNotebook
      strip (Header level _ inlines) = Header level nullAttr inlines
      strip (CodeBlock (_, classes, _) code)
        | "code" `notElem` classes = CodeBlock nullAttr code
      strip block = block
      notebook slide = "notebook" `elem` (view (attributes . attrClasses) slide)
   in Pandoc meta (deDiv stripped)

wrapSlidesinDivs :: Pandoc -> Pandoc
wrapSlidesinDivs (Pandoc meta blocks) =
  Pandoc meta $ fromSlidesWrapped $ toSlides blocks

selectActiveSlideContent :: Slide -> Decker Slide
selectActiveSlideContent (Slide header body) =
  Slide header <$> selectActiveContent body

-- Splice all the Divs back into the stream of Blocks 
deDiv :: [Block] -> [Block]
deDiv = foldr flatten []
  where
    flatten (Div attr blocks) result = blocks ++ result
    flatten block result = block : result

-- | Slide specific processing.
processSlides :: Pandoc -> Decker Pandoc
processSlides pandoc = mapSlides (concatM actions) pandoc
  where
    actions :: [Slide -> Decker Slide]
    actions =
      case pandocMeta lookupMeta pandoc "mario" of
        Just True ->
          [ marioCols
          , wrapBoxes
          , selectActiveSlideContent
          , splitJoinColumns
          , layoutSlide
          ]
        _ ->
          [wrapBoxes, selectActiveSlideContent, splitJoinColumns, layoutSlide]

selectActiveContent :: HasAttr a => [a] -> Decker [a]
selectActiveContent fragments = do
  disp <- gets disposition
  return $
    case disp of
      Disposition Deck _ -> dropByClass ["comment", "handout"] fragments
      Disposition Handout _ ->
        dropByClass ["comment", "deck", "notes"] fragments
      Disposition Page _ ->
        dropByClass ["comment", "notes", "deck", "handout"] fragments
      Disposition Notebook _ ->
        dropByClass ["comment", "notes", "deck", "handout"] fragments

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where
    repl c =
      if c `elem` [':', '!', '/']
        then '|'
        else c
