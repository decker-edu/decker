{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Notes
  ( processNotesSlides,
    processNotes,
    notesAside,
  )
where

import Data.List qualified as List
import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Pandoc.Walk

-- |  Collects speakernotes and converts them to Reveal compatible <aside>
-- elements. There are a few sources for speaker notes:
--
-- <div class="notes">
-- <span class="notes">
-- # Slide {.notes}
-- ## Box {.notes}

-- | Wraps blocks in a speaker notes <aside> and demotes their headers by one
-- level. The result is tagged, so `processNotes` leaves it alone and headers
-- are demoted exactly once, regardless of how often the filters run.
notesAside :: Attr -> [Block] -> Block
notesAside attr blocks = tag "aside" $ Div attr (demoteHeaders blocks)

isAside :: Attr -> Bool
isAside (_, _, kvs) = List.lookup "data-tag" kvs == Just "aside"

-- | Transforms slides marked `notes` to proper <aside> elements and appends
-- them to the previous slide.
processNotesSlides :: [Slide] -> Decker [Slide]
processNotesSlides slides = do
  let (anchor, processed) = foldl' putNotesAside (Nothing, []) slides
  return $ processed <> maybeToList anchor
  where
    putNotesAside (Just real, slides) slide
      | hasClass "notes" slide =
        (Just $ appendTo real (toAside slide), slides)
    putNotesAside (Just real, slides) slide =
      (Just slide, slides <> [real])
    putNotesAside (Nothing, slides) slide =
      (Just slide, slides)
    toAside (Slide header body _) =
      notesAside ("", ["notes"], []) (maybeToList header <> body)
    appendTo (Slide header blocks direction) block =
      Slide header (blocks <> [block]) direction

processNotes :: Slide -> Decker Slide
processNotes (Slide header body dir) = do
  let (Div _ processed) = walk putNotesAsideDiv (Div nullAttr body)
  return (Slide header processed dir)
  where
    putNotesAsideDiv (Div attr@(_, cls, _) body)
      | "notes" `elem` cls && not (isAside attr) = notesAside attr body
    putNotesAsideDiv div = div
