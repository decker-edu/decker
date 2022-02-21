{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Notes
  ( processNotesSlides,
    processNotes,
  )
where

import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Pandoc.Walk

-- |  Collects speakernotes and converts them to Reveal compatible <aside>
-- elements. There are a few sources for speaker notes:
--
-- <div class="notes">
-- <span class="notes">

-- | Transforms slides marked `notes` to proper <aside> elements and appends
-- them to the previous slide.
processNotesSlides :: [Slide] -> Decker [Slide]
processNotesSlides slides =
  return $ snd $ foldl' putNotesAside (Nothing, []) slides
  where
    putNotesAside (Just real, slides) slide
      | hasClass "notes" slide =
        (Just $ appendTo real (toDiv slide), slides)
    putNotesAside (Just real, slides) slide =
      (Just slide, slides <> [real])
    putNotesAside (Nothing, slides) slide =
      (Just slide, slides)
    toDiv slide =
      Div
        ("", ["notes"], [])
        (demoteSlideHeaders slide)
    appendTo (Slide header blocks direction) block =
      Slide header (blocks <> [block]) direction
    demoteSlideHeaders (Slide (Just header) body _) =
      demoteHeaders [header] <> demoteHeaders body
    demoteSlideHeaders (Slide Nothing body dir) = demoteHeaders body

processNotes :: Slide -> Decker Slide
processNotes (Slide header body dir) = do
  let (Div _ processed) = walk putNotesAsideDiv $ walk putNotesAsideSpan (Div nullAttr body)
  return (Slide header processed dir)
  where
    putNotesAsideDiv (Div attr@(_, cls, _) body)
      | "notes" `elem` cls =
        tag "aside" $ Div attr (demoteHeaders body)
    putNotesAsideDiv div = div
    putNotesAsideSpan span@(Span attr@(_, cls, _) _)
      | "notes" `elem` cls =
        tag "aside" span
    putNotesAsideSpan span = span
