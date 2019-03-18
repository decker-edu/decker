module Slide
  ( Slide(..)
  , header
  , blocks
  , _Slide
  , hasClass
  , toSlides
  , fromSlides
  ) where

import Text.Pandoc.Lens

import Text.Pandoc
import Text.Pandoc.Definition ()
import Control.Lens
import Data.List.Split
import Data.Maybe

-- A slide has maybe a header followed by zero or more blocks.
data Slide = Slide
  { _header :: Maybe Block
  , _body :: [Block]
  } deriving (Eq, Show)

-- | A lens for header access on a slide. See
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
header :: Lens' Slide (Maybe Block)
header = lens (\(Slide h _) -> h) (\(Slide _ b) h -> (Slide h b))

-- | A lens for blocks access on a slide. 
blocks :: Lens' Slide [Block]
blocks = lens (\(Slide _ b) -> b) (\(Slide h _) b -> (Slide h b))

-- | A Prism for slides
_Slide :: Prism' Slide (Maybe Block, [Block])
_Slide = prism' (uncurry Slide) (\(Slide h b) -> Just (h, b))

-- | Attributes of a slide are those of the header 
instance HasAttr Slide where
  attributes f (Slide (Just (Header n a s)) b) =
    fmap (\a' -> Slide (Just (Header n a' s)) b) (f a)
  attributes _ x = pure x

-- | Attributes of a list of blocks are those of the first block. 
instance HasAttr [Block] where
  attributes f (b:bs) =
    fmap (\a' -> set attributes a' b : bs) (f (view attributes b))
  attributes _ x = pure x

-- Converts blocks to slides. Slides start at H1 headers or at horizontal rules.
-- A horizontal rule followed by a H1 header collapses to one slide.
toSlides :: [Block] -> [Slide]
toSlides blocks = map extractHeader $ filter (not . null) slideBlocks
  where
    slideBlocks =
      split (keepDelimsL $ whenElt isSlideSeparator) $ killEmpties blocks
  -- Deconstruct a list of blocks into a Slide
    extractHeader (header@(Header 1 _ _):bs) = Slide (Just header) bs
    extractHeader (HorizontalRule:bs) = extractHeader bs
    extractHeader blocks = Slide Nothing blocks
  -- Remove redundant slide markers
    killEmpties (HorizontalRule:header@Header {}:blocks) =
      header : killEmpties blocks
    killEmpties (b:bs) = b : killEmpties bs
    killEmpties [] = []

-- Render slides as a list of Blocks. Always separate slides with a horizontal
-- rule. Slides with the `notes` classes are wrapped in ASIDE and
-- are used as spreaker notes by RevalJs.
fromSlides :: [Slide] -> [Block]
fromSlides = concatMap prependHeader
  where
    prependHeader (Slide (Just header) body)
      | hasClass "notes" header =
        [RawBlock "html" "<aside class=\"notes\">"] ++
        demoteHeaders (header : body) ++
        [RawBlock "html" "</aside>"]
    prependHeader (Slide (Just header) body) = HorizontalRule : header : body
    prependHeader (Slide Nothing body) = HorizontalRule : body

isSlideSeparator :: Block -> Bool
isSlideSeparator (Header 1 _ _) = True
isSlideSeparator HorizontalRule = True
isSlideSeparator _ = False

demoteHeaders = traverse . _Header . _1 +~ 1

hasClass :: HasAttr a => String -> a -> Bool
hasClass which = elem which . view (attributes . attrClasses)

hasAnyClass :: HasAttr a => [String] -> a -> Bool
hasAnyClass which = isJust . firstClass which

firstClass :: HasAttr a => [String] -> a -> Maybe String
firstClass which fragment = listToMaybe $ filter (`hasClass` fragment) which

attribValue :: HasAttr a => String -> a -> Maybe String
attribValue which = lookup which . view (attributes . attrs)

dropByClass :: HasAttr a => [String] -> [a] -> [a]
dropByClass which =
  filter (not . any (`elem` which) . view (attributes . attrClasses))

