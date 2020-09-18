{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Decker.Filter.Slide
  ( Slide(..)
  , _Slide
  , attribValue
  , blocks
  , dropByClass
  , keepByClass
  , firstClass
  , fromSlides
  , fromSlidesWrapped
  , classes
  , hasAnyClass
  , hasClass
  , header
  , isBoxDelim
  , toSlides
  ) where

import Text.Pandoc.Lens

import Control.Lens
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Pandoc
import Text.Pandoc.Definition ()
import qualified Data.Text as Text

-- A slide has maybe a header followed by zero or more blocks.
data Slide = Slide
  { _header :: Maybe Block
  , _body :: [Block]
  } deriving (Eq, Show)

-- | A lens for header access on a slide. See
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
header :: Lens' Slide (Maybe Block)
header = lens (\(Slide h _) -> h) (\(Slide _ b) h -> Slide h b)

-- | A lens for blocks access on a slide. 
blocks :: Lens' Slide [Block]
blocks = lens (\(Slide _ b) -> b) (\(Slide h _) b -> Slide h b)

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
-- rule. Slides with the `notes` classes are wrapped in ASIDE and are used as
-- spreaker notes by RevalJs.
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

-- | Converts slides to lists of blocks that are wrapped in divs. Used to
-- control page breaks in handout generation.
fromSlidesWrapped :: [Slide] -> [Block]
fromSlidesWrapped = concatMap wrapBlocks
  where
    wrapBlocks (Slide (Just header) body) =
      [Div ("", ["slide-wrapper"], []) (HorizontalRule : header : body)]
    wrapBlocks (Slide Nothing body) =
      [Div ("", ["slide-wrapper"], []) (HorizontalRule : body)]

isSlideSeparator :: Block -> Bool
isSlideSeparator (Header 1 _ _) = True
isSlideSeparator HorizontalRule = True
isSlideSeparator _ = False

demoteHeaders = traverse . _Header . _1 +~ 1

classes :: HasAttr a => a -> [Text.Text]
classes = view (attributes . attrClasses)

hasClass :: HasAttr a => Text.Text -> a -> Bool
hasClass which = elem which . classes

hasAnyClass :: HasAttr a => [Text.Text] -> a -> Bool
hasAnyClass which = isJust . firstClass which

firstClass :: HasAttr a => [Text.Text] -> a -> Maybe Text.Text
firstClass which fragment = find (`hasClass` fragment) which

attribValue :: HasAttr a => Text.Text -> a -> Maybe Text.Text
attribValue which = lookup which . view (attributes . attrs)

dropByClass :: HasAttr a => [Text.Text] -> [a] -> [a]
dropByClass which =
  filter (not . any (`elem` which) . view (attributes . attrClasses))

keepByClass :: HasAttr a => [Text.Text] -> [a] -> [a]
keepByClass which =
  filter (any (`elem` which) . view (attributes . attrClasses))

isBoxDelim :: Block -> Bool
isBoxDelim (Header 2 _ _) = True
isBoxDelim _ = False
