{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Filter.Slide
  ( Slide (..),
    _Slide,
    attribValue,
    dropByClass,
    keepByClass,
    firstClass,
    fromSlides,
    fromSlidesWrapped,
    classes,
    hasAnyClass,
    hasClass,
    header,
    body,
    dir,
    isBoxDelim,
    toSlides,
    fromSlidesD,
    tag,
    Direction (..),
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.State (gets, modify)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Decker.Internal.Common (Decker, DeckerState (emptyCount))
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Lens hiding (body)

data Direction = Horizontal | Vertical deriving (Show, Eq)

-- A slide has maybe a header followed by zero or more blocks.
data Slide = Slide
  { _header :: Maybe Block,
    _body :: [Block],
    _dir :: Direction
  }
  deriving (Eq, Show)

makeLenses ''Slide

-- | A lens for header access on a slide. See
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
-- header :: Lens' Slide (Maybe Block)
-- header = lens (\(Slide h _ _) -> h) (\(Slide _ b d) h -> Slide h b d)

-- | A lens for blocks access on a slide.
-- blocks :: Lens' Slide [Block]
-- blocks = lens (\(Slide _ b _) -> b) (\(Slide h _ d) b -> Slide h b d)

-- | A Prism for slides
_Slide :: Prism' Slide (Maybe Block, [Block], Direction)
_Slide = prism' (\(h, b, d) -> Slide h b d) (\(Slide h b d) -> Just (h, b, d))

-- | Attributes of a slide are those of the header
instance HasAttr Slide where
  attributes f (Slide (Just (Header n a s)) b d) =
    fmap (\a' -> Slide (Just (Header n a' s)) b d) (f a)
  attributes _ x = pure x

-- | Attributes of a list of blocks are those of the first block.
instance HasAttr [Block] where
  attributes f (b : bs) =
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
    extractHeader (header@Header {} : bs) =
      Slide
        (Just header)
        bs
        (if hasClass "sub" header then Vertical else Horizontal)
    extractHeader (HorizontalRule : bs) = extractHeader bs
    extractHeader blocks = Slide Nothing blocks Horizontal
    -- Remove redundant slide markers
    killEmpties (HorizontalRule : header@Header {} : blocks) =
      header : killEmpties blocks
    killEmpties (b : bs) = b : killEmpties bs
    killEmpties [] = []

-- Render slides as a list of Blocks. Always separate slides with a horizontal
-- rule. Slides with the `notes` classes are wrapped in ASIDE and are used as
-- speaker notes by Reval. Slides with no header get an empty header prepended.
fromSlides :: [Slide] -> [Block]
fromSlides = concatMap prependHeader
  where
    prependHeader (Slide (Just header) body _)
      | hasClass "notes" header =
        [RawBlock "html" "<aside class=\"notes\">"]
          ++ demoteHeaders (header : body)
          ++ [RawBlock "html" "</aside>"]
    prependHeader (Slide (Just header) body _) = HorizontalRule : header : body
    prependHeader (Slide Nothing body _) = HorizontalRule : body

-- Render slides as a list of Blocks. Always separate slides with a horizontal
-- rule. Slides with the `notes` classes are wrapped in ASIDE and are used as
-- speaker notes by Reval. Slide with a `sub` class are vertical slides and are
-- wrapped in an extra section. Slides with no header get an empty header prepended.
fromSlidesD :: [Slide] -> Decker [Block]
fromSlidesD slides = do
  -- mapM_ (\s -> when (view dir s == Vertical) (pPrint s)) slides
  (subs, blocks) <- foldM resolveSubs ([], []) slides
  return (blocks <> subs)
  where
    -- No verticals so far, next is horizontal.
    resolveSubs ([], blocks) slide@(Slide header body Horizontal) = do
      h <- wrapSection slide
      return ([], blocks <> h)
    -- Some verticals, next is horizontal. Wrap the vertical list in an extra
    -- section.
    resolveSubs (verticals, blocks) slide@(Slide header body Horizontal)
      | length verticals > 1 = do
        h <- wrapSection slide
        return (h, blocks <> [tag "section" (Div ("", ["vertical"], []) verticals)])
    resolveSubs (verticals, blocks) slide@(Slide header body Horizontal) = do
      h <- wrapSection slide
      return (h, blocks <> verticals)
    -- Add slide to the verticals
    resolveSubs (subs, blocks) slide@(Slide header body Vertical) = do
      h <- wrapSection slide
      return (subs <> h, blocks)
    -- Wraps a single slide in a header. Handles notes and stuff
    wrapSection (Slide (Just header@(Header n attr inlines)) body _)
      | hasClass "notes" header =
        return [tag "aside" $ Div nullAttr $ demoteHeaders (header : body)]
    wrapSection (Slide (Just (Header n attr inlines)) body _) =
      return $ wrap attr (Header n ("", [], []) inlines : body)
    wrapSection (Slide _ body _) = do
      rid <- emptyId
      return $ Header 1 (rid, [], []) [] : body
    wrap (id, cls, kvs) blocks =
      [ tag "section" $
          Div
            (id, cls ++ ["slide", "level1"], kvs)
            [ Div
                ("", ["decker"], [])
                [ Div
                    ("", ["alignment"], [])
                    blocks
                ]
            ]
      ]

tag :: HasAttr a => Text -> a -> a
tag name = over (attributes . attrs) (("data-tag", name) :)

emptyId :: Decker Text
emptyId = do
  modify incrEmptyCount
  Text.pack . ("empty-" <>) . show <$> gets emptyCount
  where
    incrEmptyCount s = s {emptyCount = emptyCount s + 1}

-- |  Converts slides to lists of blocks that are wrapped in divs. Used to
--  control page breaks in handout generation.
fromSlidesWrapped :: [Slide] -> [Block]
fromSlidesWrapped = concatMap wrapBlocks
  where
    wrapBlocks (Slide (Just header) body _) =
      [Div ("", ["slide-wrapper"], []) (HorizontalRule : header : body)]
    wrapBlocks (Slide Nothing body _) =
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
