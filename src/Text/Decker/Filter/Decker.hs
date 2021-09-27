{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the new Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker (runFilter, mediaFilter) where

import Relude
import Text.Decker.Filter.Header
import Text.Decker.Filter.Image
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Util (forceBlock, oneImagePerLine)
import Text.Decker.Internal.Common
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Walk

-- | Applies a filter to each pair of successive elements in a list. The filter
-- may consume the elements and return a list of transformed elements, or it
-- may reject the pair and return Nothing.
pairwise :: ((a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
pairwise f (x : y : zs) = do
  match <- f (x, y)
  case match of
    Just rs -> (rs ++) <$> pairwise f zs
    Nothing -> (x :) <$> pairwise f (y : zs)
pairwise _ xs = return xs

-- | Applies a filter to each triplet of successive elements in a list.
-- The filter may consume the elements and return a list of transformed elements,
-- or it may reject the triplet and return Nothing.
tripletwise :: ((a, a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
tripletwise f (w : x : y : zs) = do
  match <- f (w, x, y)
  case match of
    Just rs -> (rs ++) <$> tripletwise f zs
    Nothing -> (w :) <$> tripletwise f (x : y : zs)
tripletwise _ xs = return xs

-- | Runs the document through the four increaingly detailed filter stages. The
-- matching granularity ranges from list of blocks to single inline elements.
mediaFilter :: Disposition -> WriterOptions -> Pandoc -> IO Pandoc
mediaFilter dispo options pandoc =
  runFilter dispo options transformHeader1 pandoc
    >>= runFilter dispo options mediaBlockListFilter
    >>= runFilter dispo options mediaInlineListFilter
    >>= runFilter dispo options mediaBlockFilter
    >>= runFilter dispo options mediaInlineFilter

-- | Filters lists of Blocks that can match in pairs or triplets.
--
-- For example: Match a paragraph containing just an image followed by a
-- paragraph starting with the string "Image: " as an image that is to be
-- placed in a figure block with a caption.
mediaBlockListFilter :: [Block] -> Filter [Block]
mediaBlockListFilter blocks =
  tripletwise filterTriplets blocks >>= pairwise filterPairs
  where
    filterPairs :: (Block, Block) -> Filter (Maybe [Block])
    -- An image followed by an explicit caption paragraph.
    filterPairs (Para [image@Image {}], Para (Str "Caption:" : caption)) =
      Just . single . forceBlock <$> transformImage image caption
    -- An code block followed by an explicit caption paragraph.
    filterPairs (code@CodeBlock {}, Para (Str "Caption:" : caption)) =
      Just . single <$> transformCodeBlock code caption
    -- Any number of consecutive images in a masonry row.
    filterPairs (LineBlock lines, Para (Str "Caption:" : caption))
      | oneImagePerLine lines =
        Just . single <$> transformImages (concat lines) caption
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Block, Block, Block) -> Filter (Maybe [Block])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Filters lists of Inlines that can match in pairs or triplets
mediaInlineListFilter :: [Inline] -> Filter [Inline]
mediaInlineListFilter inlines =
  tripletwise filterTriplets inlines >>= pairwise filterPairs
  where
    filterPairs :: (Inline, Inline) -> Filter (Maybe [Inline])
    -- Default filter
    filterPairs (x, y) = return Nothing
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
-- A solitary image in a paragraph with a possible caption.
mediaBlockFilter (Para [image@(Image _ caption _)]) =
  forceBlock <$> transformImage image caption
-- A solitary code block in a paragraph with a possible caption.
mediaBlockFilter code@CodeBlock {} = transformCodeBlock code []
-- Any number of consecutive images in a masonry row.
mediaBlockFilter (LineBlock lines)
  | oneImagePerLine lines = transformImages (concat lines) []
-- Default filter
mediaBlockFilter block = return block

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
-- An inline image with a possible caption.
mediaInlineFilter image@(Image _ caption _) =
  transformImage image caption
-- Default filter
mediaInlineFilter inline = return inline

-- | Runs a filter on a Pandoc document. The options are used to rewrite document
-- fragments to HTML or back to Markdown. The meta data may be transformed by
-- the filter. The filter runs in the Filter monad and has access to options
-- and meta data via `gets` and `puts`.
runFilter ::
  Walkable a Pandoc =>
  Disposition ->
  WriterOptions ->
  (a -> Filter a) ->
  Pandoc ->
  IO Pandoc
runFilter dispo options filter pandoc@(Pandoc meta _) = do
  (Pandoc _ blocks, FilterState _ meta dispo) <-
    runStateT (walkM filter pandoc) (FilterState options meta dispo)
  return $ Pandoc meta blocks
