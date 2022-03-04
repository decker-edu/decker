{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Incremental where

import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc.Definition
import Text.Pandoc.Walk

-- | If a slide has class `incremental` all top level block elements that
-- support it are tagged as `fragment`. Also, the slide content is recursively
-- scanned for incremental containers.
incrementalBlocks :: Slide -> Decker Slide
incrementalBlocks slide@(Slide header body dir) = do
  return $
    if hasClass "incremental" slide
      then Slide header (map (setClass "fragment") body) dir
      else Slide header body dir

-- | List singletons inside containers that are tagged `incremental` or inside
-- blockquotes are fragmentized recursively.
incrementalLists :: Block -> Block
incrementalLists (Div attr@(id, cls, kvs) blocks)
  | "incremental" `elem` cls =
    Div attr (map (walk incrementalLists') blocks)
incrementalLists (BlockQuote [block]) =
  BlockQuote [walk incrementalLists' block]
incrementalLists block = block

-- | Fragmentizes list content by wrapping in appropriately tagged containers.
incrementalLists' :: Block -> Block
incrementalLists' (OrderedList attribs items) =
  OrderedList attribs (incrementalItems items)
incrementalLists' list@(BulletList items) =
  BulletList (incrementalItems items)
incrementalLists' (DefinitionList terms) = DefinitionList (incrementalTerms terms)
incrementalLists' block = block

-- | Wraps list items.
incrementalItems :: [[Block]] -> [[Block]]
incrementalItems = map ((: []) . Div ("", ["FUCK", "fragment"], []))

-- | Wraps terms and defintions.
incrementalTerms :: [([Inline], [[Block]])] -> [([Inline], [[Block]])]
incrementalTerms =
  map
    ( \(term, definitions) ->
        ( [Span ("", ["fragment"], []) term],
          incrementalItems definitions
        )
    )