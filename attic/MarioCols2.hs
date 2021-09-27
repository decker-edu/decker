{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Conversion of lvl-x-headings to x-column-layouts in HTML
--   especially for use in revealjs-slides
module Text.Decker.Filter.MarioCols2
  ( marioCols2,
  )
where

import qualified Data.Text as Text
import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc.JSON

-- | This filter makes multi-column-layouts out of lvl-x-headings
--
-- Syntax is
--
-- @
--     ## a b
-- @
--
-- yielding a 2-column-layout with aspects a:b i.e. 1:1 for 50/50-layout
-- or 8:2 for 80/20 layout
--
-- currently works for 2 and 3-columns, but extension is straight-forward.
--
-- If you need multiple Block-Elements inside one column, just wrap them
-- with a @\<div\>@:
--
-- @
--     ## 2 5
--
--     \<div\>
--     multiple things
--     ```
--     foo
--     ```
--     ![image](...)
--     \</div\>
--
--     second column here with only 1 element.
-- @
cols :: [Block] -> [Block]
cols (Header 2 attr [Str wa, Space, Str wb] : a : b : rest)
  | isInt wa && isInt wb =
    outerDiv : [Div ("", ["layout"], []) [Div ("", ["area"], []) rest]]
  where
    wa' = fromMaybe 1 (readMaybe $ Text.unpack wa) :: Int
    wb' = fromMaybe 1 (readMaybe $ Text.unpack wb) :: Int
    total = wa' + wb'
    pa = (100 * wa') `div` total
    pb = (100 * wb') `div` total
    outerDiv = Div (addClass "layout" attr) [makeDiv pa a, makeDiv pb b, clearDiv]
cols (Header 3 attr [Str wa, Space, Str wb, Space, Str wc] : a : b : c : rest) =
  outerDiv : rest
  where
    wa' = fromMaybe 1 (readMaybe $ Text.unpack wa) :: Int
    wb' = fromMaybe 1 (readMaybe $ Text.unpack wb) :: Int
    wc' = fromMaybe 1 (readMaybe $ Text.unpack wc) :: Int
    total = wa' + wb' + wc'
    pa = (100 * wa') `div` total
    pb = (100 * wb') `div` total
    pc = (100 * wc') `div` total
    outerDiv = Div (addClass "layout" attr) [makeDiv pa a, makeDiv pb b, makeDiv pc c, clearDiv]
cols x = x

addClass :: Text -> (Text, [Text], [(Text, Text)]) -> (Text, [Text], [(Text, Text)])
addClass c (id, cls, kvs) = (id, c : cls, kvs)

makeDiv :: Int -> Block -> Block
makeDiv width content =
  Div
    ( "",
      ["area"],
      [("style", "width:" <> (Text.pack $ show width) <> "%;float:left")]
    )
    [content]

clearDiv :: Block
clearDiv = Div ("", [], [("style", "clear: both")]) [Plain [toHtml ""]]

toHtml :: Text.Text -> Inline
toHtml = RawInline (Format "html")

isInt :: Text.Text -> Bool
isInt str = isJust (readMaybe $ Text.unpack str :: Maybe Int)

marioCols2 :: Slide -> Decker Slide
marioCols2 (Slide header blocks dir) = return (Slide header (cols blocks) dir)
