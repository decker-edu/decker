{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Div (divBasedLayout) where

import Data.List (partition)
import Data.List.Extra (splitOn)
import qualified Data.Text as Text
import Relude
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Pandoc
import Text.Regex.TDFA

-- {.columns-x-y-z} ->  {.grid-layout style="grid-template-columns: xfr yfr zfr;"}
-- {width="x"} -> {.sized style="width:x;"}

divBasedLayout :: Slide -> Decker Slide
divBasedLayout (Slide header body dir) =
  return (Slide header (map (processColumns . processSize) body) dir)
  where
    -- Finds the first columns tag and remove it from the classes list
    processColumns div@(Div (id, cls, kvs) body) =
      let colCls = partition (=~ ("^columns(-[0-9]+)+$" :: String)) cls
       in case first (find (not . Text.null)) colCls of
            (Just cols, cls) ->
              -- Extracts the numbers from the columns tag, default is 1
              let frs = map (fromMaybe 1 . readMaybe) $ drop 1 $ splitOn "-" $ toString cols
                  cls' = ".grid-layout" : cls
                  -- Construct the CSS style
                  gtc = Text.intercalate " " $ "grid-template-columns:" : map ((<> "fr") . show) frs
               in -- Final assembly
                  Div (id, cls', addToStyle kvs [gtc]) body
            _ -> div
    processColumns block = block
    -- Converts size attributes (currently with, height) to CSS style settings
    processSize div@(Div (id, cls, kvs) body) =
      let (sizes, rest) = partition ((`elem` ["width", "height"]) . fst) kvs
          sizesCss = map (\(k, v) -> k <> ":" <> v) sizes
       in Div (id, "sized" : cls, addToStyle rest sizesCss) body
    processSize block = block
    -- Adds a singe CSS style in front of the style attribute. Collapses style
    -- attributes if there are more than just one and preserves already existing
    -- style settings.
    addToStyle kvs css =
      let (sty, rest) = partition ((== "style") . fst) kvs
          sty' =
            (<> ";") $
              Text.intercalate ";" $
                css
                  <> concatMap
                    (filter (not . Text.null) . Text.splitOn ";" . snd)
                    sty
       in ("style", sty') : rest
