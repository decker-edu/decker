{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Div (divBasedLayout) where

import Data.List (partition)
import Data.List.Extra (notNull, splitOn)
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
  return (Slide header (processBody body) dir)
  where
    processBody = map (processColumns . processSize)
    -- Finds the first columns tag and remove it from the classes list. Recurses on contained Div elements.
    processColumns div@(Div (id, cls, attribs) body) =
      let columnClasses = partition (=~ ("^columns(-[0-9]+)+$" :: String)) cls
       in case first (find (not . Text.null)) columnClasses of
            (Just columnClass, cls) ->
              -- Extracts the numbers from the columns tag, default is 1
              let columRatios = map (fromMaybe 1 . readMaybe) $ drop 1 $ splitOn "-" $ toString columnClass
                  gridClasses = "grid-layout" : cls
                  -- Construct the CSS style
                  gridCss = Text.intercalate " " $ "grid-template-columns:" : map ((<> "fr") . show) columRatios
               in Div 
                    (id, gridClasses, addToStyle [gridCss] attribs) 
                    (processBody body)
            _ -> let (Div attrs body) = div 
                 in Div attrs (processBody body)
    processColumns block = block
    -- Converts size attributes (currently with, height) to CSS style settings
    processSize div@(Div (id, cls, attribs) body) =
      let (sizeAttribs, otherAttribs) = partition ((`elem` ["width", "height"]) . fst) attribs
          sizesCss = map (\(k, v) -> k <> ":" <> v) sizeAttribs
       in if notNull sizesCss
            then Div (id, "sized" : cls, addToStyle sizesCss otherAttribs) body
            else div
    processSize block = block
    -- Adds a singe CSS style in front of the style attribute. Collapses style
    -- attributes if there are more than just one and preserves already existing
    -- style settings.
    addToStyle css attribs =
      let (styleAttribs, otherAttribs) = partition ((== "style") . fst) attribs
          styleCss =
            concatMap
              (filter (not . Text.null) . Text.splitOn ";" . snd)
              styleAttribs
          newStyles = (<> ";") $ Text.intercalate ";" $ css <> styleCss
       in ("style", newStyles) : otherAttribs
