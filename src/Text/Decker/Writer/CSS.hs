{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Writer.CSS
  ( computeCssColorVariables,
    computeCssVariables,
  )
where

import Data.Colour
import Data.Colour.SRGB
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Relude
import Text.Decker.Internal.Meta
import Text.Pandoc (Meta)
import Text.Printf

defaultPalette :: [String]
defaultPalette =
  [ "ffffff",
    "dddddd",
    "bbbbbb",
    "999999",
    "777777",
    "555555",
    "333333",
    "000000",
    "e6261f",
    "eb7532",
    "f7d038",
    "a3e048",
    "49da9a",
    "34bbe6",
    "4355db",
    "d23be7"
  ]

-- | Tries to read the 16 value color palette from the meta data and computes
-- derived CSS variable names and values. Uses the philosophy and actual
-- palletes from https://github.com/chriskempson/base16.
--
-- Colors are read and written as hexadecimal strings with aleading `#`. The
-- resulting map of variable names and color values is added back to the meta
-- data. If there is no palette in meta, default values are used.
--
-- From https://github.com/chriskempson/base16/blob/master/styling.md:
--
-- "Colors base00 to base07 are typically variations of a shade and run from
-- darkest to lightest. These colors are used for foreground and background,
-- status bars, line highlighting and such. Colors base08 to base0F are
-- typically individual colors used for types, operators, names and variables.
-- In order to create a dark theme, colors base00 to base07 should span from
-- dark to light. For a light theme, these colours should span from light to
-- dark."
computeCssColorVariables :: Meta -> Meta
computeCssColorVariables meta =
  let palette = lookupMetaOrElse defaultPalette "palette.colors" meta
      contrast :: Float = lookupMetaOrElse 0.25 "palette.contrast" meta
      colors = map sRGB24read palette
      bg = fromJust $ colors !!? 0
      fg = fromJust $ colors !!? 7
      name i post =
        let pre = if i < 7 then "shade" else "accent"
            base = (pre <> show (i `mod` 8))
         in if Text.null post then base else base <> "-" <> post
      deriveShades colors i color =
        let c = toHex color
         in Map.union colors $
              Map.fromList
                [ (printfT "base%0.2X" i, c),
                  (name i "", c)
                ]
      deriveAccents colors i color =
        let cBbg = toHex (blend (2 * contrast) bg color)
            cBg = toHex (blend contrast bg color)
            c = toHex color
            cFg = toHex (blend contrast fg color)
            cFfg = toHex (blend (2 * contrast) fg color)
         in Map.union colors $
              Map.fromList
                [ (printfT "base%0.2X-bbg" i, cBbg),
                  (printfT "base%0.2X-bg" i, cBg),
                  (printfT "base%0.2X" i, c),
                  (printfT "base%0.2X-fg" i, cFg),
                  (printfT "base%0.2X-ffg" i, cFfg),
                  (name i "bbg", cBbg),
                  (name i "bg", cBg),
                  (name i "", c),
                  (name i "fg", cFg),
                  (name i "ffg", cFfg)
                ]
      shades :: Map Text Text = (foldi deriveShades Map.empty (0 :: Int) $ take 8 colors)
      accents :: Map Text Text = foldi deriveAccents Map.empty (8 :: Int) $ drop 8 colors
      existing :: Map Text Text = lookupMetaOrElse Map.empty "css-colors" meta
      -- Map.union ist left-biased. Does not overwrite colors that have been set
      -- by other means.
      cssColors = foldl' Map.union Map.empty [existing, shades, accents]
      cssColorDeclarations = toDeclarations cssColors
   in setMetaValue "css-color-declarations" cssColorDeclarations $ setMetaValue "css-colors" cssColors meta

computeCssVariables :: Meta -> Meta
computeCssVariables meta =
  let variables :: Map Text Text = lookupMetaOrElse Map.empty "css-variables" meta
      declarations = toDeclarations variables
   in setMetaValue "css-declarations" declarations meta

-- | Folds strictly left passing the element count to the folding function.
foldi :: (Foldable t1, Num b) => (a -> b -> t2 -> a) -> a -> b -> t1 t2 -> a
foldi f a d l = fst $ foldl' (\(a, i) b -> (f a i b, i + 1)) (a, d) l

toHex :: Colour Float -> Text
toHex = toText . sRGB24show

printfT :: String -> Int -> Text
printfT f i = toText (printf f i :: String)

toDeclarations :: Map Text Text -> [Text]
toDeclarations =
  Map.foldlWithKey'
    (\variables name value -> toText (printf "--%s: %s;" name value :: String) : variables)
    []
