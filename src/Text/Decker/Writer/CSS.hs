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
import Relude
import Text.Decker.Internal.Meta
import Text.Pandoc (Meta)
import Text.Printf

-- Solarized light (https://ethanschoonover.com/solarized/)
defaultPalette :: [String]
defaultPalette =
  [ "#fdf6e3",
    "#eee8d5",
    "#93a1a1",
    "#839496",
    "#657b83",
    "#586e75",
    "#073642",
    "#002b36",
    "#dc322f",
    "#cb4b16",
    "#b58900",
    "#859900",
    "#2aa198",
    "#268bd2",
    "#6c71c4",
    "#d33682"
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
      set colors i color = Map.insert (printfT "base%0.2x" i) (toHex color) colors
      derive colors i color =
        Map.union colors $
          Map.fromList
            [ (printfT "base%0.2X-bbg" i, toHex (blend (2 * contrast) bg color)),
              (printfT "base%0.2X-bg" i, toHex (blend contrast bg color)),
              (printfT "base%0.2X" i, toHex color),
              (printfT "base%0.2X-fg" i, toHex (blend contrast fg color)),
              (printfT "base%0.2X-ffg" i, toHex (blend (2 * contrast) fg color))
            ]
      base :: Map Text Text = foldi set Map.empty (0 :: Int) $ colors
      shades :: Map Text Text =
        -- Derivations of fore- and background colors are taken from the adjacent
        -- base shades.
        Map.union
          ( Map.fromList
              [ ("base00-fg", "var(--base01)"),
                ("base00-ffg", "var(--base02)"),
                ("base07-bg", "var(--base06)"),
                ("base07-bbg", "var(--base05)")
              ]
          )
          (foldi set Map.empty (0 :: Int) $ take 8 colors)
      accents :: Map Text Text = foldi derive Map.empty (8 :: Int) $ drop 8 colors
      existing :: Map Text Text = lookupMetaOrElse Map.empty "css-colors" meta
      -- Map.union ist left-biased. Does not overwrite colors that have been set
      -- by other means.
      cssColors = foldl' Map.union Map.empty [existing, base, shades, accents]
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
