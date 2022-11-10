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

-- Theme: Tomorrow (http://chriskempson.com/projects/base16/)
defaultLight :: [String]
defaultLight =
  [ "#ffffff",
    "#e0e0e0",
    "#d6d6d6",
    "#8e908c",
    "#969896",
    "#4d4d4c",
    "#282a2e",
    "#1d1f21",
    "#c82829",
    "#f5871f",
    "#eab700",
    "#718c00",
    "#3e999f",
    "#4271ae",
    "#8959a8",
    "#a3685a"
  ]

-- Theme: Tomorrow Night (http://chriskempson.com/projects/base16/)
defaultDark :: [String]
defaultDark =
  [ "#1d1f21",
    "#282a2e",
    "#373b41",
    "#969896",
    "#b4b7b4",
    "#c5c8c6",
    "#e0e0e0",
    "#ffffff",
    "#cc6666",
    "#de935f",
    "#f0c674",
    "#b5bd68",
    "#8abeb7",
    "#81a2be",
    "#b294bb",
    "#a3685a"
  ]

-- | Tries to read the 16 value color palette from the meta data and computes
-- derived CSS variable names and values. Uses the philosophy and actual
-- palletes from https://github.com/chriskempson/base16.
--
-- Colors are read and written as hexadecimal strings with a leading `#`. The
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
  let (paletteLight, paletteDark) =
        case ( lookupMeta "palette.colors.light" meta,
               lookupMeta "palette.colors.dark" meta
             ) of
          (Just light, Just dark) -> (light, dark)
          (Just light, Nothing) -> (light, light)
          _ -> (defaultLight, defaultDark)
      contrast = lookupMetaOrElse 0.25 "palette.contrast" meta :: Float
      existingLight :: Map Text Text = lookupMetaOrElse Map.empty "css-light-colors" meta
      existingDark :: Map Text Text = lookupMetaOrElse Map.empty "css-dark-colors" meta
      (cssLightColors, cssLightColorDeclarations) = deriveColors paletteLight contrast existingLight
      (cssDarkColors, cssDarkColorDeclarations) = deriveColors paletteDark contrast existingDark
   in -- Set the CSS variable declarations and make sure we have a palette, even
      -- if it is just the default colors.
      setMetaValue "css-light-color-declarations" cssLightColorDeclarations $
        setMetaValue "css-light-colors" cssLightColors $
          setMetaValue "palette.colors.light" paletteLight $
            setMetaValue "css-dark-color-declarations" cssDarkColorDeclarations $
              setMetaValue "css-dark-colors" cssDarkColors $
                setMetaValue "palette.colors.dark" paletteDark $
                  setMetaValue "palette.contrast" (show contrast :: Text) meta

deriveColors :: [String] -> Float -> Map Text Text -> (Map Text Text, [Text])
deriveColors palette contrast existing =
  let colors = map sRGB24read palette
      nShades = length palette `div` 2
      bg = fromJust $ colors !!? 0
      fg = fromJust $ colors !!? (nShades - 1)
      name i post =
        let pre = if i < nShades then "shade" else "accent"
            base = (pre <> show (if i < nShades then i else i - nShades))
         in if Text.null post then base else base <> "-" <> post
      deriveShades colors i color =
        let c = toHex color
         in Map.union colors $
              Map.fromList
                [ (printfT "base%0.2X" i, c),
                  (printfT "shade%d" i, c)
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
                  (printfT "accent%d-bbg" i, cBbg),
                  (printfT "accent%d-bg" i, cBg),
                  (printfT "accent%d" i, c),
                  (printfT "accent%d-fg" i, cFg),
                  (printfT "accent%d-ffg" i, cFfg)
                ]
      shades :: Map Text Text = foldi deriveShades Map.empty (0 :: Int) $ take nShades colors
      accents :: Map Text Text = foldi deriveAccents Map.empty (0 :: Int) $ drop nShades colors
      -- Map.union ist left-biased. Does not overwrite colors that have been set
      -- by other means.
      cssColors = foldl' Map.union Map.empty [existing, shades, accents]
      cssColorDeclarations = toDeclarations cssColors
   in (cssColors, cssColorDeclarations)

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
