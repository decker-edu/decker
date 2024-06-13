{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Util where

import Data.Digest.Pure.MD5 (md5)
import Data.List (partition)
import Data.Text qualified as Text
import GHC.IO (unsafePerformIO)
import Relude
import System.Random (randomIO, newStdGen, Random (random))
import Text.Pandoc.Definition

-- | adds a given String to the list if not in there; Does nothing if the
--   given String is already present.
addToAtt :: (Eq a) => a -> [a] -> [a]
addToAtt toAdd (a : as)
  | a == toAdd = toAdd : as
  | otherwise = a : addToAtt toAdd as
addToAtt toAdd [] = [toAdd]

-- | adds given String to List of key-value-pairs (like in 'Attr')
--   in the \"style\"-Key.
--
--   Useful when trying to add CSS-styles directly to (generated) elements
addToStyle :: Text.Text -> [(Text.Text, Text.Text)] -> [(Text.Text, Text.Text)]
-- we are looking for style and inject
addToStyle toAdd (("style", val) : as) =
  ( "style",
    if toAdd `Text.isInfixOf` val
      then val
      else Text.concat [val, " ", toAdd]
  )
    : as
-- if we land here the current one is not style -> skip
addToStyle toAdd (a : as) = a : addToStyle toAdd as
-- if we land here we have no more to skip -> add
addToStyle toAdd [] = [("style", toAdd)]

-- | converts Attributes to String for usage in HTML
--
-- Also converts @width=xxx@ and @height=xxx@ to the
-- corresponding style-attributes
attToString :: Attr -> Text.Text
attToString ("", classes, kvpairs) =
  Text.concat
    [ "class=\"",
      Text.unwords classes,
      "\" ",
      Text.unwords ((\(k, v) -> Text.concat [k, "=\"", v, "\""]) <$> kvpairs')
    ]
  where
    kvpairs' = convertToStyle ["width", "height", "transform"] kvpairs
attToString (id', classes, kvpairs) =
  Text.concat
    [ "id=\"",
      id',
      "\" class=\"",
      Text.unwords classes,
      "\" ",
      Text.unwords ((\(k, v) -> Text.concat [k, "=\"", v, "\""]) <$> kvpairs')
    ]
  where
    kvpairs' = convertToStyle ["width", "height", "transform"] kvpairs

-- | helper function for 'attToString', but can also be used
--   if you want to extract styles from kv-pair
convertToStyle ::
  [Text.Text] -> [(Text.Text, Text.Text)] -> [(Text.Text, Text.Text)]
convertToStyle keys kvpairs = ("style", newstyle) : rest
  where
    oldstyle =
      case filter (\(k, _) -> k == "style") kvpairs of
        [(_, st)] -> st
        _ -> ""
    stylesToAdd = filter (\(k, _) -> k `elem` keys) kvpairs
    rest = filter (\(k, _) -> k `notElem` ("style" : keys)) kvpairs
    newstyle =
      Text.concat
        [ Text.concat
            $ map (\(k, v) -> Text.concat [k, ":", v, ";"]) stylesToAdd,
          oldstyle
        ]

-- | revealjs has some special attributes that has to be
--   passed to the html, but Pandoc only allows
--   @key=value@-attributes, so we have to abuse
--   @.class@ to rewrite them.
--
--   The classes that get rewritten are listed here.
--
--   You probably want 'classToRevealAttr', as that
--   is a wrapper for splitting the class-attribute
revealjsSpecialAttrs :: [Text.Text]
revealjsSpecialAttrs =
  [ "data-markdown",
    "data-timing",
    "data-template",
    "data-autoplay",
    "data-prevent-swipe",
    "data-background-interactive",
    "data-trim",
    "data-noescape",
    "data-ignore",
    "controls",
    "loop",
    "muted"
  ]

-- | revealjs has some special attributes that has to be
--   passed to the html, but Pandoc only allows
--   @key=value@-attributes, so we have to abuse
--   @.class@ to rewrite them.
--
--   This is a wrapper-function which just splits the list
--   into real classes and 'revealjsSpecialAttrs'
classToRevealAttr :: [Text.Text] -> ([Text.Text], [Text.Text])
classToRevealAttr = partition (`elem` revealjsSpecialAttrs)

-- | HTML allows for some attributes (i.e. autoplay)
--   for which revealjs offers a special version
--   (i.e. only autoplaying on active slide).
--   These are the things that get rewritten
revealjsRewriteAttr :: [Text.Text] -> [Text.Text]
revealjsRewriteAttr = fmap replace
  where
    replace :: Text.Text -> Text.Text
    replace a =
      case filter ((== a) . fst) replacements of
        [(_, b)] -> b
        _ -> a
    replacements :: [(Text.Text, Text.Text)]
    replacements = [("autoplay", "data-autoplay")]

-- | small wrapper around @RawInline (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toHtml :: Text.Text -> Inline
toHtml = RawInline (Format "html")

-- | small wrapper around @Raw (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toBlockHtml :: Text.Text -> Block
toBlockHtml = RawBlock (Format "html")

forceBlock :: Inline -> Block
forceBlock (RawInline format html) = RawBlock format html
forceBlock inline = Plain [inline]

oneImagePerLine :: [[Inline]] -> Bool
oneImagePerLine inlines = not (null inlines) && not (any null inlines) && all (all isImage) inlines

unpackImage :: Inline -> (Attr, [Inline], Text, Text)
unpackImage (Image attr alt (url, title)) = (attr, alt, url, title)
unpackImage _ = error "Inline is not an Image. oneImagePerLine seems to have failed."

isImage Image {} = True
isImage _ = False

hash9String :: String -> String
hash9String text = take 9 $ show $ md5 $ encodeUtf8 text

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

randomId :: IO Text
randomId = Text.pack . take 9 . show . md5 . show <$> (randomIO :: IO Int)

{-# NOINLINE id9 #-}
id9 :: Text
id9 = unsafePerformIO randomId

single :: a -> [a]
single x = [x]

toId :: Int -> Text
toId = Text.pack . take 9 . show . md5 . show

randomIds :: IO [Text]
randomIds = newStdGen <&> unfoldr (Just . first toId . random)

