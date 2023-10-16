{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attrib where

import Data.Bifunctor
import Data.List (nub, partition)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Relude
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import qualified Text.URI as URI

-- | An associative list representing element attributes.
type AttrMap = [(Text, Text)]

-- |  The first set contains the transformed attributes that will be extracted
--  and added to the HTML elements. The second set contains the source
--  attributes as parsed from the Markdown attribute markup. Many functions
--  inside the Attrib monad manipulate one or both attribute sets.
type AttribState = (Attr, Attr)

-- | The Attrib monad.
type Attrib = StateT AttribState Filter

-- Source attributes potentially used on images and code blocks.
srcAttribs :: [Text]
srcAttribs = ["src", "data-src", "poster", "image"]

-- Source attributes used on header blocks by reveal.js.
bgAttribs :: [Text]
bgAttribs =
  ["data-background-image", "data-background-video", "data-background-iframe"]

runAttr :: Attr -> Attrib a -> Filter a
runAttr attr attrAction = evalStateT attrAction (nullAttr, attr)

runAttrOn :: Attr -> Attr -> Attrib a -> Filter a
runAttrOn init attr attrAction = evalStateT attrAction (init, attr)

filterAttr :: Attr -> Attrib Attr -> Filter Attr
filterAttr attr attrAction = evalStateT attrAction (nullAttr, attr)

augmentAttr :: Attr -> Attr -> Attrib Attr -> Filter Attr
augmentAttr init attr attrAction = evalStateT attrAction (init, attr)

src :: Attrib Attr
src = do
  (_, s) <- get
  return s

dst :: Attrib Attr
dst = do
  (d, _) <- get
  return d

srcAttributes :: Attrib [(Text, Text)]
srcAttributes = do
  (_, (_, _, kvs)) <- get
  return kvs

srcAttribute :: Text -> Attrib (Maybe Text)
srcAttribute key = do
  (_, (_, _, kvs)) <- get
  return $ fst <$> find ((==) key . fst) kvs

-- | Extracts the attributes that have been transformed so far. The attributes
-- are removed.
extractAttr :: Attrib Attr
extractAttr = do
  (result, remaining) <- get
  put (nullAttr, remaining)
  return result

-- | Removes all values associated with key from the list.
rmKey :: Eq a => a -> [(a, b)] -> [(a, b)]
rmKey key = filter ((/= key) . fst)

-- | Removes all instaces of value from the list.
rmClass :: Text -> [Text] -> [Text]
rmClass cls = filter (/= cls)

-- | Merges associative lists with left bias.
merge :: Ord a => [[(a, b)]] -> [(a, b)]
merge xs = Map.toList $ Map.unions $ map Map.fromList xs

-- | Alters the value at key. Can be used to add, change, or remove key value
-- pairs from an associative list. (See Data.Map.Strict.alter).
alterKey :: Eq a => (Maybe b -> Maybe b) -> a -> [(a, b)] -> [(a, b)]
alterKey f key kvs =
  case f $ List.lookup key kvs of
    Just value -> (key, value) : rmKey key kvs
    Nothing -> rmKey key kvs

-- |  Adds a CSS style value pair to the attribute map. If a style attribute
--  does not yet exist, it is created. The value of an existing style attribute
--  is ammended on the left.
addStyle :: (Text, Text) -> AttrMap -> AttrMap
addStyle (k, v) = alterKey addOne "style"
  where
    addOne style = Just $ fromMaybe "" style <> k <> ":" <> v <> ";"

-- | Adds a CSS style value pair to the target attributes.
injectStyle :: (Text, Text) -> Attrib ()
injectStyle (key, value) = modify transform
  where
    transform ((id', cs', kvs'), attr) =
      ((id', cs', addStyle (key, value) kvs'), attr)

injectStyles :: [(Text, Text)] -> Attrib ()
injectStyles = mapM_ injectStyle

injectAttribute :: (Text, Text) -> Attrib ()
injectAttribute (k, v) = modify transform
  where
    transform ((id', cs', kvs'), attr) =
      ((id', cs', (k, v) : filter ((/= k) . fst) kvs'), attr)

injectClass :: Text -> Attrib ()
injectClass cls = modify transform
  where
    transform ((id', cs', kvs'), attr) = ((id', cls : cs', kvs'), attr)

injectId :: Text -> Attrib ()
injectId id = modify transform
  where
    transform ((id', cs', kvs'), attr) = ((id, cs', kvs'), attr)

injectClasses :: [Text] -> Attrib ()
injectClasses cs = modify transform
  where
    transform ((id', cs', kvs'), attr) = ((id', cs <> cs', kvs'), attr)

-- | Pushes an additional attribute to the source side of the attribute state.
-- An existing attribute with the same key is overwritten.
pushAttribute :: (Text, Text) -> Attrib ()
pushAttribute (key, value) = modify transform
  where
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, alterKey replace key kvs))
    replace _ = Just value

-- |  Removes the attribute key from the source attribute map and adds it as a
--  CSS style value to the target style attribute. Mainly used to translate
--  witdth and height attributes into CSS style setting.
takeStyle :: Text -> Attrib Bool
takeStyle = takeStyleIf (const True)

-- |  Transfers an attribute to the targets if it exists and the value satisfies
--  the predicate.
takeStyleIf :: (Text -> Bool) -> Text -> Attrib Bool
takeStyleIf p key = do
  (_, _, kvs) <- src
  case List.lookup key kvs of
    Just value | p value -> modify (move value) >> return True
    _ -> return False
  where
    move value state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs', addStyle (key, value) kvs'), (id, cs, rmKey key kvs))

-- | Translates width and height attributes into CSS style values if they
-- exist.
takeSizeIf :: (Text -> Bool) -> Attrib Bool
takeSizeIf p = do
  w <- takeStyleIf p "width"
  h <- takeStyleIf p "height"
  return (w || h)

takeSize :: Attrib Bool
takeSize = takeSizeIf (const True)

takeId :: Attrib ()
takeId = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id, cs', kvs'), ("", cs, kvs))

takeCss :: Attrib ()
takeCss = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (css, rest) = List.partition (isCss . fst) kvs
          added =
            foldl'
              (\result (k, v) -> addStyle (Text.drop 4 k, v) result)
              kvs'
              css
       in ((id', cs', added), (id, cs, rest))
    isCss key = Text.isPrefixOf "css:" key && Text.length key > 4

coreAttribs :: [Text]
coreAttribs = ["id", "class", "style"]

dropClass :: Text -> Attrib ()
dropClass key = modify transform
  where
    transform (attr', (id, cs, kvs)) = (attr', (id, rmClass key cs, kvs))

dropAttribute :: Text -> Attrib ()
dropAttribute key = modify transform
  where
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, filter ((/= key) . fst) kvs))

dropCore :: Attrib ()
dropCore = modify transform
  where
    transform (attr', (id, cs, kvs)) = (attr', (id, cs, filter ((`notElem` coreAttribs) . fst) kvs))

passI18n :: Attrib ()
passI18n = modify transform
  where
    transform ((id', cs', kvs'), (id, cs, kvs)) =
      let (pass, rest) = List.partition ((`elem` ["dir", "xml:lang"]) . fst) kvs
       in ((id', cs', kvs' <> pass), (id, cs, rest))

takeData' :: Attrib ()
takeData' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (dta, ndta) = partition (Text.isPrefixOf "data-" . fst) kvs
          adta = dta <> map (first ("data-" <>)) ndta
       in ((id', cs', adta <> kvs'), (id, cs, []))

takeData :: Attrib ()
takeData = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs', map (first ("data-" <>)) kvs <> kvs'), (id, cs, []))

takeAllClasses :: Attrib ()
takeAllClasses = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs <> cs', kvs'), (id, [], kvs))

takeAttributes :: [Text] -> Attrib ()
takeAttributes keys = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (match, rest) = List.partition ((`elem` keys) . fst) kvs
       in ((id', cs', nub (kvs' <> match)), (id, cs, rest))

takeAllAttributes :: Attrib ()
takeAllAttributes = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', nub (cs <> cs'), nub (kvs' <> kvs)), (id, [], kvs))

injectBorder :: Attrib ()
injectBorder = do
  border <- lookupMetaOrElse False "decker.filter.border" <$> lift (gets meta)
  when border $ injectStyle ("border", "2px solid magenta")

updateStreaming :: Attrib ()
updateStreaming = do
  dropAttribute "start"
  injectClass "streaming"

takeVideoClasses :: Attrib ()
takeVideoClasses = takeClasses identity ["controls", "loop", "muted"]

takeClasses :: (Text -> Text) -> [Text] -> Attrib ()
takeClasses f classes = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vcs, rest) = List.partition (`elem` classes) cs
       in ((id', cs', map ((,"1") . f) vcs <> kvs'), (id, rest, kvs))

cutAttrib :: Text -> Attrib (Maybe Text)
cutAttrib key = do
  (attr', (id, cs, kvs)) <- get
  let (vkvs, rkvs) = List.partition ((== key) . fst) kvs
  put (attr', (id, cs, rkvs))
  return $ snd <$> listToMaybe vkvs

cutAttribs :: [Text] -> Attrib [(Text, Text)]
cutAttribs keys = do
  (attr', (id, cs, kvs)) <- get
  let (vkvs, rkvs) = List.partition ((`elem` keys) . fst) kvs
  put (attr', (id, cs, rkvs))
  return vkvs

cutClass :: Text -> Attrib [Text]
cutClass cls = do
  (attr', (id, cs, kvs)) <- get
  let (vcs, rcs) = List.partition (== cls) cs
  put (attr', (id, rcs, kvs))
  return vcs

cutClasses :: [Text] -> Attrib [Text]
cutClasses classes = do
  (attr', (id, cs, kvs)) <- get
  let (vcs, rcs) = List.partition (`elem` classes) cs
  put (attr', (id, rcs, kvs))
  return vcs

passVideoAttribs :: Attrib ()
passVideoAttribs =
  passAttribs identity ["controls", "loop", "muted", "poster", "preload"]

passAttribs :: (Text -> Text) -> [Text] -> Attrib ()
passAttribs f keys = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vkvs, rest) = List.partition ((`elem` keys) . fst) kvs
       in ((id', cs', map (first f) vkvs <> kvs'), (id, cs, rest))

xformRersourceAttribs :: [Text] -> Attrib ()
xformRersourceAttribs keys = do
  (res, (id, cs, kvs)) <- get
  local <- lift $ adjustAttribPaths keys kvs
  put (res, (id, cs, local))

takeAutoplay :: Attrib ()
takeAutoplay = do
  (id, cs, kvs) <- snd <$> get
  when ("autoplay" `elem` cs || "autoplay" `elem` map fst kvs) $ do
    dropClass "autoplay"
    dropAttribute "autoplay"
    injectAttribute ("data-autoplay", "1")
    injectAttribute ("allow", "autoplay")

takeUsual :: Attrib ()
takeUsual = do
  takeId
  takeAllClasses
  takeCss
  passI18n
  takeAttributes ["style"]
  dropCore
  takeData

-- Adjusts the values of all path values that are listed in keys.
adjustAttribPaths :: [Text] -> [(Text, Text)] -> Filter [(Text, Text)]
adjustAttribPaths keys kvs = do
  let (paths, other) = List.partition ((`elem` keys) . fst) kvs
  local <- mapM adjustAttrib paths
  return $ local <> other
  where
    adjustAttrib :: (Text, Text) -> Filter (Text, Text)
    adjustAttrib (k, v) = do
      (k,) <$> (URI.render <$> transformUrl v "")

-- Adjusts the values of all path values that are listed in keys.
adjustAttribPaths' :: [Text] -> Attrib ()
adjustAttribPaths' keys = do
  paths <- cutAttribs keys
  local <- lift $ mapM adjustAttrib paths
  mapM_ injectAttribute local
  where
    adjustAttrib :: (Text, Text) -> Filter (Text, Text)
    adjustAttrib (k, v) = do
      (k,) <$> (URI.render <$> transformUrl v "")

isPercent :: Text -> Bool
isPercent = Text.isSuffixOf "%"

ifAttrib :: Text -> (Text -> Attrib ()) -> Attrib ()
ifAttrib key action =
  cutAttrib key >>= mapM_ action

ifClass :: [Text] -> Attrib () -> Attrib ()
ifClass keys action = do
  (_, cls, _) <- src
  when (any (`elem` cls) keys) action

mediaFragment :: Attrib Text
mediaFragment = do
  (result, (id, cs, kvs)) <- get
  let start = fromMaybe "" $ List.lookup "start" kvs
      stop = fromMaybe "" $ List.lookup "stop" kvs
  put (result, (id, cs, rmKey "start" $ rmKey "stop" kvs))
  return $
    if Text.null start && Text.null stop
      then ""
      else "t=" <> start <> "," <> stop

addClass :: Text -> Attr -> Attr
addClass c (id, cs, kvs) = (id, List.nub (c : cs), kvs)

addClasses :: [Text] -> Attr -> Attr
addClasses cls (id, cs, kvs) = (id, List.nub (cls <> cs), kvs)

--- |                         | caption   | aria-label |
--- | ----------------------- | --------- | ---------- |
--- | `[caption](url "aria")` | `caption` | `aria`     |
--- | `[caption](url)`        | `caption` | `caption`  |
--- | `[](url "aria")`        |           | `aria`     |
--- | `[](url)`               |           |            |
---
injectAria :: Text -> [Inline] -> Attrib ()
injectAria aria caption = do
  let caption_ = if null caption then Nothing else Just $ stringify caption
  let aria_ = if Text.null aria then Nothing else Just aria
  case (caption_, aria_) of
    (Just caption, Just aria) -> injectAttribute ("aria-label", aria)
    (Just caption, Nothing) -> injectAttribute ("aria-label", caption)
    (Nothing, Just aria) -> injectAttribute ("aria-label", aria)
    (Nothing, Nothing) -> return ()