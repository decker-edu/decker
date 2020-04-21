{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attrib where

import Text.Decker.Filter.Monad

import Text.Decker.Internal.Meta

import Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Relude
import Text.Pandoc

-- | An associative list representing element attributes.
type AttrMap = [(Text, Text)]

-- | The first set contains the transformed attributes that will be extracted
-- and added to the HTML elements. The second set contains the source
-- attributes as parsed from the Markdown attribute markup. Many functions
-- inside the Attrib monad manipulate one or both attribute sets. 
type AttribState = (Attr, Attr)

-- | The Attrib monad.
type Attrib = StateT AttribState Filter

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

-- | Adds a CSS style value pair to the attribute map. If a style attribute
-- does not yet exist, it is created. The value of an existing style attribute 
-- is ammended on the left. 
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

injectAttribute :: (Text, Text) -> Attrib ()
injectAttribute (k, v) = modify transform
  where
    transform ((id', cs', kvs'), attr) =
      ((id', cs', (k, v) : filter ((/= k) . fst) kvs'), attr)

-- | Pushes an additional attribute to the source side of the attribute state.
-- An existing attribute with the same key is overwritten.
pushAttribute :: (Text, Text) -> Attrib ()
pushAttribute (key, value) = modify transform
  where
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, alterKey replace key kvs))
    replace _ = Just value

-- | Removes the attribute key from the source attribute map and adds it as a
-- CSS style value to the target style attribute. Mainly used to translate
-- witdth an height attributes into CSS style setting.
takeStyle :: Text -> Attrib ()
takeStyle = takeStyleIf (const True)

-- | Transfers an attribute to the targets if it exists and the value satisfies
-- the predicate.
takeStyleIf :: (Text -> Bool) -> Text -> Attrib ()
takeStyleIf p key = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      case List.lookup key kvs of
        Just value
          | p value ->
            ((id', cs', addStyle (key, value) kvs'), (id, cs, rmKey key kvs))
        _ -> state

-- | Translates width and height attributes into CSS style values if they
-- exist.
takeSizeIf :: (Text -> Bool) -> Attrib ()
takeSizeIf p = do
  takeStyleIf p "width"
  takeStyleIf p "height"

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
coreAttribs = ["id", "class", "title", "style"]

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
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, filter ((`notElem` coreAttribs) . fst) kvs))

passI18n :: Attrib ()
passI18n = modify transform
  where
    transform ((id', cs', kvs'), (id, cs, kvs)) =
      let (pass, rest) = List.partition ((`elem` ["dir", "xml:lang"]) . fst) kvs
       in ((id', cs', kvs' <> pass), (id, cs, rest))

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

injectBorder = do
  border <- getMetaBoolOrElse "decker.filter.border" False <$> lift (gets meta)
  when border $ injectStyle ("border", "2px solid magenta")

takeVideoClasses :: Attrib ()
takeVideoClasses = takeClasses identity ["controls", "loop", "muted"]

takeClasses :: (Text -> Text) -> [Text] -> Attrib ()
takeClasses f classes = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vcs, rest) = List.partition (`elem` classes) cs
       in ((id', cs', map ((, "1") . f) vcs <> kvs'), (id, rest, kvs))

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

takeAutoplay :: Attrib ()
takeAutoplay = do
  (id, cs, kvs) <- snd <$> get
  when ("autoplay" `elem` cs || "autoplay" `elem` map fst kvs) $ do
    dropClass "autoplay"
    dropAttribute "autoplay"
    injectAttribute ("data-autoplay", "1")

takeUsual = do
  takeId
  takeAllClasses
  takeCss
  dropCore
  passI18n
  takeData
