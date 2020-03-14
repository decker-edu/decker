{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Attrib where

import Text.Decker.Filter.Monad

import Text.Decker.Internal.Meta

import qualified Data.List as List
import qualified Data.Text as Text
import Relude
import Text.Pandoc

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
injectAttribute kv = modify transform
  where
    transform ((id', cs', kvs'), attr) = ((id', cs', kv : kvs'), attr)

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
takeStyle key = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      case List.lookup key kvs of
        Just value ->
          ((id', cs', addStyle (key, value) kvs'), (id, cs, rmKey key kvs))
        Nothing -> state

-- | Translates width and height attributes into CSS style values if they
-- exist.
takeSize :: Attrib ()
takeSize = do
  takeStyle "width"
  takeStyle "height"

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

videoClasses = ["controls", "loop", "muted"]

takeVideoClasses :: Attrib ()
takeVideoClasses = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vcs, rest) = List.partition (`elem` videoClasses) cs
       in ((id', cs', map (, "1") vcs <> kvs'), (id, rest, kvs))

videoAttribs = ["controls", "loop", "muted", "poster", "preload"]

passVideoAttribs :: Attrib ()
passVideoAttribs = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vkvs, rest) = List.partition ((`elem` videoAttribs) . fst) kvs
       in ((id', cs', vkvs <> kvs'), (id, cs, rest))

takeAutoplay :: Attrib ()
takeAutoplay = do
  (id, cs, kvs) <- snd <$> get
  when ("autoplay" `elem` cs || "autoplay" `elem` (map fst kvs)) $ do
    dropClass "autoplay"
    dropAttribute "autoplay"
    injectAttribute ("data-autoplay", "1")
