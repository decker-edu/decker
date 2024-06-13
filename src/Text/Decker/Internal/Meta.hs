{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Meta
  ( DeckerException (..),
    FromMetaValue (..),
    addMetaValue,
    addMetaKeyValue,
    adjustMetaStringsBelow,
    adjustMetaStringsBelowM,
    adjustMetaValue,
    adjustMetaValueM,
    embedMetaMeta,
    fromPandocMeta,
    globalMetaFileName,
    isMetaSet,
    lookupInDictionary,
    lookupMeta,
    lookupMetaOrElse,
    lookupMetaOrFail,
    mapMeta,
    mapMetaM,
    mapMetaValues,
    mapMetaValuesM,
    mapMetaWithKey,
    mergePandocMeta,
    pandocMeta,
    readMetaDataFile,
    readMetaValue,
    setMetaValue,
    toPandocMeta',
    toPandocMeta,
  )
where

import Control.Exception
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.List.Safe ((!!))
import Data.Map.Lazy qualified as Map
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as Text
import Data.Vector qualified as Vec
import Data.Yaml qualified as Y
import Relude
import Text.Decker.Internal.Exception
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Builder hiding (fromList, lookupMeta, toList)
import Text.Pandoc.Shared hiding (toString, toText)
import Text.Decker.Internal.Common (pandocReaderOpts)

-- | Name of the one global meta data file
globalMetaFileName = "decker.yaml"

-- TODO: extract this value from global meta data.
replaceLists :: [[Text]]
replaceLists = [["math", "macros"], ["palette", "colors"], ["publish", "rsync", "options"]]

shouldMerge :: [Text] -> Bool
-- shouldMerge path = not $ any (`Text.isPrefixOf` Text.intercalate "." path) replaceLists
shouldMerge path = not $ any (`List.isPrefixOf` path) replaceLists

-- | Fine-grained recursive merge of two meta values. Left-biased. Lists are
-- merged by default, but replaced if the key path is in `replaceLists`.
mergePandocMeta :: Meta -> Meta -> Meta
mergePandocMeta (Meta left) (Meta right) =
  case merge [] "" (MetaMap left) (MetaMap right) of
    MetaMap m -> Meta m
    _ -> throw $ InternalException "This cannot happen."
  where
    merge :: [Text] -> Text -> MetaValue -> MetaValue -> MetaValue
    merge path key (MetaMap mapL) (MetaMap mapR) =
      MetaMap $ Map.unionWithKey (merge (concat path key)) mapL mapR
    merge path key (MetaList listL) (MetaList listR)
      | shouldMerge (concat path key) =
          -- MetaList $ Set.toList $ Set.fromList listL <> Set.fromList listR
          -- This should be stable and result in a more predictable order
          MetaList $ List.nub $ listR <> listL
    merge path key left right = left
    concat path "" = path
    concat path key = path <> [key]

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> Meta
toPandocMeta v@(Y.Object m) =
  case toPandocMeta' v of
    (MetaMap map) -> Meta map
    _ -> Meta M.empty
toPandocMeta _ = Meta M.empty

toPandocMeta' :: Y.Value -> MetaValue
toPandocMeta' (Y.Object m) =
  MetaMap $ Map.fromList $ map (bimap Key.toText toPandocMeta') $ KeyMap.toList m
toPandocMeta' (Y.Array vector) =
  MetaList $ map toPandocMeta' $ Vec.toList vector
-- Playing around with #317
toPandocMeta' (Y.String text) = MetaString text
-- toPandocMeta' (Y.String text) = compileText text
toPandocMeta' (Y.Number scientific) = MetaString $ Text.pack $ show scientific
toPandocMeta' (Y.Bool bool) = MetaBool bool
toPandocMeta' Y.Null = MetaList []

_compileText :: Text -> MetaValue
_compileText text =
  case runPure $ readMarkdown pandocReaderOpts text of
    Right pandoc@(Pandoc _ [Plain inlines]) -> MetaInlines inlines
    Right pandoc@(Pandoc _ [Para inlines]) -> MetaInlines inlines
    Right pandoc@(Pandoc _ blocks) -> MetaBlocks blocks
    Left _ -> MetaString text

fromPandocMeta :: Meta -> A.Value
fromPandocMeta (Meta map) = fromPandocMeta' (MetaMap map)

fromPandocMeta' :: MetaValue -> A.Value
fromPandocMeta' (MetaMap map) = A.Object (KeyMap.fromList $ List.map (first Key.fromText) $ Map.toList $ Map.map fromPandocMeta' map)
fromPandocMeta' (MetaList list) = A.Array (Vec.fromList $ List.map fromPandocMeta' list)
fromPandocMeta' (MetaBool value) = A.Bool value
fromPandocMeta' (MetaString value) =
  case readMaybe (toString value) of
    Just number -> A.Number number
    Nothing -> A.String value
fromPandocMeta' (MetaInlines value) = A.String (stringify value)
fromPandocMeta' (MetaBlocks value) = A.Null

-- |  Split a compound meta key at the dots and separate the array indexes.
splitKey = concatMap splitIndex . Text.splitOn "."
  where
    splitIndex key =
      case Text.splitOn "[" key of
        [key, index] ->
          case Text.stripSuffix "]" index of
            Just index -> [key, index]
            _ -> [key]
        _ -> [key]

-- | Recursively deconstruct a compound key and drill into the meta data hierarchy.
getMetaValue :: Text -> Meta -> Maybe MetaValue
getMetaValue key meta = lookup' (splitKey key) (MetaMap (unMeta meta))
  where
    lookup' (key : path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key : path) (MetaList list) =
      (readMaybe . Text.unpack) key >>= (!!) list >>= lookup' path
    lookup' (_ : _) _ = Nothing
    lookup' [] mv = Just mv

-- | Checks if meta key is set to a value.
isMetaSet :: Text -> Meta -> Bool
isMetaSet key meta = isJust $ getMetaValue key meta

-- | Sets a meta value at the compound key in the meta data. If any intermediate
-- containers do not exist, they are created.
setMetaValue :: ToMetaValue a => Text -> a -> Meta -> Meta
setMetaValue key value meta = Meta $ set (splitKey key) (MetaMap (unMeta meta))
  where
    set [k] (MetaMap map) = M.insert k (toMetaValue value) map
    set (k : p) (MetaMap map) =
      case M.lookup k map of
        Just value -> M.insert k (MetaMap $ set p value) map
        _ -> M.insert k (MetaMap $ set p $ MetaMap M.empty) map
    set _ _ =
      throw $
        InternalException $
          "Cannot set meta value on non object at: " <> show key

readMetaValue :: Text -> Text -> Meta -> Meta
readMetaValue key value = setMetaValue key (maybe value show (readMaybe (toString value) :: Maybe Bool))

-- | Recursively deconstruct a compound key and drill into the meta data hierarchy.
-- Apply the function to the value if the key exists.
adjustMetaValue :: (MetaValue -> MetaValue) -> Text -> Meta -> Meta
adjustMetaValue f key meta =
  Meta $ adjust (splitKey key) (MetaMap (unMeta meta))
  where
    adjust :: [Text] -> MetaValue -> Map Text MetaValue
    adjust [k] (MetaMap map) = M.adjust f k map
    adjust (k : p) (MetaMap map) =
      case M.lookup k map of
        Just value -> M.insert k (MetaMap $ adjust p value) map
        _ -> map
    adjust _ _ = Map.empty

-- throw $
--   InternalException $
--     "Cannot adjust meta value on non object at: " <> show key

-- | Recursively deconstruct a compound key and drill into the meta data hierarchy.
-- Apply the IO action to the value if the key exists.
adjustMetaValueM ::
  MonadIO m => (MetaValue -> m MetaValue) -> Text -> Meta -> m Meta
adjustMetaValueM action key meta =
  Meta <$> adjust (splitKey key) (MetaMap (unMeta meta))
  where
    adjust [k] (MetaMap map) =
      case M.lookup k map of
        Just v -> do
          v' <- action v
          return $ M.insert k v' map
        _ -> return map
    adjust (k : p) (MetaMap map) =
      case M.lookup k map of
        Just value -> do
          m' <- adjust p value
          return $ M.insert k (MetaMap m') map
        _ -> return map
    adjust _ _ = return Map.empty

-- throw $
--   InternalException $
--     "Cannot adjust meta value on non object at: " <> show key

-- | Recursively traverse all meta values below the compound key that can be
-- stringified and transform them by the supplied function.
adjustMetaStringsBelow :: (Text -> Text) -> Text -> Meta -> Meta
adjustMetaStringsBelow func = adjustMetaValue (mapMetaValues func)

-- | Recursively traverse all meta values below the compound key that can be
-- stringified and transform them by the supplied action.
adjustMetaStringsBelowM ::
  (MonadFail m, MonadIO m) => (Text -> m Text) -> Text -> Meta -> m Meta
adjustMetaStringsBelowM action key meta = do
  adjusted <- adjustMetaValueM (mapMetaValuesM action) key meta
  return adjusted

-- | Adds a meta value to the list found at the compund key in the meta data.
-- If any intermediate containers do not exist, they are created.
addMetaValue :: ToMetaValue a => Text -> a -> Meta -> Meta
addMetaValue key value meta =
  case add (splitKey key) (MetaMap (unMeta meta)) of
    MetaMap map -> Meta map
    _ -> meta
  where
    add [] (MetaList list) = MetaList $ toMetaValue value : list
    add [k] (MetaMap m) =
      case M.lookup k m of
        Just value -> MetaMap $ M.insert k (add [] value) m
        _ -> MetaMap $ M.insert k (add [] $ MetaList []) m
    add (k : p) (MetaMap m) =
      case M.lookup k m of
        Just value -> MetaMap $ M.insert k (add p value) m
        _ -> MetaMap $ M.insert k (add p $ MetaMap M.empty) m
    add _ _ =
      throw $
        InternalException $
          "Cannot add meta value to non list at: " <> toString key

-- | Adds a meta value to the map found at the compund key in the meta data.
-- If any intermediate containers do not exist, they are created.
addMetaKeyValue :: Text -> Text -> MetaValue -> Meta -> Meta
addMetaKeyValue loc key value meta =
  case add (splitKey loc) (MetaMap (unMeta meta)) of
    MetaMap map -> Meta map
    _ -> meta
  where
    add [] (MetaMap m) = MetaMap $ M.insert key value m
    add [""] (MetaMap m) = MetaMap $ M.insert key value m
    add (k : p) (MetaMap m) =
      case M.lookup k m of
        Just value -> MetaMap $ M.insert k (add p value) m
        _ -> MetaMap $ M.insert k (add p $ MetaMap M.empty) m
    add _ _ =
      throw $
        InternalException $
          "Cannot add meta value to non list at: " <> toString loc

pandocMeta :: (Text -> Meta -> Maybe a) -> Pandoc -> Text -> Maybe a
pandocMeta f (Pandoc m _) = flip f m

instance (Ord a, ToMetaValue a) => ToMetaValue (Set a) where
  toMetaValue = MetaList . map toMetaValue . toList

class FromMetaValue a where
  fromMetaValue :: MetaValue -> Maybe a

instance FromMetaValue MetaValue where
  fromMetaValue = Just

instance FromMetaValue Bool where
  fromMetaValue (MetaBool bool) = Just bool
  fromMetaValue _ = Nothing

instance FromMetaValue Int where
  fromMetaValue value = (fromMetaValue value :: Maybe String) >>= readMaybe

instance FromMetaValue Float where
  fromMetaValue value = (fromMetaValue value :: Maybe String) >>= readMaybe

instance FromMetaValue Text where
  fromMetaValue (MetaString string) = Just string
  fromMetaValue (MetaInlines inlines) = Just $ stringify inlines
  fromMetaValue _ = Nothing

instance {-# OVERLAPS #-} FromMetaValue String where
  fromMetaValue value = toString <$> (fromMetaValue value :: Maybe Text)

instance {-# OVERLAPS #-} FromMetaValue [Text] where
  fromMetaValue (MetaList list) = Just $ mapMaybe fromMetaValue list
  fromMetaValue _ = Nothing

instance (Ord a, FromMetaValue a) => FromMetaValue (Set a) where
  fromMetaValue (MetaList list) = Just $ fromList $ mapMaybe fromMetaValue list
  fromMetaValue _ = Nothing

instance
  {-# OVERLAPS #-}
  (Ord a, FromMetaValue a) =>
  FromMetaValue (Map Text a)
  where
  fromMetaValue (MetaMap metaMap) =
    case M.foldlWithKey'
      ( \a k v ->
          case fromMetaValue v of
            Just string -> M.insert k string a
            _ -> a
      )
      M.empty
      metaMap of
      stringMap
        | null stringMap -> Nothing
      stringMap -> Just stringMap
  fromMetaValue _ = Nothing

instance {-# OVERLAPS #-} FromMetaValue a => FromMetaValue [a] where
  fromMetaValue (MetaList list) = Just $ mapMaybe fromMetaValue list
  fromMetaValue _ = Nothing

lookupMeta :: FromMetaValue a => Text -> Meta -> Maybe a
lookupMeta key meta = getMetaValue key meta >>= fromMetaValue

lookupMetaOrElse :: FromMetaValue a => a -> Text -> Meta -> a
lookupMetaOrElse def key meta = fromMaybe def $ lookupMeta key meta

lookupMetaOrFail :: FromMetaValue a => Text -> Meta -> a
lookupMetaOrFail key meta =
  case lookupMeta key meta of
    Just value -> value
    Nothing -> error $ "Cannot read meta value: " <> key

lookupInDictionary :: Text -> Meta -> Text
lookupInDictionary key meta =
  case lookupMeta "lang" meta of
    Just lang ->
      lookupMetaOrElse
        enDefault
        (Text.intercalate "." ["dictionary", lang, key])
        meta
    _ -> enDefault
  where
    enDefault :: Text
    enDefault =
      lookupMetaOrElse
        (Text.intercalate " " ["Dictionary entry for", key, "not available!"])
        (Text.intercalate "." ["dictionary", "en", key])
        meta

-- | Map a function over string values and stringified inline values.
-- Converts MetaInlines to MetaStrings. This may be a problem in some distant
-- future.
mapMeta :: (Text -> Text) -> Meta -> Meta
mapMeta f meta =
  let (MetaMap m) = mapMetaValues f (MetaMap (unMeta meta))
   in Meta m

-- | Map an IO action over string values and stringified inline values.
-- Converts MetaInlines to MetaStrings. This may be a problem in some distant
-- future.
mapMetaM :: (MonadFail m, MonadIO m) => (Text -> m Text) -> Meta -> m Meta
mapMetaM f meta = do
  (MetaMap m) <- mapMetaValuesM f (MetaMap (unMeta meta))
  return (Meta m)

-- | Map an IO action over string values and stringified inline values.
-- Converts MetaInlines to MetaStrings. This may be a problem in some distant
-- future.
mapMetaValuesM ::
  (MonadFail m, MonadIO m) => (Text -> m Text) -> MetaValue -> m MetaValue
mapMetaValuesM f v = do
  mapped <- map' v
  return mapped
  where
    map' (MetaMap m) =
      MetaMap . Map.fromList
        <$> mapM (\(k, v) -> (k,) <$> map' v) (Map.toList m)
    map' (MetaList l) = MetaList <$> mapM map' l
    map' (MetaString s) = MetaString <$> f s
    map' (MetaInlines i) = MetaString <$> f (stringify i)
    map' v = return v

-- | Map a function over string values and stringified inline values.
-- Converts MetaInlines to MetaStrings. This may be a problem in some distant
-- future.
mapMetaValues :: (Text -> Text) -> MetaValue -> MetaValue
mapMetaValues f = map'
  where
    map' (MetaMap m) =
      MetaMap . Map.fromList $ map (\(k, v) -> (k,) $ map' v) (Map.toList m)
    map' (MetaList l) = MetaList $ map map' l
    map' (MetaString s) = MetaString $ f s
    map' (MetaInlines i) = MetaString $ f (stringify i)
    map' v = v

-- | Map meta values in maps with the compound key.
mapMetaWithKey ::
  (MonadFail m, Monad m) => (Text -> Text -> m Text) -> Meta -> m Meta
mapMetaWithKey f meta = do
  (MetaMap m) <- map' "" (MetaMap (unMeta meta))
  return (Meta m)
  where
    map' k' (MetaMap m) =
      MetaMap . Map.fromList
        <$> mapM (\(k, v) -> (k,) <$> map' (join k' k) v) (Map.toList m)
    map' k (MetaList l) =
      MetaList
        <$> mapM (\(n, v) -> map' (k <> "[" <> show n <> "]") v) (zip [0 ..] l)
    map' k (MetaString s) = MetaString <$> f k s
    map' k (MetaInlines i) = MetaString <$> f k (stringify i)
    map' _ v = return v
    join x y = Text.intercalate "." $ filter (not . Text.null) [x, y]

-- | Reads a single meta data file. If something goes wrong, an empty Meta
-- structure is returned.
readMetaDataFile :: FilePath -> IO Meta
readMetaDataFile file =
  catch
    (toPandocMeta <$> Y.decodeFileThrow file)
    $ \(SomeException _) -> return nullMeta

embedMetaMeta :: Pandoc -> Pandoc
embedMetaMeta (Pandoc meta blocks) = Pandoc metaMeta blocks
  where
    metaMeta = addMetaField "decker-meta" (decodeUtf8 $ A.encodePretty $ fromPandocMeta meta :: Text) meta
