{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Meta
  ( DeckerException(..)
  , FromMetaValue(..)
  , addMetaValue
  , globalMetaFileName
  , mergePandocMeta'
  , pandocMeta
  , setMetaValue
  , toPandocMeta
  , toPandocMeta'
  , lookupMeta
  , lookupMetaOrElse
  , lookupMetaOrFail
  , mapMeta
  , mapMetaWithKey
  , readMetaDataFile
  ) where

import Text.Decker.Internal.Exception

import Control.Exception
import qualified Data.HashMap.Strict as H
import Data.List.Safe ((!!))
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Relude
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Builder hiding (fromList, lookupMeta, toList)
import Text.Pandoc.Shared hiding (toString, toText)

-- | Name of the one global meta data file
globalMetaFileName = "decker.yaml"

-- | Fine-grained recursive merge of two meta values. Left-biased. 
mergePandocMeta' :: Meta -> Meta -> Meta
mergePandocMeta' (Meta left) (Meta right) =
  case merge (MetaMap left) (MetaMap right) of
    MetaMap m -> Meta m
    _ -> throw $ InternalException "This cannot happen."
  where
    merge :: MetaValue -> MetaValue -> MetaValue
    merge (MetaMap mapL) (MetaMap mapR) =
      MetaMap $ Map.unionWith merge mapL mapR
    merge (MetaList listL) (MetaList listR) =
      MetaList $ Set.toList $ Set.fromList listL <> Set.fromList listR
    merge left right = left

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> Meta
toPandocMeta v@(Y.Object m) =
  case toPandocMeta' v of
    (MetaMap map) -> Meta map
    _ -> Meta M.empty
toPandocMeta _ = Meta M.empty

toPandocMeta' :: Y.Value -> MetaValue
toPandocMeta' (Y.Object m) =
  MetaMap $ Map.fromList $ map (second toPandocMeta') $ H.toList m
toPandocMeta' (Y.Array vector) =
  MetaList $ map toPandocMeta' $ Vec.toList vector
toPandocMeta' (Y.String text) = MetaString text
toPandocMeta' (Y.Number scientific) = MetaString $ Text.pack $ show scientific
toPandocMeta' (Y.Bool bool) = MetaBool bool
toPandocMeta' Y.Null = MetaList []

-- | Split a compound meta key at the dots and separate the array indexes.
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
    lookup' (key:path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key:path) (MetaList list) =
      (readMaybe . Text.unpack) key >>= (!!) list >>= lookup' path
    lookup' (_:_) _ = Nothing
    lookup' [] mv = Just mv

-- | Sets a meta value at the compund key in the meta data. If any intermediate
-- containers do not exist, they are created. 
setMetaValue :: ToMetaValue a => Text -> a -> Meta -> Meta
setMetaValue key value meta = Meta $ set (splitKey key) (MetaMap (unMeta meta))
  where
    set [k] (MetaMap map) = M.insert k (toMetaValue value) map
    set (k:p) (MetaMap map) =
      case M.lookup k map of
        Just value -> M.insert k (MetaMap $ set p value) map
        _ -> M.insert k (MetaMap $ set p $ MetaMap M.empty) map
    set _ _ =
      throw $
      InternalException $ "Cannot set meta value on non object at: " <> show key

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
    add (k:p) (MetaMap m) =
      case M.lookup k m of
        Just value -> MetaMap $ M.insert k (add p value) m
        _ -> MetaMap $ M.insert k (add p $ MetaMap M.empty) m
    add _ _ =
      throw $
      InternalException $
      "Cannot add meta value to non list at: " <> toString key

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

instance {-# OVERLAPS #-} (Ord a, FromMetaValue a) =>
                          FromMetaValue (Map Text a) where
  fromMetaValue (MetaMap metaMap) =
    case M.foldlWithKey'
           (\a k v ->
              case fromMetaValue v of
                Just string -> M.insert k string a
                _ -> a)
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

-- | Map an IO action over string values and stringified inline values.
-- Converts MetaInlines to MetaStrings. This may be a problem in some distant
-- future.
mapMeta :: (MonadFail m, Monad m) => (Text -> m Text) -> Meta -> m Meta
mapMeta f meta = do
  (MetaMap m) <- map' (MetaMap (unMeta meta))
  return (Meta m)
  where
    map' (MetaMap m) =
      MetaMap . Map.fromList <$>
      mapM (\(k, v) -> (k, ) <$> map' v) (Map.toList m)
    map' (MetaList l) = MetaList <$> mapM map' l
    map' (MetaString s) = MetaString <$> f s
    map' (MetaInlines i) = MetaString <$> f (stringify i)
    map' v = return v

-- | Map meta values in maps with the compound key.
mapMetaWithKey ::
     (MonadFail m, Monad m) => (Text -> Text -> m Text) -> Meta -> m Meta
mapMetaWithKey f meta = do
  (MetaMap m) <- map' "" (MetaMap (unMeta meta))
  return (Meta m)
  where
    map' k' (MetaMap m) =
      MetaMap . Map.fromList <$>
      mapM (\(k, v) -> (k, ) <$> map' (join k' k) v) (Map.toList m)
    map' k (MetaList l) =
      MetaList <$>
      mapM (\(n, v) -> map' (k <> "[" <> show n <> "]") v) (zip [0 ..] l)
    map' k (MetaString s) = MetaString <$> f k s
    map' k (MetaInlines i) = MetaString <$> f k (stringify i)
    map' _ v = return v
    join x y = Text.intercalate "." $ filter (not . Text.null) [x, y]

-- | Reads a single meta data file.
readMetaDataFile :: FilePath -> IO Meta
readMetaDataFile file = toPandocMeta <$> Y.decodeFileThrow file


