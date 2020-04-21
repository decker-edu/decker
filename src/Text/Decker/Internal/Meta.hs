{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Meta
  ( DeckerException(..)
  , addMetaValue
  , addStringToMetaList
  , getAdditionalMeta
  , getMetaBool
  , getMetaBoolOrElse
  , getMetaInt
  , getMetaIntOrElse
  , getMetaStringOrElse
  , getMetaString
  , getMetaStringList
  , getMetaStringListOrElse
  , getMetaText
  , getMetaTextList
  , getMetaTextListOrElse
  , getMetaTextMap
  , getMetaTextOrElse
  , getMetaValue
  , getMetaValueList
  , globalMetaFileName
  , mergePandocMeta
  , mergePandocMeta'
  , pandocMeta
  , setBoolMetaValue
  , setMetaValue
  , setTextMetaValue
  , toPandocMeta
  , toPandocMeta'
  , decodeYaml
  , readMetaDataFile
  , lookupMeta
  , lookupMetaOrElse
  , lookupMetaOrFail
  ) where

import Text.Decker.Internal.Exception

import Control.Exception
import qualified Data.HashMap.Strict as H
import Data.List.Safe ((!!))
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Development.Shake
import Relude
import System.FilePath
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Shared hiding (toString, toText)

-- | Name of the one global meta data file
globalMetaFileName = "decker.yaml"

-- | Simple top-level merge of two meta values. Left-biased. 
mergePandocMeta :: Meta -> Meta -> Meta
mergePandocMeta (Meta meta1) (Meta meta2) = Meta $ Map.union meta1 meta2

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

decodeYaml :: FilePath -> IO Y.Value
decodeYaml yamlFile = do
  result <- Y.decodeFileEither yamlFile
  case result of
    Right object@(Y.Object _) -> return object
    Right _ ->
      throw $
      YamlException $ "Top-level meta value must be an object: " ++ yamlFile
    Left exception -> throw exception

-- Check for additional meta files specified in the Meta option "meta-data"
getAdditionalMeta :: FilePath -> Meta -> Action Meta
getAdditionalMeta baseDir meta = do
  let moreFiles =
        map
          ((baseDir </>) . Text.unpack)
          (getMetaTextListOrElse "meta-data" [] meta)
  need moreFiles
  moreMeta <- liftIO $ traverse readMetaDataFile moreFiles
  return $ foldr mergePandocMeta' meta (reverse moreMeta)

-- Reads a single meta data file.
readMetaDataFile :: FilePath -> IO Meta
readMetaDataFile file = toPandocMeta <$> Y.decodeFileThrow file

getMetaInt :: Text -> Meta -> Maybe Int
getMetaInt key meta = getMetaText key meta >>= readMaybe . Text.unpack

getMetaIntOrElse :: Text -> Int -> Meta -> Int
getMetaIntOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> fromMaybe def $ readMaybe $ Text.unpack string
    _ -> def

-- | Lookup a boolean value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaBool :: Text -> Meta -> Maybe Bool
getMetaBool key meta = getMetaValue key meta >>= metaToBool

metaToBool :: MetaValue -> Maybe Bool
metaToBool (MetaBool bool) = Just bool
metaToBool _ = Nothing

getMetaBoolOrElse :: Text -> Bool -> Meta -> Bool
getMetaBoolOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaBool bool) -> bool
    _ -> def

-- | Lookup a String value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaText :: Text -> Meta -> Maybe Text
getMetaText key meta = getMetaValue key meta >>= metaToText

getMetaString :: String -> Meta -> Maybe String
getMetaString key meta =
  Text.unpack <$> (getMetaValue (Text.pack key) meta >>= metaToText)

getMetaStringOrElse :: Text -> String -> Meta -> String
getMetaStringOrElse key def meta =
  toString $ getMetaTextOrElse key (toText def) meta

getMetaTextOrElse :: Text -> Text -> Meta -> Text
getMetaTextOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> string
    Just (MetaInlines inlines) -> stringify inlines
    _ -> def

getMetaStringList :: Text -> Meta -> Maybe [String]
getMetaStringList key meta = map Text.unpack <$> getMetaTextList key meta

getMetaStringListOrElse :: Text -> [String] -> Meta -> [String]
getMetaStringListOrElse key def meta =
  fromMaybe def $ getMetaStringList key meta

getMetaTextList :: Text -> Meta -> Maybe [Text]
getMetaTextList key meta = getMetaValue key meta >>= metaToTextList

getMetaTextListOrElse :: Text -> [Text] -> Meta -> [Text]
getMetaTextListOrElse key def meta = fromMaybe def $ getMetaTextList key meta

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

getMetaValueList :: Text -> Meta -> [MetaValue]
getMetaValueList key meta =
  case getMetaValue key meta of
    Just (MetaList vs) -> vs
    _ -> []

setMetaValue :: Text -> MetaValue -> Meta -> Meta
setMetaValue key value meta = Meta $ set (splitKey key) (MetaMap (unMeta meta))
  where
    set [k] (MetaMap map) = M.insert k value map
    set (k:p) (MetaMap map) =
      case M.lookup k map of
        Just value -> M.insert k (MetaMap $ set p value) map
        _ -> M.insert k (MetaMap $ set p $ MetaMap M.empty) map
    set _ _ =
      throw $
      InternalException $ "Cannot set meta value on non object at: " <> show key

setBoolMetaValue key bool = setMetaValue key (MetaBool bool)

setTextMetaValue key string = setMetaValue key (MetaString string)

addMetaValue :: Text -> MetaValue -> Meta -> Meta
addMetaValue key value meta =
  case add (splitKey key) (MetaMap (unMeta meta)) of
    MetaMap map -> Meta map
    _ -> meta
  where
    add [] (MetaList list) = MetaList $ value : list
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

addStringToMetaList :: Text -> Text -> Meta -> Meta
addStringToMetaList key string = addMetaValue key (MetaString string)

getMetaTextMap :: Text -> Meta -> Maybe (M.Map Text Text)
getMetaTextMap key meta = getMetaValue key meta >>= metaToTextMap

metaToText :: MetaValue -> Maybe Text
metaToText (MetaString string) = Just string
metaToText (MetaInlines inlines) = Just $ stringify inlines
metaToText _ = Nothing

metaToTextList :: MetaValue -> Maybe [Text]
metaToTextList (MetaList list) = Just $ mapMaybe metaToText list
metaToTextList _ = Nothing

metaToTextMap :: MetaValue -> Maybe (M.Map Text Text)
metaToTextMap (MetaMap metaMap) =
  case M.foldlWithKey'
         (\a k v ->
            case metaToText v of
              Just string -> M.insert k string a
              _ -> a)
         M.empty
         metaMap of
    stringMap
      | null stringMap -> Nothing
    stringMap -> Just stringMap
metaToTextMap _ = Nothing

pandocMeta :: (Text -> Meta -> Maybe a) -> Pandoc -> Text -> Maybe a
pandocMeta f (Pandoc m _) = flip f m

class FromMeta a where
  fromMeta :: MetaValue -> Maybe a

instance FromMeta Bool where
  fromMeta (MetaBool bool) = Just bool
  fromMeta _ = Nothing

instance FromMeta Int where
  fromMeta value = (fromMeta value :: Maybe String) >>= readMaybe

instance FromMeta Text where
  fromMeta (MetaString string) = Just string
  fromMeta (MetaInlines inlines) = Just $ stringify inlines
  fromMeta _ = Nothing

instance FromMeta String where
  fromMeta value = toString <$> (fromMeta value :: Maybe Text)

instance FromMeta [Text] where
  fromMeta (MetaList list) = Just $ mapMaybe fromMeta list
  fromMeta _ = Nothing

instance FromMeta (Map Text Text) where
  fromMeta (MetaMap metaMap) =
    case M.foldlWithKey'
           (\a k v ->
              case fromMeta v of
                Just string -> M.insert k string a
                _ -> a)
           M.empty
           metaMap of
      stringMap
        | null stringMap -> Nothing
      stringMap -> Just stringMap
  fromMeta _ = Nothing

lookupMeta :: FromMeta a => Text -> Meta -> Maybe a
lookupMeta key meta = getMetaValue key meta >>= fromMeta

lookupMetaOrElse :: FromMeta a => a -> Text -> Meta -> a
lookupMetaOrElse def key meta = fromMaybe def $ lookupMeta key meta

lookupMetaOrFail :: FromMeta a => Text -> Meta -> a
lookupMetaOrFail key meta =
  case lookupMeta key meta of
    Just value -> value
    Nothing -> error $ "Cannot read meta value: " <> key
