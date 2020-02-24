{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Internal.Meta
  ( globalMetaFileName
  , mergePandocMeta
  , mergePandocMeta'
  , readMetaData
  , getAdditionalMeta
  , getMetaInt
  , getMetaIntOrElse
  , getMetaBool
  , getMetaBoolOrElse
  , getMetaText
  , getMetaString
  , getMetaStringList
  , getMetaTextOrElse
  , getMetaTextList
  , getMetaTextListOrElse
  , getMetaValue
  , getMetaTextMap
  , pandocMeta
  , toMeta
  , DeckerException(..)
  ) where

import Text.Decker.Internal.Exception

import Control.Exception

-- import qualified Data.List as L
import Data.Aeson.Types (parseEither)
import Data.List.Safe ((!!))
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Yaml as Y
import Development.Shake
import Prelude hiding ((!!))
import System.Directory as Dir
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Read

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

decodeYaml :: FilePath -> IO Y.Value
decodeYaml yamlFile = do
  result <- Y.decodeFileEither yamlFile
  case result of
    Right object@(Y.Object _) -> return object
    Right _ ->
      throw $
      YamlException $ "Top-level meta value must be an object: " ++ yamlFile
    Left exception -> throw exception

-- Reads the global meta data file for a given directory
readMetaData :: FilePath -> IO Meta
readMetaData dir = do
  let file = dir </> globalMetaFileName
  readMetaDataFile file

-- Check for additional meta files specified in the Meta option "meta-data"
getAdditionalMeta :: Meta -> Action Meta
getAdditionalMeta meta = do
  let m = getMetaTextList "meta-data" meta
  case m of
    Just metafiles -> do
      let filePathes = map Text.unpack metafiles
      addmeta <- liftIO $ traverse readMetaDataFile filePathes
      need filePathes
      -- foldr and reversed addmeta list because additional meta should overwrite default meta
      -- alternative: foldl and flip mergePandocMeta'
      return $ foldr mergePandocMeta' meta (reverse addmeta)
    _ -> return meta

-- Read a single meta data file
readMetaDataFile :: FilePath -> IO Meta
readMetaDataFile file = do
  exists <- Dir.doesFileExist file
  f <- makeRelativeToCurrentDirectory file
  yaml <-
    if exists
      then decodeYaml file
      else do
        putStrLn $
          "WARNING: file " ++
          show f ++
          " does not exist!\nIf you still have a \"*-meta.yaml\" file in your project please rename it to \"decker.yaml\"!"
        return (Y.object [])
  return $ toMeta yaml

toMeta :: Y.Value -> Meta
toMeta yaml = 
  case parseEither Y.parseJSON yaml of
    Right meta -> meta
    Left err -> throw $ YamlException err

getMetaInt :: Text.Text -> Meta -> Maybe Int
getMetaInt key meta = getMetaText key meta >>= readMaybe . Text.unpack

getMetaIntOrElse :: Text.Text -> Int -> Meta -> Int
getMetaIntOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> fromMaybe def $ readMaybe $ Text.unpack string
    _ -> def

-- | Lookup a boolean value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaBool :: Text.Text -> Meta -> Maybe Bool
getMetaBool key meta = getMetaValue key meta >>= metaToBool

metaToBool :: MetaValue -> Maybe Bool
metaToBool (MetaBool bool) = Just bool
metaToBool _ = Nothing

getMetaBoolOrElse :: Text.Text -> Bool -> Meta -> Bool
getMetaBoolOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaBool bool) -> bool
    _ -> def

-- | Lookup a String value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaText :: Text.Text -> Meta -> Maybe Text.Text
getMetaText key meta = getMetaValue key meta >>= metaToText

getMetaString :: String -> Meta -> Maybe String
getMetaString key meta =
  Text.unpack <$> (getMetaValue (Text.pack key) meta >>= metaToText)

getMetaTextOrElse :: Text.Text -> Text.Text -> Meta -> Text.Text
getMetaTextOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> string
    Just (MetaInlines inlines) -> stringify inlines
    _ -> def

getMetaStringList :: String -> Meta -> Maybe [String]
getMetaStringList key meta =
  map Text.unpack <$> (getMetaValue (Text.pack key) meta >>= metaToTextList)

getMetaTextList :: Text.Text -> Meta -> Maybe [Text.Text]
getMetaTextList key meta = getMetaValue key meta >>= metaToTextList

getMetaTextListOrElse :: Text.Text -> [Text.Text] -> Meta -> [Text.Text]
getMetaTextListOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaList list) -> mapMaybe metaToText list
    _ -> def

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
getMetaValue :: Text.Text -> Meta -> Maybe MetaValue
getMetaValue key meta = lookup' (splitKey key) (MetaMap (unMeta meta))
  where
    lookup' (key:path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key:path) (MetaList list) =
      (readMaybe . Text.unpack) key >>= (!!) list >>= lookup' path
    lookup' (_:_) _ = Nothing
    lookup' [] mv = Just mv

getMetaTextMap :: Text.Text -> Meta -> Maybe (M.Map Text.Text Text.Text)
getMetaTextMap key meta = getMetaValue key meta >>= metaToTextMap

metaToText :: MetaValue -> Maybe Text.Text
metaToText (MetaString string) = Just string
metaToText (MetaInlines inlines) = Just $ stringify inlines
metaToText _ = Nothing

metaToTextList :: MetaValue -> Maybe [Text.Text]
metaToTextList (MetaList list) = Just $ mapMaybe metaToText list
metaToTextList _ = Nothing

metaToTextMap :: MetaValue -> Maybe (M.Map Text.Text Text.Text)
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

pandocMeta :: (Text.Text -> Meta -> Maybe a) -> Pandoc -> Text.Text -> Maybe a
pandocMeta f (Pandoc m _) = flip f m
