{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Internal.Meta
  ( globalMetaFileName
  , toPandocMeta
  , toMustacheMeta
  , mergePandocMeta
  , mergePandocMeta'
  , readMetaData
  , getAdditionalMeta
  , getMetaInt
  , getMetaIntOrElse
  , getMetaBool
  , getMetaBoolOrElse
  , getMetaString
  , getMetaStringOrElse
  , getMetaStringList
  , getMetaStringListOrElse
  , getMetaValue
  , getMetaStringMap
  , pandocMeta
  , DeckerException(..)
  ) where

import Text.Decker.Internal.Exception
import Text.Decker.Writer.Markdown

import Control.Arrow
import Control.Exception
import qualified Data.HashMap.Strict as H

-- import qualified Data.List as L
import Data.List.Safe ((!!))
import qualified Data.List.Split as L
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Development.Shake
import Prelude hiding ((!!))
import System.Directory as Dir
import System.FilePath
import qualified Text.Mustache.Types as MT
import Text.Pandoc hiding (writeMarkdown)
import Text.Pandoc.Shared
import Text.Read
import Text.Regex.TDFA

-- | Name of the one global meta data file
globalMetaFileName = "decker.yaml"

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are
-- rendered to markdown strings with default options.
toMustacheMeta :: Meta -> MT.Value
toMustacheMeta (Meta mmap) = toMustacheMeta' (MetaMap mmap)

toMustacheMeta' :: MetaValue -> MT.Value
toMustacheMeta' (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta') $ Map.toList mmap
toMustacheMeta' (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta' a
toMustacheMeta' (MetaBool bool) = MT.Bool bool
toMustacheMeta' (MetaString string) = MT.String $ T.pack string
toMustacheMeta' (MetaInlines inlines) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) [Plain inlines])
toMustacheMeta' (MetaBlocks blocks) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) blocks)

writeMarkdownText :: WriterOptions -> Pandoc -> T.Text
writeMarkdownText options pandoc =
  case runPure $ writeMarkdown options pandoc of
    Right text -> text
    Left err -> throw $ PandocException $ show err

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
    -- merge (MetaList listL) (MetaList listR) = MetaList $ L.union listL listR
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
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta') $ H.toList m
toPandocMeta' (Y.Array vector) =
  MetaList $ map toPandocMeta' $ Vec.toList vector
toPandocMeta' (Y.String text) = MetaString $ T.unpack text
toPandocMeta' (Y.Number scientific) = MetaString $ show scientific
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

-- Reads the global meta data file for a given directory
readMetaData :: FilePath -> IO Meta
readMetaData dir = do
  let file = dir </> globalMetaFileName
  readMetaDataFile file

-- Check for additional meta files specified in the Meta option "meta-data"
getAdditionalMeta :: Meta -> Action Meta
getAdditionalMeta meta = do
  let m = getMetaStringList "meta-data" meta
  case m of
    Just metafiles -> do
      addmeta <- liftIO $ traverse readMetaDataFile metafiles
      need metafiles
      -- foldr and reversed addmeta list because additional meta should overwrite default meta
      -- alternative: foldl and flip mergePandocMeta'
      return $ foldr mergePandocMeta' meta (reverse addmeta)
    _ -> return meta

-- Read a single meta data file
readMetaDataFile :: FilePath -> IO Meta
readMetaDataFile file = do
  exists <- Dir.doesFileExist file
  f <- makeRelativeToCurrentDirectory file
  meta <-
    if exists
      then decodeYaml file
      else do
        putStrLn $
          "WARNING: file " ++
          show f ++
          " does not exist!\nIf you still have a \"*-meta.yaml\" file in your project please rename it to \"decker.yaml\"!"
        return (Y.object [])
  return $ toPandocMeta meta

getMetaInt :: String -> Meta -> Maybe Int
getMetaInt key meta = getMetaString key meta >>= readMaybe

getMetaIntOrElse :: String -> Int -> Meta -> Int
getMetaIntOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> fromMaybe def $ readMaybe string
    _ -> def

-- | Lookup a boolean value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaBool :: String -> Meta -> Maybe Bool
getMetaBool key meta = getMetaValue key meta >>= metaToBool

metaToBool :: MetaValue -> Maybe Bool
metaToBool (MetaBool bool) = Just bool
metaToBool _ = Nothing

getMetaBoolOrElse :: String -> Bool -> Meta -> Bool
getMetaBoolOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaBool bool) -> bool
    _ -> def

-- | Lookup a String value in a Pandoc meta data hierarchy. The key string
-- notation is indexed subkeys separated by '.', eg. `top.list[3].value`.
getMetaString :: String -> Meta -> Maybe String
getMetaString key meta = getMetaValue key meta >>= metaToString

getMetaStringOrElse :: String -> String -> Meta -> String
getMetaStringOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaString string) -> string
    Just (MetaInlines inlines) -> stringify inlines
    _ -> def

getMetaStringList :: String -> Meta -> Maybe [String]
getMetaStringList key meta = getMetaValue key meta >>= metaToStringList

getMetaStringListOrElse :: String -> [String] -> Meta -> [String]
getMetaStringListOrElse key def meta =
  case getMetaValue key meta of
    Just (MetaList list) -> mapMaybe metaToString list
    _ -> def

-- | Split a compound meta key at the dots and separate the array indexes.
splitKey = concatMap (L.split (L.keepDelimsL (L.oneOf "["))) . L.splitOn "."

-- | Extract the bracketed array index string.
arrayIndex :: String -> Maybe String
arrayIndex key =
  listToMaybe $
  reverse (getAllTextSubmatches (key =~ ("^\\[([0-9]+)\\]$" :: String)))

-- | Recursively deconstruct a compound key and drill into the meta data hierarchy.
getMetaValue :: String -> Meta -> Maybe MetaValue
getMetaValue key meta = lookup' (splitKey key) (MetaMap (unMeta meta))
  where
    lookup' (key:path) (MetaMap map) = M.lookup key map >>= lookup' path
    lookup' (key:path) (MetaList list) =
      arrayIndex key >>= readMaybe >>= (!!) list >>= lookup' path
    lookup' (_:_) _ = Nothing
    lookup' [] mv = Just mv

getMetaStringMap :: String -> Meta -> Maybe (M.Map String String)
getMetaStringMap key meta = getMetaValue key meta >>= metaToStringMap

metaToString :: MetaValue -> Maybe String
metaToString (MetaString string) = Just string
metaToString (MetaInlines inlines) = Just $ stringify inlines
metaToString _ = Nothing

metaToStringList :: MetaValue -> Maybe [String]
metaToStringList (MetaList list) = Just $ mapMaybe metaToString list
metaToStringList _ = Nothing

metaToStringMap :: MetaValue -> Maybe (M.Map String String)
metaToStringMap (MetaMap metaMap) =
  case M.foldlWithKey'
         (\a k v ->
            case metaToString v of
              Just string -> M.insert k string a
              _ -> a)
         M.empty
         metaMap of
    stringMap
      | null stringMap -> Nothing
    stringMap -> Just stringMap
metaToStringMap _ = Nothing

pandocMeta :: (String -> Meta -> Maybe a) -> Pandoc -> String -> Maybe a
pandocMeta f (Pandoc m _) = flip f m
