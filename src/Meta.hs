{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Meta
  ( toPandocMeta
  , toMustacheMeta
  , mergePandocMeta
  , joinMeta
  , readMetaData
  , aggregateMetaData
  , lookupPandocMeta
  , lookupInt
  , lookupBool
  , lookupString
  , lookupMetaValue
  , DeckerException(..)
  ) where

import Common
import Exception
import Markdown

import Control.Arrow
import Control.Exception
import qualified Data.HashMap.Strict as H
import Data.List.Extra
import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Yaml as Y
import Debug.Trace
import System.FilePath
import System.FilePath.Glob
import qualified Text.Mustache.Types as MT
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Read

joinMeta :: Y.Value -> Y.Value -> Y.Value
joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
joinMeta (Y.Object old) _ = Y.Object old
joinMeta _ (Y.Object new) = Y.Object new
joinMeta _ _ = throw $ YamlException "Can only join YAML objects."

-- | Converts pandoc meta data to mustache meta data. Inlines and blocks are
-- rendered to markdown strings with default options.
toMustacheMeta :: MetaValue -> MT.Value
toMustacheMeta (MetaMap mmap) =
  MT.Object $ H.fromList $ map (T.pack *** toMustacheMeta) $ Map.toList mmap
toMustacheMeta (MetaList a) = MT.Array $ Vec.fromList $ map toMustacheMeta a
toMustacheMeta (MetaBool bool) = MT.Bool bool
toMustacheMeta (MetaString string) = MT.String $ T.pack string
toMustacheMeta (MetaInlines inlines) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) [Plain inlines])
toMustacheMeta (MetaBlocks blocks) =
  MT.String $ writeMarkdownText def (Pandoc (Meta Map.empty) blocks)

writeMarkdownText :: WriterOptions -> Pandoc -> T.Text
writeMarkdownText options pandoc =
  case runPure $ Markdown.writeMarkdown options pandoc of
    Right text -> text
    Left err -> throw $ PandocException $ show err

mergePandocMeta :: MetaValue -> MetaValue -> MetaValue
mergePandocMeta (MetaMap meta1) (MetaMap meta2) =
  MetaMap $ Map.union meta1 meta2
mergePandocMeta meta1 _ = meta1

-- | Converts YAML meta data to pandoc meta data.
toPandocMeta :: Y.Value -> MetaValue
toPandocMeta (Y.Object m) =
  MetaMap $ Map.fromList $ map (T.unpack *** toPandocMeta) $ H.toList m
toPandocMeta (Y.Array vector) = MetaList $ map toPandocMeta $ Vec.toList vector
toPandocMeta (Y.String text) = MetaString $ T.unpack text
toPandocMeta (Y.Number scientific) = MetaString $ show scientific
toPandocMeta (Y.Bool bool) = MetaBool bool
toPandocMeta Y.Null = MetaList []

decodeYaml :: FilePath -> IO Y.Value
decodeYaml yamlFile = do
  result <- Y.decodeFileEither yamlFile
  case result of
    Right object@(Y.Object _) -> return object
    Right _ ->
      throw $
      Exception.YamlException $
      "Top-level meta value must be an object: " ++ yamlFile
    Left exception -> throw exception

readMetaData :: FilePath -> IO Y.Value
readMetaData dir = do
  files <- globDir1 (compile "*-meta.yaml") dir
  meta <- mapM decodeYaml files
  return $ foldl joinMeta (Y.object []) meta

aggregateMetaData :: FilePath -> FilePath -> IO Y.Value
aggregateMetaData top = walkUpTo
  where
    walkUpTo dir = do
      fromHere <- readMetaData dir
      if equalFilePath top dir
        then return fromHere
        else do
          fromAbove <- walkUpTo (takeDirectory dir)
          return $ joinMeta fromHere fromAbove

lookupPandocMeta :: String -> Meta -> Maybe String
lookupPandocMeta key (Meta m) =
  case splitOn "." key of
    [] -> Nothing
    k:ks -> lookup' ks (Map.lookup k m)
  where
    lookup' :: [String] -> Maybe MetaValue -> Maybe String
    lookup' (k:ks) (Just (MetaMap m)) = lookup' ks (Map.lookup k m)
    lookup' [] (Just (MetaBool b)) = Just $ show b
    lookup' [] (Just (MetaString s)) = Just s
    lookup' [] (Just (MetaInlines i)) = Just $ stringify i
    lookup' _ _ = Nothing

lookupInt :: String -> Int -> Meta -> Int
lookupInt key def meta =
  case lookupMetaValue key meta of
    Just (MetaString string) -> fromMaybe def $ readMaybe string
    _ -> def

lookupBool :: String -> Bool -> Meta -> Bool
lookupBool key def meta =
  case lookupMetaValue key meta of
    Just (MetaBool bool) -> bool
    _ -> def

lookupString :: String -> String -> Meta -> String
lookupString key def meta =
  case lookupMetaValue key meta of
    Just (MetaString string) -> string
    Just (MetaInlines inlines) -> stringify inlines
    _ -> def

lookupMetaValue :: String -> Meta -> Maybe MetaValue
lookupMetaValue key (Meta mm) = lookup path (MetaMap mm)
  where
    path = splitOn "." key
    lookup :: [String] -> MetaValue -> Maybe MetaValue
    lookup (first:rest) (MetaMap m) =
      maybe Nothing (lookup rest) (Map.lookup first m)
    lookup (first:rest) _ = Nothing
    lookup [] value = Just value
