{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.MetaExtra where

import Data.Aeson.Encode.Pretty qualified as A
import Development.Shake (Action, putVerbose, need)
import Relude
import System.Directory qualified as Dir
import Text.Pandoc (Pandoc (..))
import Text.Pandoc.Definition (Meta, nullMeta)
import Text.Pandoc.Shared (addMetaField)
import Text.Decker.Internal.Meta (fromPandocMeta, lookupMetaOrElse, mergePandocMeta, readMetaDataFile, adjustMetaStringsBelowM)
import System.FilePath (takeDirectory, (</>))
import Text.Decker.Internal.Common (metaArgsFile, publicDir)
import Text.Decker.Resource.Template (readTemplateMeta, readTemplateMetaIO)
import qualified Data.List as List
import Text.Decker.Internal.URI (makeProjectPath)
import Text.Decker.Internal.Helper (makeRelativeTo)
import Control.Monad (foldM)

embedMetaMeta :: Pandoc -> Pandoc
embedMetaMeta (Pandoc meta blocks) = Pandoc metaMeta blocks
  where
    metaMeta = addMetaField "decker-meta" (decodeUtf8 $ A.encodePretty $ fromPandocMeta meta :: Text) meta

-- | Check for additional meta files specified in the Meta option `meta-data`.
-- Assumes that files are specified with absolute paths.
readAdditionalMeta :: Meta -> FilePath -> Meta -> Action Meta
readAdditionalMeta globalMeta base meta = do
  let metaFiles = lookupMetaOrElse [] "meta-data" meta :: [String]
  putVerbose $ "# --> readAdditionalMeta: " <> show metaFiles
  moreMeta <- traverse (readMetaData globalMeta) metaFiles
  return $ foldr mergePandocMeta meta (reverse moreMeta)

-- | Reads a meta data file. All values that are paths to local project files
-- are made absolute. Files referenced in `meta-data` are recursively loaded
-- and merged.
readMetaData :: Meta -> FilePath -> Action Meta
readMetaData globalMeta path = do
  need [path]
  putVerbose $ "# --> readMetaData: " <> path
  let base = takeDirectory path
  meta <- liftIO $ fromRight nullMeta <$> readMetaDataFile path
  adjustMetaPaths globalMeta base meta >>= readAdditionalMeta globalMeta base

readDeckerMeta :: FilePath -> Action Meta
readDeckerMeta file = do
  deckerMeta <- readMetaData nullMeta file
  args <- liftIO metaArgsFile
  argsMeta <- readMetaData nullMeta args
  let baseMeta = mergePandocMeta argsMeta deckerMeta
  defaultMeta <- readTemplateMeta baseMeta
  return $ mergePandocMeta baseMeta defaultMeta

readMetaDataIO :: Meta -> FilePath -> IO Meta
readMetaDataIO globalMeta path = do
  -- putStrLn $ "# --> readMetaDataIO: " <> path
  let base = takeDirectory path
  meta <- liftIO $ fromRight nullMeta <$> readMetaDataFile path
  adjustMetaPaths globalMeta base meta >>= readAdditionalMetaIO globalMeta base

readAdditionalMetaIO :: Meta -> FilePath -> Meta -> IO Meta
readAdditionalMetaIO globalMeta base meta = do
  let metaFiles = lookupMetaOrElse [] "meta-data" meta :: [String]
  -- putStrLn $ "# --> readAdditionalMeta: " <> show metaFiles
  moreMeta <- traverse (readMetaDataIO globalMeta) metaFiles
  return $ foldr mergePandocMeta meta (reverse moreMeta)

readDeckerMetaIO :: FilePath -> IO Meta
readDeckerMetaIO file = do
  deckerMeta <- readMetaDataIO nullMeta file
  args <- metaArgsFile
  argsMeta <- readMetaDataIO nullMeta args
  let baseMeta = mergePandocMeta argsMeta deckerMeta
  defaultMeta <- readTemplateMetaIO baseMeta
  return $ mergePandocMeta baseMeta defaultMeta

-- | Reads additional meta data from files listed in `meta-data:`. In case of
-- conflict, order of encounter determines preference. Later values win.
expandMeta :: Meta -> FilePath -> Pandoc -> Action Pandoc
expandMeta globalMeta base (Pandoc docMeta content) = do
  expanded <-
    adjustMetaPaths globalMeta base docMeta
      >>= readAdditionalMeta globalMeta base
  return (Pandoc expanded content)

-- | Adjusts meta data values that reference local files needed at run-time (by
-- some plugin, presumeably) and at compile-time (by some template). Lists of
-- these variables can be specified in the meta data.
adjustMetaPaths :: (MonadFail m, MonadIO m) => Meta -> FilePath -> Meta -> m Meta
adjustMetaPaths globalMeta base meta = do
  let variables = pathVariables (mergePandocMeta meta globalMeta)
  adjustMetaVariables (adjust base) variables meta
  where
    adjust base path = do
      let apath = makeProjectPath base (toString path)
      isDir <- liftIO $ Dir.doesDirectoryExist apath
      isFile <- liftIO $ Dir.doesFileExist apath
      if isFile || isDir
        then do
          return $ toText apath
        else do
          return path

-- | Adjusts meta data values that reference files needed at run-time (by some
-- plugin, presumeably) and at compile-time (by some template). Lists of these
-- variables can be specified in the meta data.
needMetaTargets :: FilePath -> Meta -> Action Meta
needMetaTargets base meta =
  adjustMetaVariables (adjustR base) (runtimePathVariables meta) meta
    >>= adjustMetaVariables (adjustC base) (compiletimePathVariables meta)
  where
    adjustR base path = do
      let stringPath = toString path
      isFile <- liftIO $ Dir.doesFileExist stringPath
      if isFile
        then do
          need [publicDir </> stringPath]
          let relativePath = makeRelativeTo base stringPath
          return $ toText relativePath
        else do
          return path
    adjustC base path = do
      let pathString = toString path
      isDir <- liftIO $ Dir.doesDirectoryExist pathString
      unless isDir (need [pathString])
      return path

adjustMetaVariables :: (MonadFail m, MonadIO m) => (Text -> m Text) -> [Text] -> Meta -> m Meta
adjustMetaVariables action keys meta = foldM func meta keys
  where
    func meta key = adjustMetaStringsBelowM action key meta

-- | Merges global meta data into the document. Document meta values have
-- preference.
mergeDocumentMeta :: Meta -> Pandoc -> Action Pandoc
mergeDocumentMeta globalMeta (Pandoc docMeta content) = do
  let combinedMeta = mergePandocMeta docMeta globalMeta
  return (Pandoc combinedMeta content)

pathVariables :: Meta -> [Text]
pathVariables meta =
  List.nub $ compiletimePathVariables meta <> runtimePathVariables meta

-- TODO: what does this even mean?
compiletimePathVariables :: Meta -> [Text]
compiletimePathVariables meta =
  List.nub
    $ [ "csl",
        "bibliography",
        "meta-data",
        "static-resource-dirs",
        "static-resources",
        "extra-highlight-syntax"
      ]
    <> lookupMetaOrElse [] "compiletime-path-variables" meta

-- TODO: what does this even mean?
runtimePathVariables :: Meta -> [Text]
runtimePathVariables meta =
  List.nub $ ["template"] <> lookupMetaOrElse [] "runtime-path-variables" meta
