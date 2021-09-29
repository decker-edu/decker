{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Resource.Template
  ( TemplateCache,
    readTemplate,
    readTemplateMeta,
    templateFile,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text.IO as Text
import Data.Yaml
import Development.Shake
import Relude
import System.Environment
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Zip
import Text.DocTemplates
import Text.Pandoc hiding (getTemplate, lookupMeta)

type TemplateCache = FilePath -> Action (Template Text)

templateFiles =
  Map.fromList
    [ (Disposition Deck Html, "template/deck.html"),
      (Disposition Deck Markdown, "template/deck.md"),
      (Disposition Page Html, "template/page.html"),
      (Disposition Page Latex, "template/page.tex"),
      (Disposition Handout Html, "template/handout.html"),
      (Disposition Handout Latex, "template/handout.tex")
    ]

templateFile :: Disposition -> FilePath
templateFile disp =
  case Map.lookup disp templateFiles of
    Just file -> file
    Nothing ->
      bug $ ResourceException $ "Unsupported disposition: " <> show disp

defaultMetaPath = "template/default.yaml"

readTemplate :: Meta -> FilePath -> IO (Template Text, [FilePath])
readTemplate meta file = do
  (Resources decker pack) <- deckerResources meta
  catch (readTemplate' pack) (\(SomeException _) -> readTemplate' decker)
  where
    readTemplate' source = do
      (text, needed) <- readTemplateText source
      compiled <- handleLeft <$> runReaderT (compileTemplate "" text) source
      return (compiled, needed)
    readTemplateText (DeckerExecutable base) = do
      deckerExecutable <- getExecutablePath
      -- putStrLn $ "# reading: " <> file <> " from: " <> (deckerExecutable <> ":" <> base)
      text <- decodeUtf8 <$> extractEntry (base </> file) deckerExecutable
      return (text, [])
    readTemplateText (LocalZip path) = do
      -- putStrLn $ "# reading: " <> file <> " from: " <> path
      text <- decodeUtf8 <$> extractEntry file path
      return (text, [path])
    readTemplateText (LocalDir base) = do
      let path = base </> file
      -- putStrLn $ "# reading: " <> file <> " from: " <> path
      text <- Text.readFile path
      return (text, [path])
    readTemplateText None =
      bug $ ResourceException "Unsupported template source: None"

readTemplateMeta :: Meta -> Action Meta
readTemplateMeta meta = do
  (Resources decker pack) <- liftIO $ deckerResources meta
  deckerMeta <- readTemplateMeta' decker
  packMeta <- readTemplateMeta' pack
  return $ mergePandocMeta' packMeta deckerMeta

readTemplateMeta' :: Source -> Action Meta
readTemplateMeta' (DeckerExecutable baseDir) = do
  executable <- liftIO getExecutablePath
  putNormal $ "# extracting meta data from: " <> executable
  liftIO $
    toPandocMeta <$> (extractEntry (baseDir </> defaultMetaPath) executable >>= decodeThrow)
readTemplateMeta' (LocalZip zipPath) = do
  putNormal $ "# extracting meta data from: " <> zipPath
  need [zipPath]
  liftIO $
    toPandocMeta <$> (extractEntry defaultMetaPath zipPath >>= decodeThrow)
readTemplateMeta' (LocalDir baseDir) = do
  let defaultMeta = baseDir </> defaultMetaPath
  putNormal $ "# loading meta data from: " <> defaultMeta
  need [defaultMeta]
  liftIO $ readMetaDataFile defaultMeta
readTemplateMeta' None = do
  putNormal "# no pack, no meta data"
  return nullMeta

type SourceM = ReaderT Source IO

instance TemplateMonad SourceM where
  getPartial name = do
    source <- ask
    case source of
      DeckerExecutable base -> do
        deckerExecutable <- liftIO getExecutablePath
        liftIO $ decodeUtf8 <$> extractEntry (base </> "template" </> name) deckerExecutable
      LocalDir base ->
        liftIO $ Text.readFile (base </> "template" </> name)
      LocalZip zip -> liftIO $ decodeUtf8 <$> extractEntry ("template" </> name) zip
      None -> return ""