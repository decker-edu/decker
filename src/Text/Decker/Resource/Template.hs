{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
      (Disposition Handout Latex, "template/handout.tex"),
      (Disposition Index Html, "template/index.html"),
      (Disposition Index Latex, "template/index.tex")
    ]

templateFile :: Disposition -> FilePath
templateFile disp =
  case Map.lookup disp templateFiles of
    Just file -> file
    Nothing ->
      bug $ ResourceException $ "Unsupported disposition: " <> show disp

defaultMetaPath = "template/default.yaml"

data TemplateError = NotFound FilePath | NotCompiled Text

readTemplate :: Meta -> FilePath -> IO (Template Text, [FilePath])
readTemplate meta file = do
  resources@(Resources decker pack) <- deckerResources meta
  case pack of
    None -> readDefaultTemplate resources
    _ -> do
      fromPack <- readTemplate' pack resources
      case fromPack of
        Right result -> return result
        Left (NotFound file) -> readDefaultTemplate resources
        Left (NotCompiled err) -> error $ "# compilation of pack template failed: " <> toText file <> ",  error: " <> err
  where
    readTemplate' :: Source -> Resources -> IO (Either TemplateError (Template Text, [FilePath]))
    readTemplate' source resources = do
      catch
        ( do
            (text, needed) <- readTemplateText source
            compiled <- runReaderT (compileTemplate "" text) resources
            case compiled of
              Right compiled -> return $ Right (compiled, needed)
              Left errmsg -> return $ Left $ NotCompiled (toText errmsg)
        )
        (\(SomeException e) -> return $ Left $ NotFound file)
    readDefaultTemplate resources@(Resources decker _) = do
      fromDecker <- readTemplate' decker resources
      case fromDecker of
        Right result -> return result
        Left (NotFound file) -> error $ "# default template not found: " <> toText file
        Left (NotCompiled err) -> error $ "# compilation of default template failed: " <> toText file <> ",  error: " <> err
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
  return $ mergePandocMeta packMeta deckerMeta

readTemplateMeta' :: Source -> Action Meta
readTemplateMeta' (DeckerExecutable baseDir) = do
  executable <- liftIO getExecutablePath
  putInfo $ "# extracting meta data from: " <> executable
  liftIO $
    toPandocMeta <$> (extractEntry (baseDir </> defaultMetaPath) executable >>= decodeThrow)
readTemplateMeta' (LocalZip zipPath) = do
  putInfo $ "# extracting meta data from: " <> zipPath
  need [zipPath]
  liftIO $
    toPandocMeta <$> (extractEntry defaultMetaPath zipPath >>= decodeThrow)
readTemplateMeta' (LocalDir baseDir) = do
  let defaultMeta = baseDir </> defaultMetaPath
  putInfo $ "# loading meta data from: " <> defaultMeta
  need [defaultMeta]
  liftIO $ fromRight nullMeta <$> readMetaDataFile defaultMeta
readTemplateMeta' None = do
  putInfo "# no pack, no meta data"
  return nullMeta

type SourceM = ReaderT Resources IO

instance TemplateMonad SourceM where
  getPartial name = do
    (Resources decker pack) <- ask
    partial <- getIt pack
    case partial of
      Just partial -> return partial
      Nothing -> fromMaybe "" <$> getIt decker
    where
      getIt :: Source -> SourceM (Maybe Text)
      getIt source =
        liftIO $
          catch
            ( case source of
                DeckerExecutable base -> do
                  deckerExecutable <- getExecutablePath
                  -- putStrLn $ "# reading partial from: " <> ("template" </> name) <> " " <> deckerExecutable
                  Just . decodeUtf8 <$> extractEntry (base </> "template" </> name) deckerExecutable
                LocalDir base -> do
                  -- putStrLn $ "# reading partial from: " <> (base </> "template" </> name)
                  Just <$> Text.readFile (base </> "template" </> name)
                LocalZip zip -> do
                  -- putStrLn $ "# reading partial from: " <> ("template" </> name) <> " " <> zip
                  Just . decodeUtf8 <$> extractEntry ("template" </> name) zip
                None -> return Nothing
            )
            ( \(SomeException e) -> do
                -- putStrLn $ "# reading failed: " <> show e
                return Nothing
            )
