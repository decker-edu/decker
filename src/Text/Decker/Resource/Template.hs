{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Resource.Template
  ( TemplateSource
  , TemplateCache
  , calcTemplateSource
  , copySupportFiles
  , readTemplate
  , readTemplateMeta
  , templateFile
  ) where

import Control.Monad

import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
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
import Text.Decker.Internal.URI
import Text.Decker.Resource.Zip
import Text.Pandoc hiding (getTemplate, lookupMeta)
import Text.URI (URI)
import qualified Text.URI as URI

{- | Defines the interface to template packs that can be selected at runtime. -}
data TemplateSource
  = DeckerExecutable
  | LocalDir FilePath
  | LocalZip FilePath
  | Unsupported Text
  deriving (Ord, Eq, Show, Read)

partialDir :: TemplateSource -> FilePath
partialDir (LocalDir path) = path </> "template" </> "deck.html"
partialDir _ = ""

type TemplateCache = FilePath -> Action (Template Text)

templateFiles =
  Map.fromList
    [ (Disposition Deck Html, "template/deck.html")
    , (Disposition Deck Markdown, "template/deck.md")
    , (Disposition Page Html, "template/page.html")
    , (Disposition Page Latex, "template/page.tex")
    , (Disposition Handout Html, "template/handout.html")
    , (Disposition Handout Latex, "template/handout.tex")
    ]

templateFile :: Disposition -> FilePath
templateFile disp =
  case Map.lookup disp templateFiles of
    Just file -> file
    Nothing ->
      bug $ ResourceException $ "Unsupported disposition: " <> show disp

parseTemplateUri :: URI -> TemplateSource
parseTemplateUri uri =
  let ext = uriPathExtension uri
      scheme = uriScheme uri
      base = uriPath uri
   in if | scheme == Just "exe" && base == "" -> DeckerExecutable
         | scheme == Nothing && (Text.toLower <$> ext) == Just "zip" ->
           LocalZip $ toString base
         | scheme == Nothing -> LocalDir $ toString base
         | otherwise -> Unsupported (URI.render uri)

copySupportFiles :: TemplateSource -> Provisioning -> FilePath -> IO ()
copySupportFiles DeckerExecutable _ destination = do
  deckerExecutable <- getExecutablePath
  extractSubEntries "support" deckerExecutable (takeDirectory destination)
copySupportFiles (LocalZip zipPath) _ destination =
  extractSubEntries "support" zipPath (takeDirectory destination)
copySupportFiles (LocalDir baseDir) _ destination =
  copyDir (baseDir </> "support") destination
copySupportFiles (Unsupported uri) provisioning destination =
  bug $ ResourceException $ "Unsupported template source: " <> toString uri

defaultMetaPath = "template/default.yaml"

-- Determines which template source is in effect. Three cases.
calcTemplateSource :: Meta -> IO TemplateSource
calcTemplateSource meta =
  case lookupMeta "template-source" meta of
    Just text -> parseTemplateUri <$> URI.mkURI text
    Nothing -> do
      devRun <- isDevelopmentRun
      if devRun
        then return $ LocalDir "resource"
        else return $ DeckerExecutable

readTemplate :: Meta -> FilePath -> Action (Template Text)
readTemplate meta file = do
  templateSource <- liftIO $ calcTemplateSource meta
  text <- readTemplateText templateSource
  liftIO (handleLeft <$> compileTemplate (partialDir templateSource) text)
  where
    readTemplateText DeckerExecutable = do
      deckerExecutable <- liftIO getExecutablePath
      liftIO (decodeUtf8 <$> extractEntry file deckerExecutable)
    readTemplateText (LocalZip path) =
      liftIO (decodeUtf8 <$> extractEntry file path)
    readTemplateText (LocalDir base) = do
      let path = base </> file
      need [path]
      liftIO (Text.readFile path)
    readTemplateText (Unsupported uri) =
      bug $ ResourceException $ "Unsupported template source: " <> toString uri

readTemplateMeta :: TemplateSource -> Action Meta
readTemplateMeta DeckerExecutable = do
  executable <- liftIO getExecutablePath
  putVerbose $ "# extracting meta data from: " <> executable
  liftIO $
    toPandocMeta <$> (extractEntry defaultMetaPath executable >>= decodeThrow)
readTemplateMeta (LocalZip zipPath) = do
  putVerbose $ "# extracting meta data from: " <> zipPath
  liftIO $
    toPandocMeta <$> (extractEntry defaultMetaPath zipPath >>= decodeThrow)
readTemplateMeta (LocalDir baseDir) = do
  let defaultMeta = baseDir </> defaultMetaPath
  putVerbose $ "# loading meta data from: " <> defaultMetaPath
  need [defaultMeta]
  liftIO $ readMetaDataFile defaultMeta
readTemplateMeta (Unsupported uri) = return nullMeta
