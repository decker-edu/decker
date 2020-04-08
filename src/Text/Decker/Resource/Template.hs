{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Resource.Template
  ( DeckerTemplate(..)
  , TemplateSource
  , calcTemplateSource
  , readTemplates
  , copySupportFiles
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.URI
import Text.Decker.Resource.Zip

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Relude
import System.Environment
import System.FilePath
import Text.Pandoc hiding (getTemplate)
import qualified Text.URI as URI

{- | Defines the interface to template packs that can be selected at runtime.

-}
data TemplateSource
  = DeckerExecutable
  | LocalDir FilePath
  | LocalZip FilePath
  | Unsupported Text
  deriving (Ord, Eq, Show)

data DeckerTemplate =
  DeckerTemplate (Template Text)
                 (Maybe FilePath)
  deriving (Show)

templateFiles =
  Map.fromList
    [ (Disposition Deck Html, "template/deck.html")
    , (Disposition Deck Markdown, "template/deck.md")
    , (Disposition Page Html, "template/page.html")
    , (Disposition Page Latex, "template/page.tex")
    , (Disposition Handout Html, "template/handout.html")
    , (Disposition Handout Latex, "template/handout.tex")
    ]

calcTemplateSource :: Maybe Text -> Bool -> TemplateSource
calcTemplateSource uriStr dev =
  if dev
    then LocalDir "resource/"
    else maybe DeckerExecutable parseTemplateUri (uriStr >>= URI.mkURI)

parseTemplateUri :: URI -> TemplateSource
parseTemplateUri uri =
  let ext = uriPathExtension uri
      scheme = uriScheme uri
      base = uriPath uri
      trailing = URI.uriAuthority uri == Left True
   in if | scheme == Just "exe" -> DeckerExecutable
         | (Text.toLower <$> ext) == Just "zip" -> LocalZip $ toString base
         | trailing -> LocalDir $ toString base
         | otherwise -> Unsupported (URI.render uri)

readTemplates :: TemplateSource -> IO [(FilePath, DeckerTemplate)]
readTemplates DeckerExecutable = do
  executable <- getExecutablePath
  readTemplatesZip executable
readTemplates (LocalZip zipPath) = readTemplatesZip zipPath
readTemplates (LocalDir baseDir) = readTemplatesFs baseDir
readTemplates (Unsupported uri) = return []

readTemplatesFs :: FilePath -> IO [(FilePath, DeckerTemplate)]
readTemplatesFs dir = foldM readTemplate [] (Map.elems templateFiles)
  where
    readTemplate list path = do
      let file = dir </> path
      content <- Text.readFile file
      compiled <- handleLeft <$> compileTemplate file content
      return $ (path, DeckerTemplate compiled (Just file)) : list

readTemplatesZip :: FilePath -> IO [(FilePath, DeckerTemplate)]
readTemplatesZip archivePath = do
  entries <- extractEntryList (Map.elems templateFiles) archivePath
  forM entries compile
  where
    compile (path, content) = do
      compiled <- handleLeft <$> compileTemplate "" (decodeUtf8 content)
      return (path, DeckerTemplate compiled Nothing)

copySupportFiles :: TemplateSource -> Provisioning -> FilePath -> IO ()
copySupportFiles DeckerExecutable _ destination = do
  deckerExecutable <- getExecutablePath
  extractSubEntries "support" deckerExecutable destination
copySupportFiles (LocalZip zipPath) _ destination =
  extractSubEntries "support" zipPath destination
copySupportFiles (LocalDir baseDir) _ destination = copyDir baseDir destination
copySupportFiles (Unsupported uri) provisioning destination =
  bug $ ResourceException $ "Unsupported template source: " <> toString uri
