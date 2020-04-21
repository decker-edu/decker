-- | Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
-- This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module Text.Decker.Resource.Resource
  ( mapResources
  , deckerResourceDir
  , writeExampleProject
  , writeTutorialProject
  , copyResource
  , linkResource
  , urlToFilePathIfLocal
  -- * Provisioning
  , provisionResources
  , provisionResource
  , publishResource
  ) where

import System.Decker.OS
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Project.Project
import Text.Decker.Project.Shake
import Text.Decker.Resource.Zip

import Control.Exception
import Control.Lens ((^.))
import Control.Monad.Extra
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import Development.Shake hiding (Resource)
import Network.URI
import qualified System.Directory as Dir
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Walk

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes :: [Text.Text]
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

runtimeMetaKeys :: [Text.Text]
runtimeMetaKeys = ["css", "base-css"]

compiletimeMetaKeys :: [Text.Text]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [Text.Text]
metaKeys = runtimeMetaKeys <> compiletimeMetaKeys

-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = do
  cwd <- Dir.getCurrentDirectory
  putStrLn $ "Extracting example project to " ++ cwd ++ "."
  extractResourceEntries "example" cwd

-- | Write the tutorial project to the current folder
writeTutorialProject :: IO ()
writeTutorialProject = do
  cwd <- Dir.getCurrentDirectory
  putStrLn $ "Extracting tutorial project to " ++ cwd ++ "."
  extractResourceEntries "tutorial" cwd

-- | Copies single Resource file and returns Filepath
copyResource :: Resource -> IO FilePath
copyResource resource = do
  copyFileIfNewer (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

-- | Creates SymLink to single resource file and returns Filepath
linkResource :: Resource -> IO FilePath
linkResource resource = do
  whenM
    (Dir.doesFileExist (publicFile resource))
    (Dir.removeFile (publicFile resource))
  Dir.createDirectoryIfMissing True (takeDirectory (publicFile resource))
  Dir.createFileLink (sourceFile resource) (publicFile resource)
  return (publicUrl resource)

provisionMetaResource ::
     Provisioning -> FilePath -> (Text.Text, FilePath) -> Action FilePath
provisionMetaResource method base (key, url)
  | key `elem` runtimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    provisionResource method base filePath
provisionMetaResource method base (key, url)
  | key `elem` compiletimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    need [filePath]
    return filePath
provisionMetaResource _ _ (key, url) = return url

-- | Determines if a URL can be resolved to a local file. Absolute file URLs are
-- resolved against and copied or linked to public from 
--    1. the project root 
--    2. the local filesystem root 
--
-- Relative file URLs are resolved against and copied or linked to public from 
--
--    1. the directory path of the referencing file 
--    2. the project root Copy and link operations target the public directory
--       in the project root and recreate the source directory structure. 
--
-- This function is used to provision resources that are used at presentation
--       time.
--
-- Returns a public URL relative to base
provisionResource :: Provisioning -> FilePath -> FilePath -> Action FilePath
provisionResource method base filePath = do
  dirs <- projectDirsA
  case parseRelativeReference filePath of
    Nothing ->
      if hasDrive filePath
        then do
          let resource =
                Resource
                  { sourceFile = filePath
                  , publicFile =
                      (dirs ^. public) </>
                      makeRelativeTo (dirs ^. project) filePath
                  , publicUrl = urlPath $ makeRelativeTo base filePath
                  }
          publishResource method base resource
        else return filePath
    Just uri -> do
      let path = uriPath uri
      fileExists <- doesFileExist path
      if fileExists
        then do
          need [path]
          let resource = resourcePaths dirs base uri
          publishResource method base resource
        else throw $ ResourceException $ "resource does not exist: " ++ path

publishResource :: Provisioning -> FilePath -> Resource -> Action FilePath
publishResource method base resource = do
  publicResource <- publicResourceA
  withResource publicResource 1 $
    liftIO $
    case method of
      Copy -> copyResource resource
      SymLink -> linkResource resource
      Absolute -> absRefResource resource
      Relative -> relRefResource base resource

urlToFilePathIfLocal :: FilePath -> FilePath -> Action FilePath
urlToFilePathIfLocal base uri =
  case parseRelativeReference uri of
    Nothing -> return uri
    Just relativeUri -> do
      let filePath = uriPath relativeUri
      absBase <- liftIO $ Dir.makeAbsolute base
      absRoot <- projectA
      let absPath =
            if isAbsolute filePath
              then absRoot </> makeRelative "/" filePath
              else absBase </> filePath
      return $ show $ relativeUri {uriPath = absPath}

-- TODO: provisionResources could stay here since it uses Pandoc/Decker Pandoc
-- This probably does not need to be introduced to Resources module
provisionResources :: Pandoc -> Decker Pandoc
provisionResources pandoc = do
  base <- gets basePath
  method <- gets provisioning
  lift $
    mapMetaResources (provisionMetaResource method base) pandoc >>=
    mapResources (provisionResource method base)

mapResources :: (FilePath -> Action FilePath) -> Pandoc -> Action Pandoc
mapResources transform (Pandoc meta blocks) =
  Pandoc meta <$> walkM (mapInline transform) blocks >>=
  walkM (mapBlock transform)

mapAttributes :: (FilePath -> Action FilePath) -> Attr -> Action Attr
mapAttributes transform (ident, classes, kvs) = do
  processed <- mapM mapAttr kvs
  return (ident, classes, processed)
  where
    mapAttr kv@(key, value) =
      if key `elem` elementAttributes
        then do
          transformed <- transform (Text.unpack value)
          return (key, Text.pack transformed)
        else return kv

mapInline :: (FilePath -> Action FilePath) -> Inline -> Action Inline
mapInline transform (Image attr inlines (url, title)) = do
  a <- mapAttributes transform attr
  u <- transform (Text.unpack url)
  return $ Image a inlines (Text.pack u, title)
mapInline transform lnk@(Link attr@(_, cls, _) inlines (url, title)) =
  if "resource" `elem` cls
    then do
      a <- mapAttributes transform attr
      u <- transform (Text.unpack url)
      return (Link a inlines (Text.pack u, title))
    else return lnk
mapInline transform (Span attr inlines) = do
  attribs <- mapAttributes transform attr
  return (Span attribs inlines)
mapInline transform (Code attr string) = do
  attribs <- mapAttributes transform attr
  return (Code attribs string)
mapInline _ inline = return inline

mapBlock :: (FilePath -> Action FilePath) -> Block -> Action Block
mapBlock transform (CodeBlock attr string) = do
  attribs <- mapAttributes transform attr
  return (CodeBlock attribs string)
mapBlock transform (Header n attr inlines) = do
  attribs <- mapAttributes transform attr
  return (Header n attribs inlines)
mapBlock transform (Div attr blocks) = do
  attribs <- mapAttributes transform attr
  return (Div attribs blocks)
mapBlock _ block = return block

mapMetaResources ::
     ((Text.Text, FilePath) -> Action FilePath) -> Pandoc -> Action Pandoc
mapMetaResources transform (Pandoc (Meta kvmap) blocks) = do
  mapped <- mapM mapMeta $ Map.toList kvmap
  return $ Pandoc (Meta $ Map.fromList mapped) blocks
  where
    transform' (k, v) = Text.pack <$> transform (k, Text.unpack v)
    mapMeta (k, MetaString v)
      | k `elem` metaKeys = do
        transformed <- transform' (k, v)
        return (k, MetaString transformed)
    mapMeta (k, MetaInlines inlines)
      | k `elem` metaKeys = do
        transformed <- transform' (k, stringify inlines)
        return (k, MetaString transformed)
    mapMeta (k, MetaList l)
      | k `elem` metaKeys = do
        transformed <- mapM (mapMetaList k) l
        return (k, MetaList transformed)
    mapMeta kv = return kv
    mapMetaList k (MetaString v) = MetaString <$> transform' (k, v)
    mapMetaList k (MetaInlines inlines) =
      MetaString <$> transform' (k, stringify inlines)
    mapMetaList _ v = return v
