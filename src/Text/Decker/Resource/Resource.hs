-- | Author: Henrik Tramberend <henrik@tramberend.de>
-- Author: Armin Bernstetter <armin.bernstetter@uni-wuerzburg.de>
-- This module is an interface that provides transparent access to the resources
-- Depending on specification in "decker-meta.yaml" the source of the resource folder is chosen
-- Everything that is copying or linking Resource folders needs to be moved here
-- 
module Text.Decker.Resource.Resource
  ( mapResources
  , extractResources
  , deckerResourceDir
  , writeExampleProject
  , writeTutorialProject
  , copyDir
  , copyResource
  , linkResource
  , urlToFilePathIfLocal
  -- * Provisioning
  , provisionResources
  , provisionResource
  , provisionMetaResource
  , provisionTemplateOverrideSupport
  , provisionTemplateOverrideSupportTopLevel
  -- * keys
  , elementAttributes
  , runtimeMetaKeys
  , compiletimeMetaKeys
  , templateOverrideMetaKeys
  , metaKeys
  ) where

import System.Decker.OS
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Project.Project
import Text.Decker.Project.Shake

import Codec.Archive.Zip
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.State
import qualified Data.Map.Lazy as Map
import Data.Map.Strict (size)
import Development.Shake hiding (Resource)
import Network.URI
import qualified System.Directory as Dir
import System.Environment
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Walk

-- | These resources are needed at runtime. If they are specified as local URLs,
-- the resource must exists at compile time. Remote URLs are passed through
-- unchanged.
elementAttributes :: [String]
elementAttributes =
  [ "src"
  , "data-src"
  , "data-markdown"
  , "data-background-video"
  , "data-background-image"
  , "data-background-iframe"
  ]

-- | Resources in meta data that are needed at compile time. They have to be
-- specified as local URLs and must exist.
runtimeMetaKeys :: [String]
runtimeMetaKeys = ["css"]

templateOverrideMetaKeys :: [String]
templateOverrideMetaKeys = ["template"]

compiletimeMetaKeys :: [String]
compiletimeMetaKeys = ["bibliography", "csl", "citation-abbreviations"]

metaKeys :: [String]
metaKeys = runtimeMetaKeys ++ compiletimeMetaKeys ++ templateOverrideMetaKeys

-- import Text.Regex.TDFA
-- | Extract resources from the executable into the XDG data directory.
extractResources :: IO ()
extractResources = do
  deckerExecutable <- getExecutablePath
  dataDir <- deckerResourceDir
  exists <- Dir.doesDirectoryExist dataDir
  unless exists $ do
    numFiles <- withArchive deckerExecutable getEntries
    unless ((size numFiles) > 0) $
      throw $ ResourceException "No resource zip found in decker executable."
    Dir.createDirectoryIfMissing True dataDir
    withArchive deckerExecutable (unpackInto dataDir)
    putStrLn $ "# resources extracted to " ++ dataDir

-- | Write the example project to the current folder
writeExampleProject :: IO ()
writeExampleProject = writeResourceFiles "example" "."

-- | Write the tutorial project to the current folder
writeTutorialProject :: IO ()
writeTutorialProject = writeResourceFiles "tutorial" "."

writeResourceFiles :: FilePath -> FilePath -> IO ()
writeResourceFiles prefix destDir = do
  dataDir <- deckerResourceDir
  let src = dataDir </> prefix
  copyDir src destDir

-- | Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (Dir.doesDirectoryExist src) $
    throw (userError "src does not exist or is not a directory")
  dstExists <- Dir.doesDirectoryExist dst
  if dstExists && (last (splitPath src) /= last (splitPath dst))
    then copyDir src (dst </> last (splitPath src))
    else do
      Dir.createDirectoryIfMissing True dst
      contents <- Dir.listDirectory src
      forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- Dir.doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else copyFileIfNewer srcPath dstPath

-- | Copies the src to dst if src is newer or dst does not exist. Creates
-- missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst =
  whenM (fileIsNewer src dst) $ do
    Dir.createDirectoryIfMissing True (takeDirectory dst)
    Dir.copyFile src dst

fileIsNewer :: FilePath -> FilePath -> IO Bool
fileIsNewer a b = do
  aexists <- Dir.doesFileExist a
  bexists <- Dir.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- Dir.getModificationTime a
             bt <- Dir.getModificationTime b
             return (at > bt)
           else return False
    else return aexists

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
     FilePath -> Provisioning -> (String, FilePath) -> Action FilePath
provisionMetaResource base method (key, url)
  | key `elem` runtimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    provisionResource base method filePath
provisionMetaResource base method (key, url)
  | key `elem` templateOverrideMetaKeys = do
    cwd <- liftIO $ Dir.getCurrentDirectory
    filePath <- urlToFilePathIfLocal cwd url
    provisionTemplateOverrideSupportTopLevel cwd method filePath
provisionMetaResource base _ (key, url)
  | key `elem` compiletimeMetaKeys = do
    filePath <- urlToFilePathIfLocal base url
    need [filePath]
    return filePath
provisionMetaResource _ _ (key, url) = return url

provisionTemplateOverrideSupport ::
     FilePath -> Provisioning -> FilePath -> Action ()
provisionTemplateOverrideSupport base method url = do
  exists <- liftIO $ Dir.doesDirectoryExist url
  if exists
    then liftIO (Dir.listDirectory url) >>= mapM_ recurseProvision
    else do
      need [url]
      provisionResource base method url
      return ()
  where
    recurseProvision x = provisionTemplateOverrideSupport url method (url </> x)

provisionTemplateOverrideSupportTopLevel ::
     FilePath -> Provisioning -> FilePath -> Action FilePath
provisionTemplateOverrideSupportTopLevel base method url = do
  liftIO (Dir.listDirectory url) >>= filterM dirFilter >>=
    mapM_ recurseProvision
  return $ url
  where
    dirFilter x = liftIO $ Dir.doesDirectoryExist (url </> x)
    recurseProvision x = provisionTemplateOverrideSupport url method (url </> x)

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
provisionResource :: FilePath -> Provisioning -> FilePath -> Action FilePath
provisionResource base method filePath =
  case parseRelativeReference filePath of
    Nothing ->
      if hasDrive filePath
        then do
          dirs <- projectDirsA
          let resource =
                Resource
                  { sourceFile = filePath
                  , publicFile =
                      (dirs ^. public) </>
                      makeRelativeTo (dirs ^. project) filePath
                  , publicUrl = urlPath $ makeRelativeTo base filePath
                  }
          provision resource
        else return filePath
    Just uri -> do
      dirs <- projectDirsA
      let path = uriPath uri
      fileExists <- doesFileExist path
      if fileExists
        then do
          need [path]
          let resource = resourcePaths dirs base uri
          provision resource
        else throw $ ResourceException $ "resource does not exist: " ++ path
  where
    provision resource = do
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
      return absPath

-- TODO: provisionResources could stay here since it uses Pandoc/Decker Pandoc
-- This probably does not need to be introduced to Resources module
provisionResources :: Pandoc -> Decker Pandoc
provisionResources pandoc = do
  base <- gets basePath
  method <- gets provisioning
  lift $
    mapMetaResources (provisionMetaResource base method) pandoc >>=
    mapResources (provisionResource base method)

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
          transformed <- transform value
          return (key, transformed)
        else return kv

mapInline :: (FilePath -> Action FilePath) -> Inline -> Action Inline
mapInline transform (Image attr inlines (url, title)) = do
  a <- mapAttributes transform attr
  u <- transform url
  return $ Image a inlines (u, title)
mapInline transform lnk@(Link attr@(_, cls, _) inlines (url, title)) =
  if "resource" `elem` cls
    then do
      a <- mapAttributes transform attr
      u <- transform url
      return (Link a inlines (u, title))
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
     ((String, FilePath) -> Action FilePath) -> Pandoc -> Action Pandoc
mapMetaResources transform (Pandoc (Meta kvmap) blocks) = do
  mapped <- mapM mapMeta $ Map.toList kvmap
  return $ Pandoc (Meta $ Map.fromList mapped) blocks
  where
    mapMeta (k, MetaString v)
      | k `elem` metaKeys = do
        transformed <- transform (k, v)
        return (k, MetaString transformed)
    mapMeta (k, MetaInlines inlines)
      | k `elem` metaKeys = do
        transformed <- transform (k, stringify inlines)
        return (k, MetaString transformed)
    mapMeta (k, MetaList l)
      | k `elem` metaKeys = do
        transformed <- mapM (mapMetaList k) l
        return (k, MetaList transformed)
    mapMeta kv = return kv
    mapMetaList k (MetaString v) = MetaString <$> transform (k, v)
    mapMetaList k (MetaInlines inlines) =
      MetaString <$> transform (k, stringify inlines)
    mapMetaList _ v = return v
