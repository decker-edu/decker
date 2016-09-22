-- | Generally useful functions on pansoc data structures. Some in the IO monad.
module Pandoc
       (isCacheableURI, cacheRemoteFile,
        Pandoc.cacheRemoteImages, Pandoc.readMetaData)
       where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import Data.Digest.Pure.MD5
import qualified Data.Yaml as Y
import Development.Shake
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import Text.Pandoc
import Text.Pandoc.Walk
import Utilities
import Context
import Debug.Trace

cacheRemoteImages :: FilePath -> Pandoc -> IO Pandoc
cacheRemoteImages cacheDir pandoc = walkM cacheRemoteImage pandoc
  where cacheRemoteImage (Image attr inlines (url,title)) =
          do cachedFile <- cacheRemoteFile cacheDir url
             return (Image attr inlines (cachedFile,title))
        cacheRemoteImage img = return img

cacheRemoteFile :: FilePath -> String -> IO FilePath
cacheRemoteFile cacheDir url
  | isCacheableURI url =
    do let cacheFile = cacheDir </> hashURI url
       exists <- fileExist cacheFile
       if exists
         then return cacheFile
         else do content <- downloadUrl url
                 createDirectoryIfMissing True cacheDir
                 L.writeFile cacheFile content
                 return cacheFile
cacheRemoteFile _ url = return url

clearCachedFile :: FilePath -> String -> IO ()
clearCachedFile cacheDir url
  | isCacheableURI url =
    do let cacheFile = cacheDir </> hashURI url
       exists <- fileExist cacheFile
       when exists $ removeFile cacheFile
clearCachedFile _ _ = return ()

downloadUrl :: String -> IO L.ByteString
downloadUrl url =
  do request <- parseRequest url
     result <- httpLBS request
     let status = getResponseStatus result
     if status == ok200
        then return $ getResponseBody result
        else throw $
             HttpException $
             "Cannot download " ++ url ++ ": status: " ++ show status

hashURI :: String -> String
hashURI uri = (show $ md5 $ L.pack uri) <.> takeExtension uri

type MetaData = M.Map FilePath Y.Value

readMetaData :: [FilePath] -> IO MetaData
readMetaData metaFiles =
  do canonMetaFiles <- mapM canonicalizePath metaFiles
     aDataList <- mapM decodeYaml canonMetaFiles
     let joined = M.map joinHorizontally $ MM.toMap $ MM.fromList aDataList
     return $
       M.mapWithKey (joinVertically joined)
                    joined
  where decodeYaml
          :: FilePath -> IO (FilePath,Y.Value)
        decodeYaml file =
          do result <- Y.decodeFileEither file
             case result of
               Right object@(Y.Object _) -> return (takeDirectory file,object)
               Right _ ->
                 throw $
                 YamlException $
                 "Top-level meta value must be an object: " ++ file
               Left exception -> throw exception
        joinHorizontally :: [Y.Value] -> Y.Value
        joinHorizontally = foldl1 joinMeta
        joinVertically
          :: MetaData -> FilePath -> Y.Value -> Y.Value
        joinVertically meta dir ignore =
          if not $ equalFilePath dir "/"
             then let up =
                        joinVertically meta
                                       (takeDirectory dir)
                                       ignore
                  in maybe up
                           (joinMeta up)
                           (M.lookup dir meta)
             else Y.Object (H.fromList [])
        joinMeta :: Y.Value -> Y.Value -> Y.Value
        joinMeta (Y.Object old) (Y.Object new) = Y.Object (H.union new old)
        joinMeta _ _ = throw $ YamlException "Can only join YAML objects."
