{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Cache
  ( isCacheableURI
  , cacheRemoteFile
  , cacheRemoteImages
  ) where

import Common
import Exception

import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Digest.Pure.MD5
import Development.Shake.FilePath as SFP
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Network.URI
import qualified System.Directory as Dir
import System.FilePath as SF
import Text.Pandoc
import Text.Pandoc.Walk

cacheRemoteImages :: FilePath -> Pandoc -> IO Pandoc
cacheRemoteImages cacheDir = walkM cacheRemoteImage
  where
    cacheRemoteImage (Image attr inlines (url, title)) = do
      cachedFile <- cacheRemoteFile cacheDir url
      return (Image attr inlines (cachedFile, title))
    cacheRemoteImage img = return img

cacheRemoteFile :: FilePath -> String -> IO FilePath
cacheRemoteFile cacheDir url
  | isCacheableURI url = do
    let cacheFile = cacheDir </> hashURI url
    exists <- Dir.doesFileExist cacheFile
    if exists
      then return cacheFile
      else catch
             (do content <- downloadUrl url
                 Dir.createDirectoryIfMissing True cacheDir
                 LB.writeFile cacheFile content
                 return cacheFile)
             (\e -> do
                putStrLn $ "Warning: " ++ show (e :: SomeException)
                return url)
cacheRemoteFile _ url = return url

-- clearCachedFile :: FilePath -> String -> IO ()
-- clearCachedFile cacheDir url
--   | isCacheableURI url = do
--     let cacheFile = cacheDir </> hashURI url
--     exists <- Dir.doesFileExist cacheFile
--     when exists $ Dir.removeFile cacheFile
-- clearCachedFile _ _ = return ()
downloadUrl :: String -> IO LB.ByteString
downloadUrl url = do
  request <- parseRequest url
  result <- httpLBS request
  let status = getResponseStatus result
  if status == ok200
    then return $ getResponseBody result
    else throw $
         HttpException $
         "Cannot download " ++
         url ++
         " (" ++
         show (statusCode status) ++
         " " ++ B.unpack (statusMessage status) ++ ")"

isCacheableURI :: String -> Bool
isCacheableURI url =
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:", "https:"]
    Nothing -> False

hashURI :: String -> String
hashURI uri = show (md5 $ L8.pack uri) SF.<.> SF.takeExtension uri
