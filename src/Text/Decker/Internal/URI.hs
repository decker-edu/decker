{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Internal.URI where

import Control.Monad.Catch

import qualified Data.Text as Text

import Development.Shake

import Relude

import System.Directory
import System.FilePath.Posix

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.URI (URI)
import qualified Text.URI as URI

uriPathExtension :: URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing

uriPath :: URI -> Text
uriPath uri =
  URI.render
    URI.emptyURI
      { URI.uriPath = URI.uriPath uri
      , URI.uriAuthority = Left (URI.isPathAbsolute uri)
      }

absolutePathIfLocal :: FilePath -> FilePath -> Text -> IO (Maybe Text)
absolutePathIfLocal project base uriString = do
  uri <- URI.mkURI uriString
  absolute <- absoluteUriPathIfLocal project base uri
  case absolute of
    Just absolute -> return $ Just $ URI.render absolute
    Nothing -> return Nothing

makeProjectPath :: FilePath -> FilePath -> FilePath
makeProjectPath base path =
  if hasDrive path
    then dropDrive path
    else base </> path

makeProjectUriPath :: FilePath -> Text -> IO Text
makeProjectUriPath base uriString = do
  uri <- URI.mkURI uriString
  if uriScheme uri == Nothing && not (null (uriFilePath uri))
    then do
      let path = makeProjectPath base (uriFilePath uri)
      URI.render <$> setUriPath (toText path) uri
    else return uriString

makeAbsolutePath :: FilePath -> FilePath -> FilePath -> FilePath
makeAbsolutePath project base path =
  if hasDrive path
    then project </> dropDrive path
    else base </> path

uriFilePath :: URI -> FilePath
uriFilePath uri = toString (uriPath uri)

isUriPathLocal :: URI -> Bool
isUriPathLocal uri =
  let path = uriPath uri
      schemeLocal =
        case uriScheme uri of
          Just "file" -> True
          Just _ -> False
          Nothing -> True
   in schemeLocal && not (Text.null path)

absoluteUriPathIfLocal :: FilePath -> FilePath -> URI -> IO (Maybe URI)
absoluteUriPathIfLocal project docBase uri =
  handleAll (\_ -> return Nothing) $
  if | uriScheme uri == Just "public" -> return (Just uri)
     | isUriPathLocal uri ->
       do let absolute = makeAbsolutePath project docBase (uriFilePath uri)
          exists <- doesPathExist absolute
          if exists
            then Just <$> setUriPath (toText absolute) uri
            else return Nothing
     | otherwise -> return Nothing

makeAbsolutePathIfLocal :: FilePath -> FilePath -> Text -> IO Text
makeAbsolutePathIfLocal project base uriString = do
  uri <- URI.mkURI uriString
  URI.render <$> makeAbsoluteUriPathIfLocal project base uri

needUriPath :: URI -> Action ()
needUriPath uri = need [toString $ uriPath uri]

needTargetUri :: FilePath -> FilePath -> FilePath -> Text -> Action Text
needTargetUri project public base source = do
  uri <- liftIO $ URI.mkURI source
  let target = targetFilePath uri
  need [target]
  URI.render <$> (liftIO $ targetUri base uri)

targetFilePath :: URI -> FilePath
targetFilePath uri =
  let source = toString $ uriPath uri
   in publicDir </> source

targetPath :: URI -> Text
targetPath = toText . targetFilePath

targetUri :: MonadThrow m => FilePath -> URI -> m URI
targetUri base uri = do
  let source = toString $ uriPath uri
  let relative = toText $ makeRelativeTo base source
  setUriPath relative uri

makeAbsoluteUriPathIfLocal :: FilePath -> FilePath -> URI -> IO URI
makeAbsoluteUriPathIfLocal project base uri =
  fromMaybe uri <$> absoluteUriPathIfLocal project base uri

uriScheme :: URI -> Maybe Text
uriScheme uri = URI.unRText <$> URI.uriScheme uri

setUriScheme :: Text -> URI -> URI
setUriScheme scheme uri = uri {URI.uriScheme = URI.mkScheme scheme}

setUriPath :: MonadThrow m => Text -> URI -> m URI
setUriPath path uri = do
  pathUri <- URI.mkURI (Text.replace "\\" "/" path)
  return
    uri
      { URI.uriPath = URI.uriPath pathUri
      , URI.uriAuthority =
          case URI.uriAuthority uri of
            Left _ -> Left $ URI.isPathAbsolute pathUri
            auth -> auth
      }

addPathExtension :: MonadThrow m => Text -> URI -> m URI
addPathExtension ext uri =
  if not (Text.null ext)
    then setUriPath (uriPath uri <> "." <> ext) uri
    else return uri

setQuery :: MonadThrow m => [Text] -> [(Text, Text)] -> URI -> m URI
setQuery flags params uri = do
  qFlags <- map URI.QueryFlag <$> mapM URI.mkQueryKey flags
  qParams <- mapM mkParam params
  return $ uri {URI.uriQuery = qFlags <> qParams}
  where
    mkParam (k, v) = do
      qKey <- URI.mkQueryKey k
      qVal <- URI.mkQueryValue v
      return $ URI.QueryParam qKey qVal

addQueryFlag :: MonadThrow m => Text -> URI -> m URI
addQueryFlag flag uri = do
  let query = filter (not . thisFlag) $ URI.uriQuery uri
  qFlag <- URI.QueryFlag <$> URI.mkQueryKey flag
  return $ uri {URI.uriQuery = qFlag : query}
  where
    thisFlag (URI.QueryFlag rtext) = URI.unRText rtext == flag
    thisFlag _ = False

addQueryParam :: MonadThrow m => (Text, Text) -> URI -> m URI
addQueryParam (key, value) uri = do
  let query = filter (not . thisParam) $ URI.uriQuery uri
  qKey <- URI.mkQueryKey key
  qVal <- URI.mkQueryValue value
  return $ uri {URI.uriQuery = URI.QueryParam qKey qVal : query}
  where
    thisParam (URI.QueryParam rtext _) = URI.unRText rtext == key
    thisParam _ = False
