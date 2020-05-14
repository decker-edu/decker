{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.URI
  ( uriPathExtension
  , uriPath
  , uriScheme
  , setUriPath
  , setQuery
  , addQueryFlag
  , addQueryParam
  , URI
  , absolutePathIfLocal
  , makeAbsolutePathIfLocal
  ) where

import Control.Monad.Catch
import qualified Data.Text as Text
import Relude
import System.Directory
import System.FilePath
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

--isUriAbsolute :: URI -> Bool
--isUriAbsolute uri = isJust (URI.uriScheme uri)

absolutePathIfLocal :: FilePath -> FilePath -> Text -> IO (Maybe Text)
absolutePathIfLocal project base uriString =
  catchAll decide (\_ -> return Nothing)
  where
    decide = do
      uri <- URI.mkURI uriString
      case uriScheme uri of
        Just "file" -> return $ Just $ uriPath uri
        Just _ -> return Nothing
        Nothing -> do
          let path = toString (uriPath uri)
          let absPath =
                if URI.isPathAbsolute uri
                  then project </> drop 1 path
                  else base </> path
          exists <- doesPathExist absPath
          return $
            if exists
              then Just (toText absPath)
              else Nothing

makeAbsolutePathIfLocal :: FilePath -> FilePath -> Text -> IO Text
makeAbsolutePathIfLocal project base uriString =
  fromMaybe uriString <$> absolutePathIfLocal project base uriString

uriScheme :: URI -> Maybe Text
uriScheme uri = URI.unRText <$> URI.uriScheme uri

setUriPath :: MonadThrow m => Text -> URI -> m URI
setUriPath path uri = do
  pathUri <- URI.mkURI path
  return
    uri
      { URI.uriPath = URI.uriPath pathUri
      , URI.uriAuthority =
          case URI.uriAuthority uri of
            Left _ -> Left $ URI.isPathAbsolute pathUri
            auth -> auth
      }

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
