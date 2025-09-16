{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.URI where

import Control.Monad.Catch
import qualified Data.Text as Text
import Network.HTTP.Types (urlDecode, urlEncode)
import Relude
import System.FilePath.Posix
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper
import Text.URI (URI)
import qualified Text.URI as URI
import Network.URI.Encode


renderUriDecode :: URI -> Text
renderUriDecode  = decodeText . URI.render 

-- | Extracts the path extension from the URI path, if there is any.
uriPathExtension :: URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing

-- | Extracts the path component of a URI. Be careful not to urlencode anything.
uriPath :: URI -> Text
uriPath uri =
  let root = if URI.isPathAbsolute uri then "/" else ""
   in case URI.uriPath uri of
        (Just (False, pieces)) -> root <> path pieces
        (Just (True, pieces)) -> root <> path pieces <> "/"
        Nothing -> ""
  where
    path = Text.intercalate "/" . map URI.unRText . toList

-- URI.render
--   URI.emptyURI
--     { URI.uriPath = URI.uriPath uri,
--       URI.uriAuthority = Left (URI.isPathAbsolute uri)
--     }

-- | Adjusts a path to be relative to the current project root (which is also
-- the current working directory of the process) directory. Two cases are
-- considered:
--
-- 1. Paths that start with a @/@ are considered relative to the project root.
-- The @/@ is just removed.
--
-- 2. Relative paths, are considered to be relative to `base`.
makeProjectPath :: FilePath -> FilePath -> FilePath
makeProjectPath base path =
  if hasDrive path
    then dropDrive path
    else base </> path

-- | A version of `makeProjectPath` that works on URIs. Only relative file URIs
-- with a non-empty path component are considered. Fragment and query parts of
-- the URI are preserved.
makeProjectUriPath :: FilePath -> Text -> IO Text
makeProjectUriPath base uriString = do
  uri <- URI.mkURI uriString
  case uriScheme uri of
    Nothing | not (null (uriFilePath uri)) -> do
      let path = makeProjectPath base (uriFilePath uri)
      uri <- URI.render <$> setUriFilePath (toText path) uri
      -- putStrLn $ "base: " <> base
      -- putStrLn $ "  in: " <> show uriString
      -- putStrLn $ " out: " <> show uri
      return uri
    _ -> return uriString

isAbsoluteUri :: MonadThrow m => Text -> m Bool
isAbsoluteUri uriString = do
  uri <- URI.mkURI uriString
  return $ not (uriScheme uri == Nothing && not (Text.null (uriPath uri)))

-- | Extracts the URI path component and urldecodes it.
uriFilePath :: URI -> FilePath
uriFilePath = toString . uriDecode' . uriPath

-- uriFilePath = toString . uriPath

-- | Calculates the target file path of a local file URI. The target path is
-- the source path relative to the public directory.
targetFilePath :: URI -> FilePath
targetFilePath uri =
  publicDir </> uriFilePath uri

-- | Interprets the path component of `uri` as a local project relative path
-- and converts it to a path relative to `base`.
targetUri :: MonadThrow m => FilePath -> URI -> m URI
targetUri base uri = do
  let relative = toText $ makeRelativeTo base $ uriFilePath uri
  setUriFilePath relative uri

-- | Extracts the URI scheme fromm `uri`.
uriScheme :: URI -> Maybe Text
uriScheme uri = URI.unRText <$> URI.uriScheme uri

-- | Sets the scheme of `uri`.
setUriScheme :: Text -> URI -> URI
setUriScheme scheme uri = uri {URI.uriScheme = URI.mkScheme scheme}

uriEncode' :: Text -> Text
-- uriEncode' = decodeUtf8 . urlEncode False . encodeUtf8
uriEncode' = Text.intercalate "/" . map (decodeUtf8 . urlEncode False . encodeUtf8) . Text.splitOn "/"

uriDecode' :: Text -> Text
uriDecode' = decodeUtf8 . urlDecode False . encodeUtf8

-- | Sets the URI path of `uri` to `path`. Handles absolute and relative pathes
-- correctly. The path component gets URI-encoded.
setUriFilePath :: MonadThrow m => Text -> URI -> m URI
setUriFilePath path uri = do
  pathUri <- URI.mkURI (uriEncode' path)
  return
    uri
      { URI.uriPath = URI.uriPath pathUri,
        URI.uriAuthority =
          case URI.uriAuthority uri of
            Left _ -> Left $ URI.isPathAbsolute pathUri
            auth -> auth
      }

-- | Adds the file extension `ext` to the path component of `uri`.
addPathExtension :: MonadThrow m => Text -> URI -> m URI
addPathExtension ext uri =
  if not (Text.null ext)
    then setUriFilePath (uriPath uri <> "." <> ext) uri
    else return uri

-- | Sets the query component of `uri` to the combination of `flags` and
-- `params`.
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

-- | Adds `flag` to the query component of `uri`, if it is not a duplicate.
addQueryFlag :: MonadThrow m => Text -> URI -> m URI
addQueryFlag flag uri = do
  let query = filter (not . thisFlag) $ URI.uriQuery uri
  qFlag <- URI.QueryFlag <$> URI.mkQueryKey flag
  return $ uri {URI.uriQuery = qFlag : query}
  where
    thisFlag (URI.QueryFlag rtext) = URI.unRText rtext == flag
    thisFlag _ = False

-- | Adds a parameter to the query component of `uri`. If the parameter already
-- exists, its value is overwritten.
addQueryParam :: MonadThrow m => (Text, Text) -> URI -> m URI
addQueryParam (key, value) uri = do
  let query = filter (not . thisParam) $ URI.uriQuery uri
  qKey <- URI.mkQueryKey key
  qVal <- URI.mkQueryValue value
  return $ uri {URI.uriQuery = URI.QueryParam qKey qVal : query}
  where
    thisParam (URI.QueryParam rtext _) = URI.unRText rtext == key
    thisParam _ = False
