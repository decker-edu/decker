{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Internal.URI where

import Control.Monad.Catch
import qualified Data.Text as Text
import Relude
import System.FilePath.Posix
import Text.URI (URI)
import qualified Text.URI as URI

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper

-- | Extracts the path extension from the URI path, if there is any.
uriPathExtension :: URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing

-- | Extracts the path component of a URI.
uriPath :: URI -> Text
uriPath uri =
  URI.render
    URI.emptyURI
      { URI.uriPath = URI.uriPath uri
      , URI.uriAuthority = Left (URI.isPathAbsolute uri)
      }

-- | Adjusts a path to be relative to the current project root (which is also
-- the current working directory of the process) directory. Two cases are
-- considered:
--
-- 1. Paths that start with a @/@ are considered relative to the project root.
-- The @/@ is just removed.
--
-- 2. Relative paths, are considered to be relative to `base`.
-- 
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
  if uriScheme uri == Nothing && not (null (uriFilePath uri))
    then do
      let path = makeProjectPath base (uriFilePath uri)
      URI.render <$> setUriPath (toText path) uri
    else return uriString

-- | Extracts the URI path component.
uriFilePath :: URI -> FilePath
uriFilePath uri = toString (uriPath uri)

-- | Calculates the target file path of a local file URI. The target path is
-- the source path relative to the public directory.
targetFilePath :: URI -> FilePath
targetFilePath uri =
  let source = toString $ uriPath uri
   in publicDir </> source

-- | Interprets the path component of `uri` as a local project relative path
-- and converts it to a path relative to `base`.
targetUri :: MonadThrow m => FilePath -> URI -> m URI
targetUri base uri = do
  let source = toString $ uriPath uri
  let relative = toText $ makeRelativeTo base source
  setUriPath relative uri

-- | Extracts the URI scheme fromm `uri`.
uriScheme :: URI -> Maybe Text
uriScheme uri = URI.unRText <$> URI.uriScheme uri

-- | Sets the scheme of `uri`.
setUriScheme :: Text -> URI -> URI
setUriScheme scheme uri = uri {URI.uriScheme = URI.mkScheme scheme}

-- | Sets the URI path of `uri` to `path`. Handles absolute and relative pathes
-- correctly.
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

-- | Adds the file extension `ext` to the path component of `uri`.
addPathExtension :: MonadThrow m => Text -> URI -> m URI
addPathExtension ext uri =
  if not (Text.null ext)
    then setUriPath (uriPath uri <> "." <> ext) uri
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
