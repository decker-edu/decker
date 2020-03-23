{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Local where

import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta
import Text.Decker.Project.Project

import Control.Monad.Catch
import Data.Digest.Pure.MD5
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Relude
import System.Directory
import System.FilePath
import Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable)
import qualified Text.Blaze.Internal as H
import Text.Pandoc
import Text.URI (URI)
import qualified Text.URI as URI

instance H.ToMarkup Block where
  toMarkup (RawBlock (Format "html") html) =
    H.Content (H.PreEscaped (H.Text html)) ()
  toMarkup block =
    bug $ InternalException $ "toMarkup: illegal block argument: " <> show block

instance H.ToMarkup Inline where
  toMarkup (RawInline (Format "html") html) =
    H.Content (H.PreEscaped (H.Text html)) ()
  toMarkup inline =
    bug $ InternalException $ "toMarkup: illegal inline argument" <> show inline

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

class RawHtml a where
  rawHtml :: Text -> a

instance RawHtml Inline where
  rawHtml = RawInline (Format "html")

instance RawHtml [Inline] where
  rawHtml text = [RawInline (Format "html") text]

instance RawHtml Block where
  rawHtml = RawBlock (Format "html")

instance RawHtml [Block] where
  rawHtml text = [RawBlock (Format "html") text]

-- | File-extensions that should be treated as image
imageExt = ["jpg", "jpeg", "png", "gif", "tif", "tiff", "bmp", "svg"]

videoExt = ["mp4", "mov", "ogg", "avi"]

audioExt = ["mp3", "aiff", "wav"]

iframeExt = ["html", "htm"]

codeExt = ["cpp", "java", "hs", "js"]

pdfExt = ["pdf"]

svgExt = ["svg"]

renderExt = ["dot", "gnuplot"]

mviewExt = ["off", "obj", "stl", "ply", "pmp"]

streamScheme = ["youtube", "vimeo", "twitch", "veer", "veer-photo"]

data MediaT
  = ImageT
  | VideoT
  | AudioT
  | IframeT
  | CodeT
  | PdfT
  | EmbedSvgT
  | MviewT
  | RenderT
  | StreamT
  deriving (Show, Eq, Ord)

classifyMedia :: URI -> Attr -> MediaT
classifyMedia uri (_, classes, _) =
  let ext = uriPathExtension uri
      scheme = uriScheme uri
   in if | ext `maybeElem` svgExt && "embed" `elem` classes -> EmbedSvgT
         | ext `maybeElem` renderExt && "render" `elem` classes -> RenderT
         | ext `maybeElem` imageExt || "image" `elem` classes -> ImageT
         | ext `maybeElem` videoExt || "video" `elem` classes -> VideoT
         | ext `maybeElem` audioExt || "audio" `elem` classes -> AudioT
         | ext `maybeElem` iframeExt || "iframe" `elem` classes -> IframeT
         | ext `maybeElem` pdfExt || "pdf" `elem` classes -> PdfT
         | ext `maybeElem` mviewExt || "mview" `elem` classes -> MviewT
         | ext `maybeElem` codeExt || "code" `elem` classes -> CodeT
         | scheme `maybeElem` streamScheme -> StreamT
         | otherwise -> ImageT

maybeElem :: Eq a => Maybe a -> [a] -> Bool
maybeElem (Just x) xs = x `elem` xs
maybeElem Nothing _ = False

renderHtml :: RawHtml a => Html -> Filter a
renderHtml html = do
  pretty <- getMetaBoolOrElse "decker.filter.pretty" False <$> gets meta
  return $
    rawHtml $
    toText $
    if pretty
      then toText $ Pretty.renderHtml html
      else fromLazy $ Text.renderHtml html

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) = foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (H.toValue v))

mkFigureTag :: Html -> Html -> Attr -> Html
mkFigureTag content caption (id, cs, kvs) =
  H.figure !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !*
  kvs $ do
    content
    H.figcaption ! A.class_ "decker" $ caption

-- | Renders a list of inlines to Text.
inlinesToMarkdown :: [Inline] -> Text
inlinesToMarkdown [] = ""
inlinesToMarkdown inlines =
  case runPure (writeMarkdown def (Pandoc nullMeta [Plain inlines])) of
    Right html -> html
    Left err -> bug $ PandocException $ "BUG: " <> show err

-- | Renders a list of inlines to HTML.
inlinesToHtml :: [Inline] -> Filter Html
inlinesToHtml [] = return $ H.span ""
inlinesToHtml inlines = do
  FilterState options meta <- get
  case runPure (writeHtml5 options (Pandoc meta [Plain inlines])) of
    Right html -> return html
    Left err -> bug $ PandocException $ "BUG: " <> show err

readLocalUri :: URI -> Filter Text
readLocalUri uri = do
  isFile <- isFileUri uri
  if isFile
    then resolveFileUri uri >>= lift . Text.readFile
    else error $ "Cannot read from remote URL" <> URI.render uri

isFileUri :: MonadThrow m => URI -> m Bool
isFileUri uri =
  case URI.uriScheme uri of
    Just rtext
      | URI.unRText rtext /= "file" -> return False
    _ -> return True

-- | Transforms a URL and handles local and remote URLs differently.
transformUrl :: Text -> Filter URI
transformUrl url = do
  uri <- URI.mkURI url
  isFile <- isFileUri uri
  if isFile
    then processLocalUri uri
    else processRemoteUri uri

-- | Adds a remote URL to the `decker.filter.links` list in the meta data.
processRemoteUri :: URI -> Filter URI
processRemoteUri uri = do
  modifyMeta (addStringToMetaList "decker.filter.links" (URI.render uri))
  return uri

-- | Applies the modification function f to the meta data in the filter
-- state.
modifyMeta :: (Meta -> Meta) -> Filter ()
modifyMeta f = modify (\s -> s {meta = f (meta s)})

processLocalUri :: URI -> Filter URI
processLocalUri uri = do
  cwd <- toText <$> liftIO getCurrentDirectory
  -- | The project relative (!) document directory from which this is called.
  docBaseDir <- toString <$> getMeta "decker.base-dir" cwd
  topBaseDir <- toString <$> getMeta "decker.top-base-dir" cwd
  -- | The absolute (!) project directory from which this is called.
  projectDir <- toString <$> getMeta "decker.project-dir" cwd
  -- | The absolute (!) public directory where everything is published to.
  publicDir <- toString <$> getMeta "decker.public-dir" (cwd <> "/public")
  -- | The path component from the URI
  urlPath <- toString <$> uriPath uri
  -- | Interpret urlPath either project relative or document relative,
  -- depending on the leading slash.
  let relPath =
        normalise $
        if hasDrive urlPath
          then dropDrive urlPath
          else makeRelative projectDir docBaseDir </> urlPath
  let sourcePath = projectDir </> relPath
  let topPath = projectDir </> topBaseDir
  let targetPath = publicDir </> relPath
  let publicRelPath = makeRelativeTo topBaseDir sourcePath
  publicUri <- setUriPath (toText publicRelPath) uri
  let publicUrl = URI.render publicUri
  exists <- liftIO $ doesFileExist sourcePath
  if exists
    then storeResourceInfo sourcePath targetPath publicUrl
    else throwM $
         ResourceException $ "Local resource does not exist: " <> relPath
  return publicUri

storeResourceInfo :: FilePath -> FilePath -> Text -> Filter ()
storeResourceInfo source target url = do
  let key = "decker" <.> "filter" <.> "resources" <.> hash9String target
  setMeta (toText $ key <.> "source") $ toText source
  setMeta (toText $ key <.> "target") $ toText target
  setMeta (toText $ key <.> "url") url

resolveFileUri :: URI -> Filter FilePath
resolveFileUri uri = do
  urlPath <- toString <$> uriPath uri
  cwd <- liftIO getCurrentDirectory
  baseDir <- toString <$> getMeta "decker.base-dir" "."
  projectDir <- toString <$> getMeta "decker.project-dir" (toText cwd)
  publicDir <-
    toString <$> getMeta "decker.public-dir" (toText $ cwd </> "public")
  urlPath <- toString <$> uriPath uri
  let relPath =
        normalise $
        if hasDrive urlPath
          then dropDrive urlPath
          else makeRelative projectDir baseDir </> urlPath
  let sourcePath = projectDir </> relPath
  return sourcePath

uriPathExtension :: URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing

uriPath :: MonadThrow m => URI -> m Text
uriPath uri =
  return $
  URI.render
    URI.emptyURI
      { URI.uriPath = URI.uriPath uri
      , URI.uriAuthority = Left (URI.isPathAbsolute uri)
      }

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

addQueryFlag :: MonadThrow m => Text -> URI -> m URI
addQueryFlag flag uri = do
  let query = filter isFlag $ URI.uriQuery uri
  qFlag <- URI.QueryFlag <$> URI.mkQueryKey flag
  return $ uri {URI.uriQuery = qFlag : query}
  where
    isFlag (URI.QueryFlag rtext) = URI.unRText rtext /= flag

addQueryParam :: MonadThrow m => (Text, Text) -> URI -> m URI
addQueryParam (key, value) uri = do
  let query = filter isParam $ URI.uriQuery uri
  qKey <- URI.mkQueryKey key
  qVal <- URI.mkQueryValue value
  return $ uri {URI.uriQuery = URI.QueryParam qKey qVal : query}
  where
    isParam (URI.QueryParam rtext _) = URI.unRText rtext /= key

setMeta :: Text -> Text -> Filter ()
setMeta key value =
  modify (\s -> s {meta = setMetaValue key (MetaString value) (meta s)})

getMeta :: Text -> Text -> Filter Text
getMeta key def = getMetaTextOrElse key def <$> gets meta

hash9String :: String -> String
hash9String text = take 9 $ show $ md5 $ encodeUtf8 text

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

single :: a -> [a]
single x = [x]
