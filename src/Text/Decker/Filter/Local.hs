{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Local where

import Control.Monad.Catch
import Data.Digest.Pure.MD5
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.IO.Unsafe (unsafePerformIO)
import Relude
import System.Random
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty qualified as Pretty
import Text.Blaze.Html.Renderer.Text qualified as Text
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Internal (Attributable)
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Pandoc hiding (lookupMeta)
import Text.URI (URI)
import Text.URI qualified as URI

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

class RawHtml a where
  rawHtml :: Text -> a
  rawHtml' :: Html -> a
  rawHtml' = rawHtml . toStrict . Text.renderHtml

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

iframeExt = ["html", "htm", "php"]

codeExt = ["cpp", "java", "hs", "js"]

pdfExt = ["pdf"]

svgExt = ["svg"]

yamlExt = ["yaml", "yml"]

renderExt = ["dot", "gnuplot", "tex", "plantuml"]

javascriptExt = ["js"]

mviewExt = ["off", "obj", "stl", "ply", "pmp", "xyz", "agi"]

geogebraExt = ["ggb"]

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
  | GeogebraT
  | RenderT
  | JavascriptT
  | StreamT
  | ExamQuestT
  deriving (Show, Eq, Ord)

classifyMedia :: URI -> Attr -> MediaT
classifyMedia uri (_, classes, _) =
  let ext = uriPathExtension uri
      scheme = uriScheme uri
   in if
          | "code" `elem` classes -> CodeT
          | ext `maybeElem` renderExt && "render" `elem` classes -> RenderT
          | ext `maybeElem` javascriptExt && "run" `elem` classes -> JavascriptT
          | ext `maybeElem` svgExt && "embed" `elem` classes -> EmbedSvgT
          | ext `maybeElem` yamlExt && "question" `elem` classes -> ExamQuestT
          | ext `maybeElem` imageExt || "image" `elem` classes -> ImageT
          | ext `maybeElem` videoExt || "video" `elem` classes -> VideoT
          | ext `maybeElem` iframeExt || "iframe" `elem` classes -> IframeT
          | ext `maybeElem` pdfExt || "pdf" `elem` classes -> PdfT
          | ext `maybeElem` mviewExt || "mview" `elem` classes -> MviewT
          | ext `maybeElem` geogebraExt || "geogebra" `elem` classes -> GeogebraT
          | ext `maybeElem` audioExt || "audio" `elem` classes -> AudioT
          | scheme `maybeElem` streamScheme -> StreamT
          | otherwise -> ImageT

maybeElem :: Eq a => Maybe a -> [a] -> Bool
maybeElem (Just x) xs = x `elem` xs
maybeElem Nothing _ = False

renderHtml :: RawHtml a => Html -> Filter a
renderHtml html = do
  pretty <- lookupMetaOrElse False "decker.filter.pretty" <$> gets meta
  return $
    rawHtml $
      toText $
        if pretty
          then toText $ Pretty.renderHtml html
          else fromLazy $ Text.renderHtml html

booleanAttribs =
  [ "allowfullscreen",
    "async",
    "autofocus",
    "autoplay",
    "checked",
    "controls",
    "default",
    "defer",
    "disabled",
    "formnovalidate",
    "hidden",
    "inert",
    "ismap",
    "itemscope",
    "loop",
    "multiple",
    "muted",
    "novalidate",
    "open",
    "readonly",
    "required",
    "reversed",
    "scoped",
    "seamless",
    "selected",
    "typemustmatch"
  ]

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) =
  foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (handleBoolean k v))
  where
    handleBoolean k v =
      if k `elem` booleanAttribs
        then H.toValue k
        else H.toValue v

mkFigureTag :: Html -> Html -> Attr -> Html
mkFigureTag content caption (id, cs, kvs) =
  H.figure
    !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    !* kvs
    $ do
      content
      H.figcaption ! A.class_ "decker" $ caption

-- | Renders a list of inlines to Text.
inlinesToMarkdown :: [Inline] -> Filter Text
inlinesToMarkdown [] = return ""
inlinesToMarkdown inlines = do
  FilterState meta _ _ <- get
  liftIO $ runIOorExplode (writeMarkdown writerHtmlOptions (Pandoc nullMeta [Plain inlines]))

-- | Renders a list of inlines to HTML.
inlinesToHtml :: [Inline] -> Filter Html
inlinesToHtml [] = return $ toHtml ("" :: Text)
inlinesToHtml inlines = blocksToHtml [Plain inlines]

-- | Renders a list of blocks to HTML.
blocksToHtml :: [Block] -> Filter Html
blocksToHtml [] = return $ toHtml ("" :: Text)
blocksToHtml blocks = do
  FilterState meta _ _ <- get
  liftIO $ runIOorExplode (writeHtml5 writerHtmlOptions (Pandoc meta blocks))

-- | Renders a list of blocks to Markdown.
blocksToMarkdown :: [Block] -> Filter Text
blocksToMarkdown [] = return ""
blocksToMarkdown blocks = do
  FilterState meta _ _ <- get
  liftIO $ runIOorExplode (writeMarkdown writerHtmlOptions (Pandoc meta blocks))

writerHtmlOptions =
  def
    { writerTemplate = Nothing,
      writerHTMLMathMethod = MathJax "Handled by reveal.js in the template",
      writerExtensions =
        (enableExtension Ext_auto_identifiers . enableExtension Ext_emoji)
          pandocExtensions
    }

-- | Renders a list of inlines to HTML. IO version that has no access
-- to the real meta data and the real writer options.
inlinesToHtml' :: [Inline] -> Html
inlinesToHtml' [] = toHtml ("" :: Text)
inlinesToHtml' inlines = blocksToHtml' [Plain inlines]

-- | Renders a list of blocks to HTML. IO version that has no access
-- to the real meta data and the real writer options, nor the file system.
blocksToHtml' :: [Block] -> Html
blocksToHtml' [] = toHtml ("" :: Text)
blocksToHtml' blocks = do
  case runPure (writeHtml5 writerHtmlOptions (Pandoc nullMeta blocks)) of
    Right html -> html
    Left err -> bug $ PandocException $ "BUG: " <> show err

instance H.ToMarkup Inline where
  toMarkup inline = inlinesToHtml' [inline]

instance H.ToMarkup [Inline] where
  toMarkup = inlinesToHtml'

instance H.ToMarkup Block where
  toMarkup block = blocksToHtml' [block]

instance H.ToMarkup [Block] where
  toMarkup = blocksToHtml'

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
      | URI.unRText rtext `notElem` ["file", "public"] -> return False
    _ -> return True

-- | Transforms a URL and handle local and remote URLs differently.
transformUrl :: Text -> Text -> Filter URI
transformUrl url ext = do
  uri <- URI.mkURI url
  transformUri uri ext

transformUri :: URI -> Text -> Filter URI
transformUri uri ext = do
  base <- lookupMetaOrFail "decker.base-dir" <$> gets meta
  case uriScheme uri of
    Just "public" -> do
      targetUri base (setUriScheme "" uri)
    Nothing -> do
      if null (uriFilePath uri)
        then return uri
        else do
          source <- addPathExtension ext uri
          needFile $ targetFilePath source
          targetUri base source
    _ -> do
      modifyMeta (addMetaValue "decker.filter.links" (URI.render uri))
      return uri

-- | Adds a remote URL to the `decker.filter.links` list in the meta data.
processRemoteUri :: URI -> Filter URI
processRemoteUri uri = do
  modifyMeta (addMetaValue "decker.filter.links" (URI.render uri))
  return uri

-- | Applies the modification function f to the meta data in the filter
-- state.
modifyMeta :: (Meta -> Meta) -> Filter ()
modifyMeta f = modify (\s -> s {meta = f (meta s)})

checkAbsoluteUri :: MonadThrow m => URI -> m ()
checkAbsoluteUri uri =
  unless (URI.isPathAbsolute uri) $
    throwM $
      InternalException $
        "relative path detected in URI: " <> show uri

needFile :: FilePath -> Filter ()
needFile path = modifyMeta (addMetaValue "decker.filter.resources" path)

resolveFileUri :: URI -> Filter FilePath
resolveFileUri uri = do
  docBase <- lookupMetaOrFail "decker.base-dir" <$> gets meta
  return $ makeProjectPath docBase $ uriFilePath uri

setMeta :: Text -> Text -> Filter ()
setMeta key value = modifyMeta (setMetaValue key (MetaString value))

getMeta :: Text -> Text -> Filter Text
getMeta key def = lookupMetaOrElse def key <$> gets meta

getMetaS :: Text -> String -> Filter String
getMetaS key def = toString <$> getMeta key (toText def)

hash9String :: String -> String
hash9String text = take 9 $ show $ md5 $ encodeUtf8 text

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text

randomId :: IO Text
randomId = Text.pack . take 9 . show . md5 . show <$> (randomIO :: IO Int)

{-# NOINLINE id9 #-}
id9 :: Text
id9 = unsafePerformIO randomId

single :: a -> [a]
single x = [x]
