{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Image where

import Control.Monad.Catch
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Relude
import System.Directory
import System.FilePath.Posix
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.CRC32
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Streaming
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Pandoc hiding (lookupMeta)
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

-- |  Generates an HTML error message for Image inlines from the image
--  source and the actual exception. The Image element is rendered back
--  to Markdown format and included in the error message.
inlineError :: Inline -> SomeException -> Filter Inline
inlineError img@Image {} (SomeException e) = do
  imgMarkup <- inlinesToMarkdown [img]
  renderHtml $
    H.div ! A.class_ "decker image error" $ do
      H.h2 ! A.class_ "title" $ do
        H.i ! A.class_ "fa fa-exclamation-triangle" $ ""
        H.text " Decker error"
      H.p ! A.class_ "message" $ toHtml (displayException e)
      H.p $ H.text "encountered while processing"
      H.pre ! A.class_ "markup" $ H.code ! A.class_ "markup" $ toHtml imgMarkup
inlineError _ _ = bug $ InternalException "inlineError: non image argument "

blockError :: Block -> SomeException -> Filter Block
blockError code@CodeBlock {} (SomeException e) = do
  codeMarkup <- blocksToMarkdown [code]
  renderHtml $
    H.div ! A.class_ "decker image error" $ do
      H.h2 ! A.class_ "title" $ do
        H.i ! A.class_ "fa fa-exclamation-triangle" $ ""
        H.text " Decker error"
      H.p ! A.class_ "message" $ toHtml (displayException e)
      H.p $ H.text "encountered while processing"
      H.pre ! A.class_ "markup" $ H.code ! A.class_ "markup" $ toHtml codeMarkup
blockError _ _ = bug $ InternalException "blockError: non code argument "

imageTransformers :: Map MediaT (URI -> [Inline] -> Attrib Html)
imageTransformers =
  Map.fromList
    [ (EmbedSvgT, svgHtml),
      (PdfT, objectHtml "application/pdf"),
      (MviewT, mviewHtml),
      (IframeT, iframeHtml),
      (ImageT, imageHtml),
      (VideoT, videoHtml),
      (StreamT, streamHtml'),
      (AudioT, audioHtml),
      (RenderT, renderCodeHtml),
      (JavascriptT, javascriptHtml)
    ]

transformImage :: Inline -> [Inline] -> Filter Inline
transformImage image@(Image attr@(_, classes, _) _ (url, _)) caption =
  handle (inlineError image) $ do
    uri <- URI.mkURI url
    let mediaType = classifyMedia uri attr
    case Map.lookup mediaType imageTransformers of
      Just transform -> runAttr attr (transform uri caption) >>= renderHtml
      Nothing -> return image
transformImage inline _ = return inline

-- Lines up a list of images in a div element. Use with flexbox css.
transformImages :: [Inline] -> [Inline] -> Filter Block
transformImages images caption = do
  imageRow <-
    mapM (\img@(Image _ caption _) -> transformImage img caption) images
  if null caption
    then
      renderHtml $
        H.div ! A.class_ "decker image-row" $ toHtml $ map toHtml imageRow
    else do
      captionHtml <- inlinesToHtml caption
      renderHtml $
        H.figure ! A.class_ "decker" $ do
          H.div ! A.class_ "decker image-row" $ toHtml $ map toHtml imageRow
          H.figcaption captionHtml

language cls = find (`elem` ["plantuml", "dot", "gnuplot", "tex"]) cls

-- TODO this is incomplete
--   - captions are just swallowed but never rendered.
--   - caption recognition is disabled for now (see mediaBlockListFilter)
transformCodeBlock :: Block -> [Inline] -> Filter Block
transformCodeBlock code@(CodeBlock attr@(_, classes, _) text) caption =
  handle (blockError code) $
    if
        | all (`elem` classes) ["plantuml", "render"] ->
          runAttr attr (transform "plantuml") >>= renderHtml
        | all (`elem` classes) ["dot", "render"] ->
          runAttr attr (transform "dot") >>= renderHtml
        | all (`elem` classes) ["gnuplot", "render"] ->
          runAttr attr (transform "gnuplot") >>= renderHtml
        | all (`elem` classes) ["tex", "render"] ->
          runAttr attr (transform "tex") >>= renderHtml
        | all (`elem` classes) ["javascript", "run"] ->
          runAttr attr (renderJavascriptHtml text) >>= renderHtml
        | otherwise -> return code
  where
    transform :: Text -> Attrib Html
    transform ext = do
      dropClass ext
      let crc = printf "%08x" (calc_crc32 $ toString text)
      let path =
            transientDir </> "code"
              </> intercalate "-" ["code", crc]
              <.> toString ext
      exists <- liftIO $ doesFileExist path
      unless exists $
        liftIO $ do
          createDirectoryIfMissing True (takeDirectory path)
          Text.writeFile path text
      uri <- lift $ URI.mkURI (toText path)
      renderCodeHtml uri caption
transformCodeBlock block _ = return block

mkAudioTag :: Text -> Attr -> Html
mkAudioTag url (id, cs, kvs) =
  H.audio !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    ! H.dataAttribute "src" (H.preEscapedToValue url)
    !* kvs
    $ ""

mkVideoTag :: Text -> Attr -> Html
mkVideoTag url (id, cs, kvs) =
  H.video !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    ! H.dataAttribute "src" (H.preEscapedToValue url)
    !* kvs
    $ ""

mkIframeTag :: Text -> Attr -> Html
mkIframeTag url (id, cs, kvs) =
  H.iframe !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    ! H.customAttribute "allow" "fullscreen"
    ! H.dataAttribute "src" (H.preEscapedToValue url)
    !* kvs
    $ ""

mkImageTagF :: Text -> Attr -> Filter Html
mkImageTagF url (id, cs, kvs) = do
  useDataSrc <- fromMaybe True <$> lookupMetaF "decker.use-data-src"
  let srcAttr =
        if useDataSrc
          then H.dataAttribute "src" (H.preEscapedToValue url)
          else A.src (H.preEscapedToValue url)
  return $
    H.img !? (not (Text.null id), A.id (H.toValue id))
      ! A.class_ (H.toValue ("decker" : cs))
      ! srcAttr
      !* kvs

mkImageTag :: Text -> Attr -> Html
mkImageTag url (id, cs, kvs) =
  H.img !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    ! H.dataAttribute "src" (H.preEscapedToValue url)
    !* kvs

mkObjectTag :: Text -> Text -> Attr -> Html
mkObjectTag url mime (id, cs, kvs) =
  H.object !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker" : cs))
    ! A.type_ "application/pdf"
    ! A.data_ (H.preEscapedToValue url)
    !* kvs
    $ ""

mkSvgTag :: Text -> Attr -> Html
mkSvgTag svg (id, cs, kvs) =
  H.span !? (not (Text.null id), A.id (H.toValue id))
    ! A.class_ (H.toValue ("decker svg" : cs))
    !* kvs
    $ H.preEscapedText svg

audioHtml :: URI -> [Inline] -> Attrib Html
audioHtml uri caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let audioUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  let audioAttribs =
        takeClasses identity ["controls", "loop", "muted"]
          >> passAttribs identity ["controls", "loop", "muted", "preload"]
  case caption of
    [] -> do
      injectBorder >> takeAutoplay >> audioAttribs >> takeSize >> takeUsual
      mkAudioTag audioUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      audioAttr <- takeAutoplay >> audioAttribs >> extractAttr
      let audioTag = mkAudioTag audioUri audioAttr
      figureAttr <- injectBorder >> takeSize >> takeUsual >> extractAttr
      return $ mkFigureTag audioTag captionHtml figureAttr

imageHtml :: URI -> [Inline] -> Attrib Html
imageHtml uri caption = do
  uri <- lift $ transformUri uri ""
  let rendered = URI.render uri
  let fileName = toText $ takeFileName $ toString rendered
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual >> injectAttribute ("alt", fileName)
      extractAttr >>= lift . mkImageTagF rendered
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      imageTag <- lift $ mkImageTagF rendered imgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
        >> injectAttribute ("alt", fileName)
      mkFigureTag imageTag captionHtml <$> extractAttr

objectHtml :: Text -> URI -> [Inline] -> Attrib Html
objectHtml mime uri caption = do
  uri <- lift $ transformUri uri ""
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkObjectTag (URI.render uri) mime <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      objAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let imageTag = mkObjectTag (URI.render uri) mime objAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

svgHtml :: URI -> [Inline] -> Attrib Html
svgHtml uri caption = do
  uri <- lift $ transformUri uri ""
  svg <- lift $ readLocalUri uri
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkSvgTag svg <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      svgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let svgTag = mkSvgTag svg svgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag svgTag captionHtml <$> extractAttr

javascriptHtml :: URI -> [Inline] -> Attrib Html
javascriptHtml uri caption = do
  uri <- lift $ transformUri uri ""
  javascript <- lift $ readLocalUri uri
  case caption of
    [] -> renderJavascriptHtml javascript
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      javascriptTag <- renderJavascriptHtml javascript
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag javascriptTag captionHtml <$> extractAttr

mviewHtml :: URI -> [Inline] -> Attrib Html
mviewHtml uri caption = do
  uri <- lift $ transformUri uri ""
  let model = URI.render uri
  pushAttribute ("model", model)
  mviewUri <- URI.mkURI "public:support/mview/mview.html"
  iframeHtml mviewUri caption

iframeHtml :: URI -> [Inline] -> Attrib Html
iframeHtml uri caption = do
  uri <- lift $ transformUri uri ""
  xformRersourceAttribs ["image"]
  case caption of
    [] -> do
      iframeAttr <- injectBorder >> takeSize >> takeUsual >> extractAttr
      return $ mkIframeTag (URI.render uri) iframeAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      figureAttr <-
        injectBorder >> takeSizeIf isPercent >> takeId >> takeAllClasses
          >> takeCss
          >> dropCore
          >> passI18n
          >> extractAttr
      iframeAttr <- takeSizeIf (not . isPercent) >> takeData >> extractAttr
      let iframeTag = mkIframeTag (URI.render uri) iframeAttr
      return $ mkFigureTag iframeTag captionHtml figureAttr

videoHtml :: URI -> [Inline] -> Attrib Html
videoHtml uri caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  xformRersourceAttribs ["poster"]
  case caption of
    [] -> do
      injectBorder
      takeAutoplay
      takeVideoClasses
      passVideoAttribs
      takeSize
      takeUsual
      mkVideoTag videoUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      videoAttr <-
        takeSizeIf (not . isPercent) >> takeAutoplay >> takeVideoClasses
          >> passVideoAttribs
          >> extractAttr
      let videoTag = mkVideoTag videoUri videoAttr
      figureAttr <-
        injectBorder >> takeSizeIf isPercent >> takeUsual >> extractAttr
      return $ mkFigureTag videoTag captionHtml figureAttr

-- | Render an SVG image from the code linked to here.
renderCodeHtml :: URI -> [Inline] -> Attrib Html
renderCodeHtml uri caption = do
  uri <- lift $ transformUri uri "svg"
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let imageTag = mkImageTag (URI.render uri) imgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

renderJavascriptHtml :: Text -> Attrib Html
renderJavascriptHtml code = do
  id <- liftIO randomId
  let anchor = "let anchor = document.getElementById(\"" <> id <> "\");\n"
  return $ do
    H.div ! A.id (toValue id) ! A.class_ "es6 module anchor" $ ""
    H.script ! A.type_ "module" ! A.defer "" $ toHtml (anchor <> code)
