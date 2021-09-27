{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO Background movies do not work
-- TODO .grid layout has no CSS yet (column-deck)
-- TODO .inverse needs to change the background color
-- TODO engine decks chrash
-- TODO CSS for decks containing examiner questions
-- TODO Organisation of CSS for deck, page and handout

module Text.Decker.Filter.Media where

import Control.Monad.Catch
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import HTMLEntities.Text (text)
import Relude
import System.Directory
import System.FilePath.Posix
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.CRC32
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Slide
import Text.Decker.Filter.Streaming
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.URI
import Text.Pandoc
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

-- | Compiles the contents of an Image into a Decker specific structure. This is
-- context aware and produces either a Block or an Inline element.
compileImage :: Container c => Attr -> [Inline] -> Text -> Text -> [Inline] -> Filter c
compileImage attr alt url title caption = do
  uri <- URI.mkURI url
  let mediaType = classifyMedia uri attr
  runAttr attr $ do
    media <- case Map.lookup mediaType imageCompilers of
      Just transform -> transform uri caption
      Nothing -> error $ "No transformer for media type " <> show mediaType
    attribs <- do
      injectBorder
      injectClasses ["media"]
      takeUsual
      extractAttr
    return $ mkContainer attribs [media]

-- | Compiles the contents of a CodeBlock into a Decker specific structure.
compileCodeBlock :: Attr -> Text -> [Inline] -> Filter Block
compileCodeBlock attr@(_, classes, _) code caption =
  runAttr attr $ do
    media <-
      if
          | all (`elem` classes) ["plantuml", "render"] ->
            (transform "plantuml")
          | all (`elem` classes) ["dot", "render"] ->
            (transform "dot")
          | all (`elem` classes) ["gnuplot", "render"] ->
            (transform "gnuplot")
          | all (`elem` classes) ["tex", "render"] ->
            (transform "tex")
          | all (`elem` classes) ["javascript", "run"] ->
            (javascriptCodeBlock code caption)
          | otherwise ->
            (codeBlock code caption)
    attribs <- do
      injectBorder
      injectClasses ["media"]
      takeUsual
      extractAttr
    return $ mkContainer attribs [media]
  where
    transform :: Text -> Attrib Block
    transform ext = do
      dropClass ext
      let crc = printf "%08x" (calc_crc32 $ toString code)
      let path =
            transientDir </> "code"
              </> intercalate "-" ["code", crc]
              <.> toString ext
      exists <- liftIO $ doesFileExist path
      unless exists $
        liftIO $ do
          createDirectoryIfMissing True (takeDirectory path)
          Text.writeFile path code
      uri <- lift $ URI.mkURI (toText path)
      renderCodeBlock uri caption

-- | Compiles the contents of a LineBlock into a Decker specific structure.
compileLineBlock :: [(Attr, [Inline], Text, Text)] -> [Inline] -> Filter Block
compileLineBlock images caption = return $ Div dragons []

dragons :: (Text, [Text], [a])
dragons = ("", ["here be dragons"], [])

-- | One compiler for each image media type.
imageCompilers :: Container c => Map MediaT (URI -> [Inline] -> Attrib c)
imageCompilers =
  Map.fromList
    [ (EmbedSvgT, svgBlock),
      (PdfT, objectBlock "application/pdf"),
      (MviewT, mviewBlock),
      (IframeT, iframeBlock),
      (ImageT, imageBlock),
      (VideoT, videoBlock),
      (StreamT, streamBlock),
      -- (AudioT, audioHtml),
      (CodeT, includeCodeBlock),
      (RenderT, renderCodeBlock),
      (JavascriptT, javascriptBlock)
    ]

-- |  Compiles the image data to a plain image.
imageBlock :: Container c => URI -> [Inline] -> Attrib c
imageBlock uri caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  let fileName = toText $ takeFileName $ toString turl
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["image"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      containOne $
        Image imgAttr [Str fileName] (turl, "")

includeCodeBlock :: Container c => URI -> [Inline] -> Attrib c
includeCodeBlock uri caption = do
  uri <- lift $ transformUri uri ""
  code <- lift $ readLocalUri uri
  codeBlock code caption

-- |  Compiles the image data to a plain image.
codeBlock :: Container c => Text -> [Inline] -> Attrib c
codeBlock code caption = do
  (innerSizes, outerSizes) <- calcImageSizes
  codeAttr <- do
    takeAllClasses
    injectClasses ["processed"]
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["code"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkPre codeAttr code

-- |  Compiles the image data to an iframe.
iframeBlock :: Container c => URI -> [Inline] -> Attrib c
iframeBlock uri caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  xformRersourceAttribs ["image"]
  (innerSizes, outerSizes) <- calcIframeSizes
  iframeAttr <- do
    injectAttribute ("data-src", turl)
    injectAttribute ("allow", "fullscreen")
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["iframe"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkIframe iframeAttr

-- |  Compiles the image data to an object showing a document of the given type.
objectBlock :: Container c => Text -> URI -> [Inline] -> Attrib c
objectBlock otype uri caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  (innerSizes, outerSizes) <- calcIframeSizes
  objectAttr <- do
    injectAttribute ("data", turl)
    injectAttribute ("type", otype)
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["object"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkObject objectAttr

-- |  Compiles the image data to an directly embedded SVG element.
svgBlock :: Container c => URI -> [Inline] -> Attrib c
svgBlock uri caption = do
  uri <- lift $ transformUri uri ""
  svg <- lift $ readLocalUri uri
  (innerSizes, outerSizes) <- calcImageSizes
  svgAttr <- do
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["svg embedded"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $ mkRaw svgAttr svg

-- |  Compiles the image data to a remote streaming video. If the aspect ratio of
--  the stream is known, it is a good idea to set the `aspect` attribute to
--  reflect that.
streamBlock :: Container c => URI -> [Inline] -> Attrib c
streamBlock uri caption = do
  let scheme = uriScheme uri
  let streamId = uriPath uri
  streamUri <-
    case scheme of
      Just "youtube" -> mkYoutubeUri streamId
      Just "vimeo" -> mkVimeoUri streamId
      Just "twitch" -> mkTwitchUri streamId
      _ ->
        throwM $
          ResourceException $
            "Unsupported stream service: " <> toString (fromMaybe "<none>" scheme)

  iframeAttr <- do
    takeAutoplay
    injectAttribute ("src", URI.render streamUri)
    extractAttr

  fluidAttr <- do
    ifAttrib "aspect" $
      \aspect -> injectStyle ("--aspect-ratio", calcAspect aspect)
    injectClass "fluid-iframe"
    extractAttr

  figureAttr <- do
    cutAttrib "height"
    ifAttrib "width" $
      \width -> injectStyle ("width", width)
    injectClasses ["stream"]
    extractAttr

  return $
    wrapFigure figureAttr caption $
      mkContainer fluidAttr [mkIframe iframeAttr]
  where
    calcAspect :: Text -> Text
    calcAspect ratio =
      fromMaybe "0.5625" $
        case Text.splitOn ":" ratio of
          [w, h] -> do
            wf <- readMaybe $ toString w :: Maybe Float
            hf <- readMaybe $ toString h :: Maybe Float
            return $ Text.pack (printf "%.4f" (hf / wf))
          _ -> Nothing

-- |  Compiles the image data to an iframe containing marios mview tool.
mviewBlock :: Container c => URI -> [Inline] -> Attrib c
mviewBlock uri caption = do
  turi <- lift $ transformUri uri ""
  let model = URI.render turi
  pushAttribute ("model", model)
  mviewUri <- URI.mkURI "public:support/mview/mview.html"
  iframeBlock mviewUri caption

-- |  Compiles the image data to a local video.
videoBlock :: Container c => URI -> [Inline] -> Attrib c
videoBlock uri caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  xformRersourceAttribs ["poster"]
  (innerSizes, outerSizes) <- calcImageSizes
  videoAttr <- do
    injectAttribute ("src", videoUri)
    injectStyles innerSizes
    takeAutoplay
    takeVideoClasses
    passVideoAttribs
    extractAttr
  figureAttr <- do
    injectClasses ["video"]
    injectStyles outerSizes
    extractAttr
  return $ wrapFigure figureAttr caption $ mkVideo videoAttr

-- |  Compiles the image data to a plain image.
renderCodeBlock :: Container c => URI -> [Inline] -> Attrib c
renderCodeBlock uri caption = do
  turi <- lift $ transformUri uri "svg"
  let turl = URI.render turi
  let fileName = toText $ takeFileName $ toString turl
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["image rendered"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      containOne $
        Image imgAttr [Str fileName] (turl, "")

-- |  Compiles the image data to a plain image.
javascriptBlock :: Container c => URI -> [Inline] -> Attrib c
javascriptBlock uri caption = do
  id <- liftIO randomId
  fragment <- URI.mkFragment id
  uri <- lift $ transformUri uri ""
  let furi = uri {URI.uriFragment = Just fragment}
  (innerSizes, outerSizes) <- calcIframeSizes
  imgAttr <- do
    injectStyles innerSizes
    injectId id
    injectClasses ["es6", "module", "anchor"]
    extractAttr
  figureAttr <- do
    injectClasses ["javascript"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      renderJavascript' imgAttr furi

javascriptCodeBlock :: Text -> [Inline] -> Attrib Block
javascriptCodeBlock code caption = do
  (innerSizes, outerSizes) <- calcIframeSizes
  id <- liftIO randomId
  imgAttr <- do
    injectStyles innerSizes
    injectId id
    injectClasses ["es6", "module", "anchor"]
    extractAttr
  figureAttr <- do
    injectClasses ["javascript"]
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      renderJavascript id imgAttr code

renderJavascript :: Container c => Text -> Attr -> Text -> c
renderJavascript id attr code =
  let anchor = "let anchor = document.getElementById(\"" <> id <> "\");\n"
   in mkContainer
        ("", ["es6", "module"], [])
        [ mkContainer attr [],
          mkContainer
            ("", [], [("data-tag", "script"), ("type", "module"), ("defer", "")])
            [mkRaw' (anchor <> code)]
        ]

renderJavascript' :: Container c => Attr -> URI -> c
renderJavascript' attr uri =
  mkContainer
    ("", ["es6", "module"], [])
    [ mkContainer attr [],
      mkContainer
        ( "",
          [],
          [ ("data-tag", "script"),
            ("src", URI.render uri),
            ("type", "module"),
            ("async", "")
          ]
        )
        []
    ]

-- |  Wraps any container in a figure. Adds a caption element if the caption is
--  not empty.
wrapFigure :: Container a => Attr -> [Inline] -> a -> a
wrapFigure attr caption inline =
  mkFigure
    attr
    ( [inline]
        <> [ mkFigCaption
               [containSome caption]
             | not (null caption)
           ]
    )

-- | These functions construct the right container for the context they are used
-- in, which can be either Block or Inline as defied by Pandoc. This ensures
-- that legal HTML is generated in all circumstances.
class Container a where
  toBlock :: a -> Block
  onlyBlock :: Block -> a
  mkContainer :: Attr -> [a] -> a
  mkFigure :: Attr -> [a] -> a
  mkFigCaption :: [a] -> a
  mkIframe :: Attr -> a
  mkVideo :: Attr -> a
  mkObject :: Attr -> a
  mkPre :: Attr -> Text -> a
  mkRaw :: Attr -> Text -> a
  mkRaw' :: Text -> a
  containSome :: [Inline] -> a
  containOne :: Inline -> a

instance Container Inline where
  toBlock c = Plain [c]
  onlyBlock _ = error "Block element not allowed in this context."
  mkContainer = Span
  mkFigure a cs = Span (addClass "figure" a) cs
  mkFigCaption cs = Span (addClass "figcaption" nullAttr) cs
  mkIframe a = tag "iframe" $ Span a []
  mkVideo a = tag "video" $ Span a []
  mkObject a = tag "object" $ Span a []
  mkPre a t =
    Span
      (addClass "pre" a)
      [ tag "code" $
          Span a [RawInline "html" (text t)]
      ]
  mkRaw a t = Span a [RawInline "html" t]
  mkRaw' t = RawInline "html" t
  containSome = Span nullAttr
  containOne = identity

instance Container Block where
  toBlock = id
  onlyBlock = id
  mkContainer = Div
  mkFigure a cs = tag "figure" $ Div a cs
  mkFigCaption cs = tag "figcaption" $ Div nullAttr cs
  mkIframe a = tag "iframe" $ Div a []
  mkVideo a = tag "video" $ Div a []
  mkObject a = tag "object" $ Div a []
  mkPre a t = CodeBlock a t
  mkRaw a t = Div a [RawBlock "html" t]
  mkRaw' t = RawBlock "html" t
  containSome = Plain
  containOne i = Plain [i]

-- | Calculates the inner (first pair) and outer (second pair) size settings for
-- images. The outer sizes are supposed to be used on the container, the inner
-- sizes are used on the image tag itself. Width is controlled with the
-- container, height is controlled on the image. The respective other setting is
-- set to do the right thing. auto values are used to override any default
-- settings, which can be set through CSS. Either both sizes are returned, or
-- none are. The returned values are suppoed to be added to the style attribute
-- of the elements.
calcImageSizes :: Attrib ([(Text, Text)], [(Text, Text)])
calcImageSizes = do
  width <- cutAttrib "width"
  height <- cutAttrib "height"
  if
      | isJust width && isJust height ->
        -- Both sizes are specified. Aspect ratio be damned.
        return
          ( [("height", fromJust height), ("width", "100%")],
            [("height", "auto"), ("width", fromJust width)]
          )
      | isJust width && isNothing height ->
        -- Only width is specified. Keep aspect ratio.
        return
          ( [("height", "auto"), ("width", "100%")],
            [("height", "auto"), ("width", fromJust width)]
          )
      | isNothing width && isJust height ->
        -- Only height is specified. Keep aspect ratio.
        return
          ( [("height", fromJust height), ("width", "auto")],
            [("height", "auto"), ("width", "auto")]
          )
      | otherwise ->
        -- Nothing is specified, use CSS defaults.
        return ([], [])

-- | See calcImageSizes. Iframes have no aspect ratio and therefore behave a
-- little differently.
calcIframeSizes :: Attrib ([(Text, Text)], [(Text, Text)])
calcIframeSizes = do
  width <- cutAttrib "width"
  height <- cutAttrib "height"
  if
      | isJust width && isJust height ->
        return
          ( [("height", fromJust height), ("width", "100%")],
            [("height", "auto"), ("width", fromJust width)]
          )
      | isJust width && isNothing height ->
        return
          ( [("width", "100%")],
            [("height", "auto"), ("width", fromJust width)]
          )
      | isNothing width && isJust height ->
        return
          ( [("height", fromJust height), ("width", "100%")],
            [("height", "auto"), ("width", "100%")]
          )
      | otherwise -> return ([], [])