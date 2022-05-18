{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Background movies do not work (unclear tags compile correctly)
-- TODO: CSS for decks containing examiner questions
-- TODO: CSS for decks containing examiner wburg questions

module Text.Decker.Filter.Media where

import Control.Monad.Catch
import Data.List (lookup)
import qualified Data.Map.Strict as Map
import Data.Maybe
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
import Text.Decker.Filter.Util
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Internal.Helper
import Text.Decker.Internal.URI
import Text.Decker.Server.Video
import Text.Pandoc
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

fragmentRelated =
  [ "fragment",
    "fade-out",
    "fade-up",
    "fade-down",
    "fade-left",
    "fade-right",
    "fade-in-then-out",
    "fade-in-then-semi-out",
    "grow",
    "semi-fade-out",
    "shrink",
    "strike",
    "highlight-red",
    "highlight-green",
    "highlight-blu",
    "highlight-current-red",
    "highlight-current-green",
    "highlight-current-blu"
  ]

-- | Compiles the contents of an Image into a Decker specific structure. This is
-- context aware and produces either a Block or an Inline element. The caption
-- might either come from the alt attribute or the separate Caption: line.
compileImage :: Container c => Attr -> [Inline] -> Text -> Text -> [Inline] -> Filter c
compileImage attr alt url title caption = do
  uri <- URI.mkURI url
  let mediaType = classifyMedia uri attr
  runAttr attr $ do
    media <- case Map.lookup mediaType imageCompilers of
      Just transform -> transform uri title caption
      Nothing -> error $ "No transformer for media type " <> show mediaType
    attribs <- do
      injectBorder
      injectClasses ["media"]
      takeUsual
      extractAttr
    return $ mkContainer attribs [media]

defaultAspectRatio = "16/9"

compileLineBlock :: Container c => [[Inline]] -> [Inline] -> Filter c
compileLineBlock lines caption = do
  let images = map extract lines
  compileLineBlock' images caption
  where
    extract [image@Image {}] = unpackImage image
    extract _ = error "Inline is not an Image. oneImagePerLine seems to have failed."

-- | Compiles the contents of a LineBlock into a Decker specific structure.
compileLineBlock' :: Container c => [(Attr, [Inline], Text, Text)] -> [Inline] -> Filter c
compileLineBlock' images caption = do
  aspects <- mapMaybeM determineAspectRatio images
  let columns =
        if length aspects == length images
          then map (printFr . (\a -> sum aspects * a)) aspects
          else map (const "1fr") images
  figures <- mapM compile images
  let row =
        mkContainer
          ( "",
            ["lineblock"],
            [ ( "style",
                "grid-template-columns: " <> Text.intercalate " " columns <> ";"
              )
            ]
          )
          figures
  let figure = wrapFigure' ("", ["lineblock"], []) caption [row]
  return $ mkContainer ("", ["lineblock", "media"], []) [figure]
  where
    printFr a = toText (printf "%.4ffr" a :: String)
    compile (attr, alt, url, title) = do
      uri <- URI.mkURI url
      let mediaType = classifyMedia uri attr
      runAttr attr $ do
        case Map.lookup mediaType imageCompilers of
          Just transform -> transform uri title alt
          Nothing -> error $ "No transformer for media type " <> show mediaType

determineAspectRatio :: (Attr, [Inline], Text, Text) -> Filter (Maybe Float)
determineAspectRatio (attr@(_, _, attribs), alt, url, title) = do
  uri <- URI.mkURI url
  let path = uriFilePath uri
  let mediaType = classifyMedia uri attr
  intrinsic <- case mediaType of
    ImageT -> do
      size <- liftIO $ imageSize path
      return $ aspect <$> size
    VideoT -> do
      size <- liftIO $ videoSize path
      return $ aspect <$> size
    _ -> do
      return Nothing
  return $
    asum
      [ lookup "w:h" attribs >>= readRatio,
        lookup "aspect-ratio" attribs >>= readRatio,
        intrinsic,
        readRatio defaultAspectRatio
      ]
  where
    aspect (w, h) = fromIntegral w / fromIntegral h
    readRatio :: Text -> Maybe Float
    readRatio ratio =
      case readMaybe (toString ratio) of
        Just ratio -> ratio
        Nothing ->
          case Text.splitOn "/" ratio of
            [w, h] -> do
              wf <- readMaybe $ toString w :: Maybe Float
              hf <- readMaybe $ toString h :: Maybe Float
              return (wf / hf)
            _ -> Nothing

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
      -- disp <- show <$> lift (gets dispo)
      let crc = printf "%08x" (calc_crc32 $ toString code)
      let path =
            transientDir </> "code"
              </> intercalate "-" ["code", crc]
              <.> toString ext
      -- Avoid a possible race condition when the same code block content is
      -- used twice and written only when the file does not yet exist: Just do
      -- not prematurely optimise by testing for existence first and atomically
      -- write the file.
      liftIO $ do
        createDirectoryIfMissing True (takeDirectory path)
        tmp <- uniqueTransientFileName path
        Text.writeFile tmp code
        renameFile tmp path
        putStrLn $ "# write (" <> path <> ")"
      uri <- lift $ URI.mkURI (toText path)
      renderCodeBlock uri "" caption

dragons :: (Text, [Text], [a])
dragons = ("", ["here be dragons"], [])

-- | One compiler for each image media type.
imageCompilers :: Container c => Map MediaT (URI -> Text -> [Inline] -> Attrib c)
imageCompilers =
  Map.fromList
    [ (EmbedSvgT, svgBlock),
      (PdfT, objectBlock "application/pdf"),
      (MviewT, mviewBlock),
      (IframeT, iframeBlock),
      (ImageT, imageBlock),
      (VideoT, videoBlock),
      (StreamT, streamBlock),
      (AudioT, audioBlock),
      (CodeT, includeCodeBlock),
      (RenderT, renderCodeBlock),
      (JavascriptT, javascriptBlock)
    ]

-- ┌───────────────────────┐
-- │<div>                  │
-- │   ┌───────────────┐   │
-- │   │ <figure>      │   │
-- │   │  ┌─────────┐  │   │
-- │   │  │<image>  │  │   │
-- │   │  │         │  │   │
-- │   │  │         │  │   │
-- │   │  └─────────┘  │   │
-- │   │               │   │
-- │   └───────────────┘   │
-- │                       │
-- └───────────────────────┘

-- |  Compiles the image data to a plain image.
imageBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
imageBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  let fileName = toText $ uriFilePath turi
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    inventTitleAndAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["image"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      containOne $
        Image imgAttr [Str fileName] (turl, "")

includeCodeBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
includeCodeBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  code <- lift $ readLocalUri uri
  codeBlock code caption

-- |  Compiles the image data to a plain image.
codeBlock :: Container c => Text -> [Inline] -> Attrib c
codeBlock code caption = do
  (innerSizes, outerSizes) <- calcImageSizes
  codeAttr <- do
    takeAllClasses
    injectStyles innerSizes
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["code"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkPre codeAttr code

-- |  Compiles the image data to an iframe.
iframeBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
iframeBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  xformRersourceAttribs ["image"]
  (innerSizes, outerSizes) <- calcIframeSizes
  iframeAttr <- do
    injectAttribute ("data-src", turl)
    injectAttribute ("allow", "fullscreen")
    takeAttributes ["style"]
    takeData
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    takeUsual
    injectClasses ["iframe"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkIframe iframeAttr

-- |  Compiles the image data to an object showing a document of the given type.
objectBlock :: Container c => Text -> URI -> Text -> [Inline] -> Attrib c
objectBlock otype uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  (innerSizes, outerSizes) <- calcIframeSizes
  objectAttr <- do
    injectAttribute ("data", turl)
    injectAttribute ("type", otype)
    injectStyles innerSizes
    inventTitleAndAria title caption
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["object"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkObject objectAttr

-- |  Compiles the image data to an directly embedded SVG element.
svgBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
svgBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  svg <- lift $ readLocalUri uri
  (innerSizes, outerSizes) <- calcImageSizes
  svgAttr <- do
    injectStyles innerSizes
    inventTitleAndAria title caption
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["svg embedded"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure figureAttr caption $ mkRaw svgAttr svg

-- |  Compiles the image data to a remote streaming video. If the aspect ratio of
--  the stream is known, it is a good idea to set the `aspect` attribute to
--  reflect that.
streamBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
streamBlock uri title caption = do
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

  (innerSizes, outerSizes) <- calcIframeSizes
  iframeAttr <- do
    takeAutoplay
    injectAttribute ("src", URI.render streamUri)
    injectAttribute ("allow", "fullscreen")
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["stream"]
    cutClasses fragmentRelated >>= injectClasses
    takeUsual
    injectStyles outerSizes
    extractAttr
  return $
    wrapFigure figureAttr caption $
      mkIframe iframeAttr

-- |  Compiles the image data to an iframe containing marios mview tool.
mviewBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
mviewBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let model = URI.render turi
  pushAttribute ("model", model)
  mviewUri <- URI.mkURI "public:support/mview/mview.html"
  iframeBlock mviewUri title caption

audioBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
audioBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let audioUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  audioAttr <- do
    takeClasses identity ["controls", "loop", "muted"]
    passAttribs identity ["controls", "loop", "muted", "preload"]
    injectAttribute ("src", audioUri)
    takeAutoplay
    inventTitleAndAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["audio"]
    cutClasses fragmentRelated >>= injectClasses
    takeUsual
    extractAttr
  return $ wrapFigure figureAttr caption $ mkAudio audioAttr

-- |  Compiles the image data to a local video.
videoBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
videoBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  xformRersourceAttribs ["poster"]
  (innerSizes, outerSizes) <- calcImageSizes
  videoAttr <- do
    injectAttribute ("data-src", videoUri)
    injectStyles innerSizes
    takeAutoplay
    takeVideoClasses
    passVideoAttribs
    inventTitleAndAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["video"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $ wrapFigure figureAttr caption $ mkVideo videoAttr

-- |  Compiles the image data to a plain image.
renderCodeBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
renderCodeBlock uri title caption = do
  turi <- lift $ transformUri uri "svg"
  let turl = URI.render turi
  let fileName = toText $ takeFileName $ toString turl
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    inventTitleAndAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["image rendered"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure figureAttr caption $
      containOne $
        Image imgAttr [Str fileName] (turl, "")

-- |  Compiles the image data to a plain image.
javascriptBlock :: Container c => URI -> Text -> [Inline] -> Attrib c
javascriptBlock uri title caption = do
  id <- liftIO randomId
  fragment <- URI.mkFragment id
  uri <- lift $ transformUri uri ""
  let furi = uri {URI.uriFragment = Just fragment}
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectStyles innerSizes
    injectId id
    injectClasses ["es6", "module", "anchor"]
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["javascript"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure' figureAttr caption $
      renderJavascript' imgAttr furi

javascriptCodeBlock :: Text -> [Inline] -> Attrib Block
javascriptCodeBlock code caption = do
  (innerSizes, outerSizes) <- calcImageSizes
  id <- liftIO randomId
  imgAttr <- do
    injectStyles innerSizes
    injectId id
    injectClasses ["es6", "module", "anchor"]
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["javascript"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $
    wrapFigure' figureAttr caption $
      renderJavascript id imgAttr code

renderJavascript :: Container c => Text -> Attr -> Text -> [c]
renderJavascript id attr code =
  let anchor = "let anchor = document.getElementById(\"" <> id <> "\");\n"
   in [ mkContainer attr [],
        mkContainer
          ("", [], [("data-tag", "script"), ("type", "module"), ("defer", "")])
          [mkRaw' (anchor <> code)]
      ]

renderJavascript' :: Container c => Attr -> URI -> [c]
renderJavascript' attr uri =
  [ mkContainer attr [],
    mkContainer
      ( "",
        [],
        [ ("data-tag", "script"),
          ("src", URI.render uri),
          ("type", "module")
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

wrapFigure' :: Container a => Attr -> [Inline] -> [a] -> a
wrapFigure' attr caption inline =
  mkFigure
    attr
    ( inline
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
  mkAudio :: Attr -> a
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
  mkAudio a = tag "audio" $ Span a []
  mkObject a = tag "object" $ Span a []
  mkPre a t =
    Span
      (addClass "pre" a)
      [ tag "code" $
          Span (addClass "processed" nullAttr) [RawInline "html" (text t)]
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
  mkAudio a = tag "audio" $ Div a []
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

-- | See calcImageSizes. Iframes have no intrinsic aspect ratio and therefore behave a
-- little differently.
calcIframeSizes :: Attrib ([(Text, Text)], [(Text, Text)])
calcIframeSizes = do
  width <- cutAttrib "width"
  height <- cutAttrib "height"
  a1 <- cutAttrib "aspect-ratio"
  a2 <- cutAttrib "w:h"
  let aspect = asum [a2, a1]
  return $
    case (aspect, width, height) of
      (Nothing, Nothing, Nothing) ->
        ( [("width", "100%"), ("height", "auto"), ("aspect-ratio", defaultAspectRatio)], -- iframe
          [("width", "100%"), ("height", "auto")] -- figure
        )
      (Nothing, Nothing, Just height) ->
        ( [("width", "auto"), ("height", height), ("aspect-ratio", defaultAspectRatio)],
          [("width", "auto"), ("height", "auto")]
        )
      (Nothing, Just width, Nothing) ->
        ( [("width", "100%"), ("height", "auto"), ("aspect-ratio", defaultAspectRatio)],
          [("width", width), ("height", "auto")]
        )
      (Nothing, Just width, Just height) ->
        ( [("width", "100%"), ("height", height)],
          [("width", width), ("height", "auto")]
        )
      (Just aspect, Just width, Nothing) ->
        ( [("width", width), ("height", "auto"), ("aspect-ratio", aspect)],
          [("width", "auto"), ("height", "auto")]
        )
      (Just _, Just width, Just height) ->
        ( [("width", "100%"), ("height", height)],
          [("width", width), ("height", "auto")]
        )
      (Just aspect, Nothing, Nothing) ->
        ( [("width", "100%"), ("height", "auto"), ("aspect-ratio", aspect)], -- iframe
          [("width", "100%"), ("height", "auto")] -- figure
        )
      (Just aspect, Nothing, Just height) ->
        ( [("width", "auto"), ("height", height), ("aspect-ratio", aspect)],
          [("width", "auto"), ("height", "auto")]
        )
