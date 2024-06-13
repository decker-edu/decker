{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Background movies do not work (unclear tags compile correctly)
-- TODO: CSS for decks containing examiner questions
-- TODO: CSS for decks containing examiner wburg questions

module Text.Decker.Filter.Media where

import Conduit (runConduit, withSourceFile, (.|))
import Control.Concurrent (withMVar)
import Control.Monad.Catch
import Data.Conduit.ImageSize (Size (Size), sinkImageSize)
import Data.List (lookup)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
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
import Text.Decker.Internal.Meta (lookupMetaOrFail)
import Text.Decker.Internal.URI
import Text.Decker.Server.Video
import Text.Pandoc
import Text.Printf
import Text.URI (URI)
import Text.URI qualified as URI

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
compileImage :: (Container c) => Attr -> [Inline] -> Text -> Text -> [Inline] -> Filter c
compileImage attr alt url title caption = do
  uri <- URI.mkURI url
  turi <- transformUri uri ""
  let turl = renderUriDecode turi
  let mediaType = classifyMedia uri attr
  runAttr attr
    $ if mediaType == RawImageT
      then do
        return $ mkRawImage attr alt turl title
      else do
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

compileLineBlock :: (Container c) => [[Inline]] -> [Inline] -> Filter c
compileLineBlock lines caption = do
  let images = map extract lines
  compileLineBlock' images caption
  where
    extract [image@Image {}] = unpackImage image
    extract _ = error "Inline is not an Image. oneImagePerLine seems to have failed."

-- | Compiles the contents of a LineBlock into a Decker specific structure.
compileLineBlock' :: (Container c) => [(Attr, [Inline], Text, Text)] -> [Inline] -> Filter c
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

imageSize2 :: FilePath -> IO (Maybe (Int, Int))
imageSize2 image =
  handle (\(SomeException e) -> return Nothing) $ do
    conv <$> withSourceFile image (\source -> runConduit $ source .| sinkImageSize)
  where
    conv (Just (Size w h)) = Just (w, h)
    conv _ = Nothing

determineAspectRatio :: (Attr, [Inline], Text, Text) -> Filter (Maybe Float)
determineAspectRatio (attr@(_, _, attribs), alt, url, title) = do
  uri <- URI.mkURI url
  let path = uriFilePath uri
  let mediaType = classifyMedia uri attr
  intrinsic <- case mediaType of
    ImageT -> do
      size <- liftIO $ imageSize2 path
      return $ aspect <$> size
    VideoT -> do
      size <- liftIO $ videoSize path
      return $ aspect <$> size
    _ -> do
      return Nothing
  return
    $ asum
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
compileCodeBlock attr@(id, classes, _) code caption = do
  meta' <- gets meta
  let codeId = toString $ if Text.null id then "code" else id
  let docPath = lookupMetaOrFail "decker.doc-path" meta'
  runAttr attr $ do
    media <-
      if
        | all (`elem` classes) ["plantuml", "render"] ->
            (writeAndRenderCodeBlock docPath codeId "plantuml")
        | all (`elem` classes) ["dot", "render"] ->
            (writeAndRenderCodeBlock docPath codeId "dot")
        | all (`elem` classes) ["gnuplot", "render"] ->
            (writeAndRenderCodeBlock docPath codeId "gnuplot")
        | all (`elem` classes) ["tex", "render"] ->
            (writeAndRenderCodeBlock docPath codeId "tex")
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
    writeAndRenderCodeBlock :: String -> String -> Text -> Attrib Block
    writeAndRenderCodeBlock docPath codeId ext = do
      dropClass ext
      disp <- show <$> lift (gets dispo)
      -- Add disposition to the CRC32 value to prevent collision between
      -- concurrent Deck and Handout references to the same code block.
      let crc = printf "%08x" (calc_crc32 $ disp <> toString code)
      transient <- liftIO transientDir
      let path = takeDirectory docPath </> renderedCodeDir </> intercalate "-" [takeBaseName docPath, codeId, crc] <.> toString ext
      -- Avoid a possible race condition
      mutex <- lift $ gets codeMutex
      liftIO
        $ withMVar
          mutex
          ( \_ -> do
              exists <- doesFileExist path
              unless exists $ do
                createDirectoryIfMissing True (takeDirectory path)
                tmp <- uniqueTransientFileName path
                Text.writeFile tmp code
                renameFile tmp path
                putStrLn $ "# write (" <> path <> ")"
          )
      uri <- lift $ URI.mkURI (toText path)
      renderCodeBlock uri "" caption

compileBlockQuote :: [Block] -> [Inline] -> Filter Block
compileBlockQuote quote caption =
  return
    $ wrapFigure nullAttr caption
    $ mkQuote quote

dragons :: (Text, [Text], [a])
dragons = ("", ["here be dragons"], [])

-- | One compiler for each image media type.
imageCompilers :: (Container c) => Map MediaT (URI -> Text -> [Inline] -> Attrib c)
imageCompilers =
  Map.fromList
    [ (EmbedSvgT, svgBlock),
      (PdfT, objectBlock "application/pdf"),
      (MviewT, mviewBlock),
      (ModelviewerT, modelviewerBlock),
      (GeogebraT, geogebraBlock),
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
imageBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
imageBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = renderUriDecode turi
  let fileName = toText $ uriFilePath turi
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    injectAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["image"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ containOne
    $ Image imgAttr [Str fileName] (turl, "")

-- |  Reads source code from a local file and wraps it in a pre tag.
includeCodeBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
includeCodeBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  code <- lift $ readLocalUri uri
  codeBlock code caption

-- |  Converts the contents of a code block to a standard pre tag.
codeBlock :: (Container c) => Text -> [Inline] -> Attrib c
codeBlock code caption = do
  (innerSizes, outerSizes) <- calcImageSizes
  codeAttr <- do
    takeAllClasses
    injectClasses ["processed"]
    injectStyles innerSizes
    takeAttributes ["style", "startFrom"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["code"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ mkPre codeAttr code

-- |  Compiles the image data to an iframe.
iframeBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
iframeBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = renderUriDecode turi
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
  return
    $ wrapFigure figureAttr caption
    $ mkIframe iframeAttr

-- |  Compiles the image data to an object showing a document of the given type.
objectBlock :: (Container c) => Text -> URI -> Text -> [Inline] -> Attrib c
objectBlock otype uri title caption = do
  turi <- lift $ transformUri uri ""
  let turl = renderUriDecode turi
  (innerSizes, outerSizes) <- calcIframeSizes
  objectAttr <- do
    injectAttribute ("data", turl)
    injectAttribute ("type", otype)
    injectStyles innerSizes
    injectAria title caption
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["object"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ mkObject objectAttr

-- |  Compiles the image data to an directly embedded SVG element.
svgBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
svgBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  svg <- lift $ readLocalUri uri
  (innerSizes, outerSizes) <- calcImageSizes
  svgAttr <- do
    injectStyles innerSizes
    injectAria title caption
    takeAttributes ["style"]
    takeData
    extractAttr
  figureAttr <- do
    injectClasses ["svg embedded"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ mkRaw svgAttr svg

-- |  Compiles the image data to a remote streaming video. If the aspect ratio of
--  the stream is known, it is a good idea to set the `aspect` attribute to
--  reflect that.
streamBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
streamBlock uri title caption = do
  let scheme = uriScheme uri
  let streamId = uriPath uri
  streamUri <-
    case scheme of
      Just "youtube" -> mkYoutubeUri streamId
      Just "vimeo" -> mkVimeoUri streamId
      Just "twitch" -> mkTwitchUri streamId
      _ ->
        throwM
          $ ResourceException
          $ "Unsupported stream service: "
          <> toString (fromMaybe "<none>" scheme)

  (innerSizes, outerSizes) <- calcIframeSizes
  iframeAttr <- do
    takeAutoplay
    injectAttribute ("data-src", renderUriDecode streamUri)
    injectAttribute ("allow", "fullscreen")
    injectStyles innerSizes
    extractAttr
  figureAttr <- do
    injectClasses ["stream"]
    cutClasses fragmentRelated >>= injectClasses
    takeUsual
    injectStyles outerSizes
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ mkIframe iframeAttr

-- |  Compiles the image data to an iframe containing marios mview tool.
mviewBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
mviewBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let model = renderUriDecode turi
  pushAttribute ("model", model)
  mviewUri <- URI.mkURI "public:support/mview/mview.html"
  iframeBlock mviewUri title caption

-- |  Compiles the image data to an iframe containing Google's modelviewer.
modelviewerBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
modelviewerBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  let model = renderUriDecode turi
  pushAttribute ("model", model)
  modelviewerUri <- URI.mkURI "public:support/modelviewer/model-viewer.html"
  iframeBlock modelviewerUri title caption

-- |  Compiles the image data to an iframe containing marios geogebra page.
geogebraBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
geogebraBlock uri title caption = do
  turi <- lift $ transformUri uri ""
  meta' <- lift $ gets meta
  -- Constructs the relative path from /support/geogebra/geogebra.html to .ggb file
  let docRelative = toString $ URI.render turi
  let docBase = lookupMetaOrFail "decker.base-dir" meta'
  let projRelative = docBase </> docRelative
  let scriptRelative = ".." </> ".." </> projRelative
  pushAttribute ("filename", toText scriptRelative)
  geogebraUri <- URI.mkURI "public:support/geogebra/geogebra.html"
  iframeBlock geogebraUri title caption

audioBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
audioBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let audioUri =
        if Text.null mediaFrag
          then renderUriDecode uri
          else renderUriDecode uri {URI.uriFragment = URI.mkFragment mediaFrag}
  audioAttr <- do
    takeClasses identity ["controls", "loop", "muted"]
    passAttribs identity ["controls", "loop", "muted", "preload"]
    injectAttribute ("src", audioUri)
    takeAutoplay
    injectAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["audio"]
    cutClasses fragmentRelated >>= injectClasses
    takeUsual
    extractAttr
  return $ wrapFigure figureAttr caption $ mkAudio audioAttr

-- |  Compiles the image data to a local video.
videoBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
videoBlock uri title caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then renderUriDecode uri
          else renderUriDecode uri {URI.uriFragment = URI.mkFragment mediaFrag}
  xformRersourceAttribs ["poster"]
  (innerSizes, outerSizes) <- calcImageSizes
  videoAttr <- do
    injectAttribute ("data-src", videoUri)
    injectStyles innerSizes
    takeAutoplay
    takeVideoClasses
    passVideoAttribs
    injectAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["video"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return $ wrapFigure figureAttr caption $ mkVideo videoAttr

-- | Assumes an SVG image has been produced from some source and constructs an
-- image around it.
renderCodeBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
renderCodeBlock uri title caption = do
  turi <- lift $ transformUri uri "svg"
  let turl = renderUriDecode turi
  let fileName = toText $ takeFileName $ toString turl
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <- do
    injectClasses ["processed"]
    injectStyles innerSizes
    injectAria title caption
    extractAttr
  figureAttr <- do
    injectClasses ["image rendered"]
    cutClasses fragmentRelated >>= injectClasses
    injectStyles outerSizes
    takeUsual
    extractAttr
  return
    $ wrapFigure figureAttr caption
    $ containOne
    $ Image imgAttr [Str fileName] (turl, "")

-- |  Transforms an image tag to script tag using the image url as src. Only
-- supports ES6 modules.
javascriptBlock :: (Container c) => URI -> Text -> [Inline] -> Attrib c
javascriptBlock uri title caption = do
  -- Pandoc insists that ids start with letters
  id <- ("id" <>) <$> liftIO randomId
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
  return
    $ wrapFigure' figureAttr caption
    $ renderJavascript imgAttr furi
  where
    renderJavascript :: (Container c) => Attr -> URI -> [c]
    renderJavascript attr uri =
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

-- | Inserts the contents of the code block into a script tag. Only supports ES6
-- modules.
javascriptCodeBlock :: Text -> [Inline] -> Attrib Block
javascriptCodeBlock code caption = do
  (innerSizes, outerSizes) <- calcImageSizes
  -- Pandoc insists that ids start with letters
  id <- ("id" <>) <$> liftIO randomId
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
  return
    $ wrapFigure' figureAttr caption
    $ renderJavascript id imgAttr code
  where
    renderJavascript :: (Container c) => Text -> Attr -> Text -> [c]
    renderJavascript id attr code =
      let anchor = "let anchor = document.getElementById(\"" <> id <> "\");\n"
       in [ mkContainer attr [],
            mkContainer
              ("", [], [("data-tag", "script"), ("type", "module"), ("defer", "")])
              [mkRaw' (anchor <> code)]
          ]

-- |  Wraps any container in a figure. Adds a caption element if the caption is
--  not empty.
wrapFigure :: (Container a) => Attr -> [Inline] -> a -> a
wrapFigure attr caption inline =
  mkFigure
    attr
    ( [inline]
        <> [ mkFigCaption
               [containSome caption]
             | not (null caption)
           ]
    )

wrapFigure' :: (Container a) => Attr -> [Inline] -> [a] -> a
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
  mkQuote :: [Block] -> a
  mkPre :: Attr -> Text -> a
  mkRaw :: Attr -> Text -> a
  mkRaw' :: Text -> a
  containSome :: [Inline] -> a
  containOne :: Inline -> a
  mkRawImage :: Attr -> [Inline] -> Text -> Text -> a

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
  mkQuote a = error "Inline element not allowed in this context."
  mkPre a t =
    Span
      (addClass "pre" a)
      [ tag "code"
          $ Span (addClass "processed" nullAttr) [RawInline "html" (text t)]
      ]
  mkRaw a t = Span a [RawInline "html" t]
  mkRaw' t = RawInline "html" t
  containSome = Span nullAttr
  containOne = identity
  mkRawImage attr alt url title = Image attr alt (url, title)

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
  mkQuote blocks = BlockQuote blocks
  mkPre a t = CodeBlock (addClass "processed" a) t
  mkRaw a t = Div a [RawBlock "html" t]
  mkRaw' t = RawBlock "html" t
  containSome = Plain
  containOne i = Plain [i]
  mkRawImage attr alt url title = Plain [Image (addClass "processed" attr) alt (url, title)]

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
  return
    $ case (aspect, width, height) of
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
