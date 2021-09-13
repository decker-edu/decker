{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Media where

import Control.Monad.Catch
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Relude
import System.FilePath.Posix
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Slide
import Text.Decker.Filter.Streaming
import Text.Decker.Internal.Exception
import Text.Decker.Internal.URI
import Text.Pandoc
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

compileImageToInline :: Attr -> [Inline] -> Text -> Text -> [Inline] -> Filter Inline
compileImageToInline attr alt url title caption = do
  runAttr attr $ do
    media <- compileImage alt url title caption
    attribs <-
      injectBorder
        >> injectClasses ["media"]
        >> takeUsual
        >> extractAttr
    return $ Span attribs [media]

compileImageToBlock :: Attr -> [Inline] -> Text -> Text -> [Inline] -> Filter Block
compileImageToBlock attr alt url title caption = do
  runAttr attr $ do
    media <- compileImage alt url title caption
    attribs <-
      injectBorder
        >> injectClasses ["media"]
        >> takeUsual
        >> extractAttr
    return $ Div attribs [Plain [media]]

compileCodeBlockToBlock :: Attr -> Text -> [Inline] -> Filter Block
compileCodeBlockToBlock attr code caption = return $ Div nullAttr []

compileLineBlockToBlock :: [(Attr, [Inline], Text, Text)] -> [Inline] -> Filter Block
compileLineBlockToBlock images caption = return $ Div nullAttr []

compileImage :: [Inline] -> Text -> Text -> [Inline] -> Attrib Inline
compileImage alt url title caption = do
  uri <- URI.mkURI url
  mediaType <- classifyMedia uri <$> src
  case Map.lookup mediaType transformers of
    Just transform -> transform uri caption
    Nothing -> error $ "No transformer for media type " <> show mediaType

transformers :: Map MediaT (URI -> [Inline] -> Attrib Inline)
transformers =
  Map.fromList
    [ -- (EmbedSvgT, svgHtml),
      -- (PdfT, objectHtml "application/pdf"),
      -- (MviewT, mviewHtml),
      (IframeT, iframeBlock),
      (ImageT, imageBlock),
      -- (VideoT, videoHtml),
      (StreamT, streamBlock)
      -- (AudioT, audioHtml),
      -- (RenderT, renderCodeHtml),
      -- (JavascriptT, javascriptHtml)
    ]

imageBlock :: URI -> [Inline] -> Attrib Inline
imageBlock uri caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  let fileName = toText $ takeFileName $ toString turl
  (innerSizes, outerSizes) <- calcImageSizes
  imgAttr <-
    injectClasses ["processed"]
      >> injectStyles innerSizes
      >> extractAttr
  figureAttr <-
    injectClasses ["image"]
      >> injectStyles outerSizes
      >> extractAttr
  return $ wrapFigure figureAttr caption $ Image imgAttr ([Str fileName]) (turl, "")

iframeBlock :: URI -> [Inline] -> Attrib Inline
iframeBlock uri caption = do
  turi <- lift $ transformUri uri ""
  let turl = URI.render turi
  xformRersourceAttribs ["image"]
  (innerSizes, outerSizes) <- calcIframeSizes
  iframeAttr <-
    injectAttribute ("data-src", turl)
      >> injectAttribute ("allow", "fullscreen")
      >> injectStyles innerSizes
      >> extractAttr
  figureAttr <-
    injectClasses ["iframe"]
      >> injectStyles outerSizes
      >> extractAttr
  return $
    wrapFigure figureAttr caption $
      tag "iframe" $ Span iframeAttr []

streamBlock :: URI -> [Inline] -> Attrib Inline
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

  (innerSizes, outerSizes) <- calcImageSizes
  iframeAttr <-
    takeAutoplay
      >> injectAttribute ("src", URI.render streamUri)
      >> extractAttr
  aspect <- cutAttrib "aspect"
  case aspect of
    Just a -> injectStyle ("--aspect-ratio", calcAspect a)
    Nothing -> return ()
  injectClass "fluid-iframe"
  fluidAttr <- extractAttr
  figureAttr <-
    injectClasses ["stream"]
      >> injectStyles outerSizes
      >> extractAttr
  return $
    wrapFigure figureAttr caption $
      tag "div" $
        Span fluidAttr [tag "iframe" $ Span iframeAttr []]
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

wrapFigure :: Attr -> [Inline] -> Inline -> Inline
wrapFigure attr caption inline =
  tag "figure" $
    Span
      attr
      ([inline] <> [tag "figcaption" $ Span nullAttr caption | not (null caption)])

calcImageSizes :: Attrib ([(Text, Text)], [(Text, Text)])
calcImageSizes = do
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
          ( [("height", "auto"), ("width", "100%")],
            [("height", "auto"), ("width", fromJust width)]
          )
      | isNothing width && isJust height ->
        return
          ( [("height", fromJust height), ("width", "auto")],
            [("height", "auto"), ("width", "auto")]
          )
      | otherwise -> return ([], [])

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