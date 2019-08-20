{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Filter.Filter
  ( OutputFormat(..)
  , Disposition(..)
  , processPandoc
  , processSlides
  , useCachedImages
  , escapeToFilePath
  , extractLocalImagePathes
  , renderMediaTags
  , extractFigures
  , iframeExtensions
  , audioExtensions
  , videoExtensions
  , convertMediaAttributes
  ) where

import Text.Decker.Filter.Layout
import Text.Decker.Filter.MarioCols
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

import Control.Exception
import Control.Monad.Loops as Loop
import Control.Monad.State
import Data.Default ()
import Data.List
import Data.List.Split
import Data.Maybe
import Development.Shake (Action)
import qualified Network.URI as U
import System.Directory
import System.FilePath
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Lens
import Text.Pandoc.Shared
import Text.Pandoc.Walk

processPandoc ::
     (Pandoc -> Decker Pandoc)
  -> FilePath
  -> Disposition
  -> Provisioning
  -> Pandoc
  -> Action Pandoc
processPandoc transform base disp prov pandoc =
  evalStateT (transform pandoc) (DeckerState base disp prov 0 [] [])

-- | Split join columns with CSS3. Must be performed after `wrapBoxes`.
splitJoinColumns :: Slide -> Decker Slide
splitJoinColumns slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html -> return $ Slide header $ concatMap wrapRow rowBlocks
      where rowBlocks =
              split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
            wrapRow row@(first:_)
              | hasClass "split" first = [Div ("", ["css-columns"], []) row]
            wrapRow row = row
    Disposition _ _ -> return slide

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.
fragmentRelated :: [String]
fragmentRelated =
  [ "fragment"
  , "grow"
  , "shrink"
  , "roll-in"
  , "fade-in"
  , "fade-out"
  , "current-visible"
  , "highlight-current-blue"
  , "highlight-red"
  , "highlight-green"
  , "highlight-blu"
  ]

deFragment :: [String] -> [String]
deFragment = filter (`notElem` fragmentRelated)

allImages :: Inline -> [Inline]
allImages image@Image {} = [image]
allImages _ = []

zapImages :: Inline -> Inline
zapImages Image {} = Space
zapImages inline = inline

-- Transform inline image or video elements within the header line with
-- background attributes of the respective section. 
handleBackground :: Slide -> Decker Slide
handleBackground slide@(Slide Nothing blocks) = return slide
handleBackground slide@(Slide header blocks) =
  case header of
    Just (Header 1 (headerId, headerClasses, headerAttributes) inlines) ->
      case query allImages inlines of
        image@(Image (_, imageClasses, imageAttributes) _ (imageSrc, _)):_ -> do
          disp <- gets disposition
          case disp of
            Disposition Deck Html ->
              return $
              Slide
                (Just
                   (Header -- Construct a new header with the necessary attributes for RevealJs background content
                      1
                      ( headerId
                      , headerClasses ++ imageClasses
                      , srcAttribute imageSrc :
                        headerAttributes ++ map transform imageAttributes)
                      (walk zapImages inlines)))
                blocks
            Disposition _ _ ->
              return $
              Slide
                (Just
                   (Header
                      1
                      (headerId, headerClasses, headerAttributes)
                      (walk zapImages inlines)))
                (Para [image] : blocks)
        _ -> return slide -- Find the fist Image in the header text -- There is no image in the header
    Nothing -> throw $ InternalException "Illegal block in slide header"
  where
    transform ("size", value) = ("data-background-size", value)
    transform ("position", value) = ("data-background-position", value)
    transform ("repeat", value) = ("data-background-repeat", value)
    transform ("loop", value) = ("data-background-video-loop", value)
    transform ("muted", value) = ("data-background-video-muted", value)
    transform ("color", value) = ("data-background-color", value)
    transform ("interactive", value) = ("data-background-interactive", value)
    transform kv = kv
    srcAttribute src =
      case classifyFilePath src of
        VideoMedia -> ("data-background-video", src)
        AudioMedia -> ("data-background-audio", src)
        IframeMedia -> ("data-background-iframe", src)
        ImageMedia -> ("data-background-image", src)

-- | Wrap boxes around H2 headers and the following content. All attributes are
-- promoted from the H2 header to the enclosing DIV.
wrapBoxes :: Slide -> Decker Slide
wrapBoxes slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition _ Latex -> return slide
    Disposition _ Html -> return $ Slide header $ concatMap wrap boxes
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap (Header 2 (id_, cls, kvs) text:blocks) =
      [ Div
          ("", "box" : cls, kvs)
          (Header 2 (id_, deFragment cls, kvs) text : blocks)
      ]
    wrap box = box

-- | Map over all active slides in a deck. 
mapSlides :: (Slide -> Decker Slide) -> Pandoc -> Decker Pandoc
mapSlides action (Pandoc meta blocks) = do
  slides <- selectActiveContent (toSlides blocks)
  Pandoc meta . fromSlides <$> mapM action slides

selectActiveSlideContent :: Slide -> Decker Slide
selectActiveSlideContent (Slide header body) =
  Slide header <$> selectActiveContent body

-- | Slide specific processing.
processSlides :: Pandoc -> Decker Pandoc
processSlides pandoc = mapSlides (concatM actions) pandoc
  where
    actions :: [Slide -> Decker Slide]
    actions =
      case pandocMeta lookupMetaBool pandoc "mario" of
        Just True ->
          [ marioCols
          , wrapBoxes
          , selectActiveSlideContent
          , splitJoinColumns
          , layoutSlide
          , handleBackground
          ]
        _ ->
          [ wrapBoxes
          , selectActiveSlideContent
          , splitJoinColumns
          , layoutSlide
          , handleBackground
          ]

selectActiveContent :: HasAttr a => [a] -> Decker [a]
selectActiveContent fragments = do
  disp <- gets disposition
  return $
    case disp of
      Disposition Deck _ -> dropByClass ["handout"] fragments
      Disposition Handout _ -> dropByClass ["deck", "notes"] fragments
      Disposition Page _ -> dropByClass ["notes", "deck", "handout"] fragments

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where
    repl c =
      if c `elem` [':', '!', '/']
        then '|'
        else c

useCachedImages :: FilePath -> Inline -> IO Inline
useCachedImages cacheDir image@(Image (ident, cls, values) inlines (url, imgTitle)) = do
  let cached = cacheDir </> escapeToFilePath url
  exists <- doesFileExist cached
  if exists
    then return
           (Image (ident, "cached" : cls, values) inlines (cached, imgTitle))
    else return image
useCachedImages _ inline = return inline

localImagePath :: Inline -> [FilePath]
localImagePath (Image _ _ (url, _)) =
  if isHttpUri url
    then []
    else [url]
localImagePath _ = []

extractLocalImagePathes :: Pandoc -> [FilePath]
extractLocalImagePathes = Text.Pandoc.Walk.query localImagePath

isHttpUri :: String -> Bool
isHttpUri url =
  case U.parseURI url of
    Just uri -> U.uriScheme uri `elem` ["http:", "https:"]
    Nothing -> False

renderMediaTags :: Pandoc -> Decker Pandoc
renderMediaTags pandoc = do
  disp <- gets disposition
  lift $ walkM (renderMediaTag disp) pandoc

-- | File extensions that signify video content.
videoExtensions :: [String]
videoExtensions =
  [".mp4", ".m4v", ".webm", ".ogg", ".avi", ".dv", ".mp2", ".mov", ".qt"]

-- | File extensions that signify audio content.
audioExtensions :: [String]
audioExtensions = [".m4a", ".mp3", ".ogg", ".wav"]

-- | File extensions that signify iframe content.
iframeExtensions :: [String]
iframeExtensions = [".html", ".htm", ".pdf", ".php"]

uriPathExtension :: String -> String
uriPathExtension reference =
  case U.parseRelativeReference reference of
    Nothing -> takeExtension reference
    Just uri -> takeExtension (U.uriPath uri)

classifyFilePath :: FilePath -> MediaType
classifyFilePath name =
  case uriPathExtension name of
    ext
      | ext `elem` videoExtensions -> VideoMedia
    ext
      | ext `elem` audioExtensions -> AudioMedia
    ext
      | ext `elem` iframeExtensions -> IframeMedia
    _ -> ImageMedia

-- Renders an image with a video reference to a video tag in raw HTML. Faithfully
-- transfers attributes to the video tag.
renderMediaTag :: Disposition -> Inline -> Action Inline
renderMediaTag disp (Image attrs@(ident, cls, values) [] (url, tit)) =
  liftIO imageVideoTag
  where
    imageVideoTag =
      if uriPathExtension url == ".svg" && "embed" `elem` cls
        then do
          fileContent <- catch (readFile url) svgLoadErrorHandler
          return $
            if attrs /= nullAttr
              then Span (ident, cls, values) [toHtml fileContent]
              else toHtml fileContent
        else return $
             toHtml $
             renderHtml $
             if "iframe" `elem` cls
               then mediaTag (H.iframe "Browser does not support iframe.")
               else createMediaTag
    createMediaTag =
      case classifyFilePath url of
        VideoMedia -> mediaTag (H.video "Browser does not support video.")
        AudioMedia -> mediaTag (H.audio "Browser does not support audio.")
        IframeMedia -> mediaTag (H.iframe "Browser does not support iframe.")
        ImageMedia -> mediaTag H.img
    appendAttr element (key, value) =
      element H.! customAttribute (H.stringTag key) (H.toValue value)
    mediaTag tag =
      ifNotEmpty A.id ident $
      ifNotEmpty A.class_ (unwords cls) $
      ifNotEmpty A.title tit $ foldl appendAttr tag transformedValues
    ifNotEmpty attr value element =
      if value == ""
        then element
        else element H.! attr (H.toValue value)
    srcAttr =
      if disp == Disposition Deck Html
        then "data-src"
        else "src"
    transformedValues =
      case classifyFilePath url of
        VideoMedia -> lazyLoad $ retrieveVideoStart $ transformImageSize values
        _ -> lazyLoad (transformImageSize values, Nothing)
    lazyLoad (vs, (Just start)) = (srcAttr, url ++ "#t=" ++ start) : vs
    lazyLoad (vs, Nothing) = (srcAttr, url) : vs
renderMediaTag disp (Image attrs@(ident, cls, values) inlines (url, tit)) = do
  image <- renderMediaTag disp (Image attrsForward [] (url, tit))
  return $
    Span
      nullAttr
      ([toHtml "<figure>", image, toHtml "<figcaption>"] ++
       inlines ++ [toHtml "</figcaption>", toHtml "</figure>"])
  where
    attrsForward = (ident, cls, ("alt", stringify inlines) : values)
-- | return inline if it is no image
renderMediaTag _ inline = return inline

svgLoadErrorHandler :: IOException -> IO String
svgLoadErrorHandler e = return "<div>Couldn't load SVG</div>"

-- | Converts attributes 
convertMediaAttributes :: Attr -> Attr
convertMediaAttributes attrs =
  convertMediaAttributeGatherStyle $ convertMediaAttributeImageSize attrs

convertMediaAttributeGatherStyle :: Attr -> Attr
convertMediaAttributeGatherStyle (id, cls, vals) = (id, cls, vals')
  where
    (style_cls, cls') = partition (\x -> fst x == "style") vals
    style_combined =
      if null style_cls
        then []
        else [("style", intercalate "" $ map snd style_cls)]
    vals' = style_combined ++ cls'

convertMediaAttributeImageSize :: Attr -> Attr
convertMediaAttributeImageSize (id, cls, vals) = (id, cls, vals_processed)
  where
    (height, vals') = partition (\x -> fst x == "height") vals
    height_attr =
      if null height
        then []
        else [("style", "height:" ++ snd (head height) ++ ";")]
    (width, vals'') = partition (\x -> fst x == "width") vals'
    width_attr =
      if null width
        then []
        else [("style", "width:" ++ snd (head width) ++ ";")]
    vals_processed = vals'' ++ height_attr ++ width_attr

-- | small wrapper around @RawInline (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toHtml :: String -> Inline
toHtml = RawInline (Format "html")

-- | Mimic pandoc for handling the 'width' and 'height' attributes of images.
-- That is, transfer 'width' and 'height' attribute values to css style values
-- and add them to the 'style' attribute value.
transformImageSize :: [(String, String)] -> [(String, String)]
transformImageSize attributes =
  let style :: [String]
      style =
        delete "" $
        split (dropDelims $ oneOf ";") $
        fromMaybe "" $ snd <$> find (\(k, _) -> k == "style") attributes
      unstyled :: [(String, String)]
      unstyled = filter (\(k, _) -> k /= "style") attributes
      unsized =
        filter (\(k, _) -> k /= "width") $
        filter (\(k, _) -> k /= "height") unstyled
      size =
        ( snd <$> find (\(k, _) -> k == "width") unstyled
        , snd <$> find (\(k, _) -> k == "height") unstyled)
      sizeStyle =
        case size of
          (Just w, Just h) -> ["width:" ++ w, "height:" ++ h]
          (Just w, Nothing) -> ["width:" ++ w, "height:auto"]
          (Nothing, Just h) -> ["width:auto", "height:" ++ h]
          (Nothing, Nothing) -> []
      css = style ++ sizeStyle
      styleAttr = ("style", intercalate ";" $ reverse $ "" : css)
   in if null css
        then unstyled
        else styleAttr : unsized

extractFigures :: Pandoc -> Decker Pandoc
extractFigures pandoc = return $ walk extractFigure pandoc

extractFigure :: Block -> Block
extractFigure (Para content) =
  case content of
    [Span attr inner@(RawInline (Format "html") "<figure>":otherContent)] ->
      Div attr [Plain inner]
    a -> Para a
extractFigure b = b

-- | Retrieves the start attribute for videos to append it to the url
retrieveVideoStart :: [(String, String)] -> ([(String, String)], Maybe String)
retrieveVideoStart attributes = (attributeRest, urlStartMarker)
  where
    attributeRest = filter (\(k, _) -> k /= "start") attributes
    urlStartMarker = snd <$> find (\(k, _) -> k == "start") attributes
