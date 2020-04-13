{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Filter.Filter
  ( OutputFormat(..)
  , Disposition(..)
  , processPandoc
  , processSlides
  , escapeToFilePath
  , renderMediaTags
  , extractFigures
  , iframeExtensions
  , audioExtensions
  , videoExtensions
  , convertMediaAttributes
  , filterNotebookSlides
  , wrapSlidesinDivs
  ) where

import Control.Exception
import Control.Lens
import Control.Monad.Loops as Loop
import Control.Monad.State
import Data.Default ()
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Development.Shake (Action)
import qualified Network.URI as U
import System.FilePath
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Decker.Filter.Layout
import Text.Decker.Filter.MarioCols
import Text.Decker.Filter.Slide
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
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
  evalStateT (transform pandoc) (DeckerState base disp prov)

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
    Disposition Handout Html ->
      return $ Slide header $ concatMap wrapRow rowBlocks
      where rowBlocks =
              split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
            wrapRow row@(first:_)
              | hasClass "split" first = [Div ("", ["css-columns"], []) row]
            wrapRow row = row
    Disposition _ _ -> return slide

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.
fragmentRelated :: [Text.Text]
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

deFragment :: [Text.Text] -> [Text.Text]
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
            Disposition Deck Html -> do
              let cls = headerClasses ++ imageClasses
              return $
                Slide
                  (Just
                     (Header -- Construct a new header with the necessary attributes for RevealJs background content
                        1
                        ( headerId
                        , cls
                        , srcAttribute imageSrc cls :
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
    srcAttribute :: Text.Text -> [Text.Text] -> (Text.Text, Text.Text)
    srcAttribute src cls =
      case classifyFilePath src cls of
        VideoMedia -> ("data-background-video", src)
        AudioMedia -> ("data-background-audio", src)
        IframeMedia -> ("data-background-iframe", src)
        ImageMedia -> ("data-background-image", src)

-- | Wrap boxes around H2 headers and the following content. All attributes are
-- promoted from the H2 header to the enclosing DIV. Since Pandoc 2.9 the class
-- "column" needs to be added to boxes to prevent sectioning by the Pandoc
-- writer (see `Text.Pandoc.Shared.makeSections`). This must only be done for
-- slide decks, not for handouts or pages.
wrapBoxes :: Slide -> Decker Slide
wrapBoxes slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition Deck Html -> return $ Slide header $ concatMap (wrap True) boxes
    Disposition _ Html -> return $ Slide header $ concatMap (wrap False) boxes
    Disposition _ Latex -> return slide
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap isDeck (Header 2 (id_, cls, kvs) text:blocks) =
      let tags =
            if isDeck
              then ["box", "columns"]
              else ["box"]
       in [ Div
              ("", tags ++ cls, kvs)
              (Header 2 (id_, deFragment cls, kvs) text : blocks)
          ]
    wrap _ box = box

-- | Map over all active slides in a deck. 
mapSlides :: (Slide -> Decker Slide) -> Pandoc -> Decker Pandoc
mapSlides action (Pandoc meta blocks) = do
  slides <- selectActiveContent (toSlides blocks)
  Pandoc meta . fromSlides <$> mapM action slides

filterNotebookSlides :: Pandoc -> Pandoc
filterNotebookSlides (Pandoc meta blocks) =
  let inNotebook = fromSlides $ filter notebook (toSlides blocks)
      stripped = walk strip inNotebook
      strip (Header level _ inlines) = Header level nullAttr inlines
      strip (CodeBlock (_, classes, _) code)
        | "code" `notElem` classes = CodeBlock nullAttr code
      strip block = block
      notebook slide = "notebook" `elem` (view (attributes . attrClasses) slide)
   in Pandoc meta (deDiv stripped)

wrapSlidesinDivs :: Pandoc -> Pandoc
wrapSlidesinDivs (Pandoc meta blocks) =
  Pandoc meta $ fromSlidesWrapped $ toSlides blocks

selectActiveSlideContent :: Slide -> Decker Slide
selectActiveSlideContent (Slide header body) =
  Slide header <$> selectActiveContent body

-- Splice all the Divs back into the stream of Blocks 
deDiv :: [Block] -> [Block]
deDiv = foldr flatten []
  where
    flatten (Div attr blocks) result = blocks ++ result
    flatten block result = block : result

-- | Slide specific processing.
processSlides :: Pandoc -> Decker Pandoc
processSlides pandoc = mapSlides (concatM actions) pandoc
  where
    actions :: [Slide -> Decker Slide]
    actions =
      case pandocMeta getMetaBool pandoc "mario" of
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
      Disposition Deck _ -> dropByClass ["comment", "handout"] fragments
      Disposition Handout _ ->
        dropByClass ["comment", "deck", "notes"] fragments
      Disposition Page _ ->
        dropByClass ["comment", "notes", "deck", "handout"] fragments
      Disposition Notebook _ ->
        dropByClass ["comment", "notes", "deck", "handout"] fragments

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where
    repl c =
      if c `elem` [':', '!', '/']
        then '|'
        else c

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
iframeExtensions = [".html", ".htm", ".pdf", ".php", "ipynb"]

uriPathExtension :: String -> String
uriPathExtension reference =
  case U.parseRelativeReference reference of
    Nothing -> takeExtension reference
    Just uri -> takeExtension (U.uriPath uri)

classifyFilePath :: Text.Text -> [Text.Text] -> MediaType
classifyFilePath name cls =
  case uriPathExtension (Text.unpack name) of
    ext
      | ext `elem` videoExtensions -> VideoMedia
    ext
      | ext `elem` audioExtensions -> AudioMedia
    ext
      | ext `elem` iframeExtensions || "iframe" `elem` cls -> IframeMedia
    _ -> ImageMedia

-- Renders an image with a video reference to a video tag in raw HTML. Faithfully
-- transfers attributes to the video tag.
renderMediaTag :: Disposition -> Inline -> Action Inline
renderMediaTag disp (Image attrs@(ident, cls, values) [] (url, tit)) =
  liftIO imageVideoTag
  where
    urlS = Text.unpack url
    imageVideoTag =
      if uriPathExtension urlS == ".svg" && "embed" `elem` cls
        then do
          fileContent <- catch (Text.readFile urlS) svgLoadErrorHandler
          return $
            if attrs /= nullAttr
              then Span (ident, cls, values) [toHtml fileContent]
              else toHtml fileContent
        else return $
             toHtml $
             Text.pack $
             renderHtml $
             if "iframe" `elem` cls
               then mediaTag (H.iframe "Browser does not support iframe.")
               else createMediaTag
    createMediaTag =
      case classifyFilePath url cls of
        VideoMedia -> mediaTag (H.video "Browser does not support video.")
        AudioMedia -> mediaTag (H.audio "Browser does not support audio.")
        IframeMedia -> mediaTag (H.iframe "Browser does not support iframe.")
        ImageMedia -> mediaTag H.img
    appendAttr element (key, value) =
      element H.!
      customAttribute (H.stringTag $ Text.unpack key) (H.toValue value)
    mediaTag tag =
      ifNotEmpty A.id ident $
      ifNotEmpty A.class_ (Text.unwords cls) $
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
      case classifyFilePath url cls of
        VideoMedia -> lazyLoad $ retrieveVideoStart $ transformImageSize values
        _ -> lazyLoad (transformImageSize values, Nothing)
    lazyLoad (vs, (Just start)) = (srcAttr, url <> "#t=" <> start) : vs
    lazyLoad (vs, Nothing) = (srcAttr, url) : vs
renderMediaTag disp (Image (ident, cls, values) inlines (url, tit)) = do
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

svgLoadErrorHandler :: IOException -> IO Text.Text
svgLoadErrorHandler _ = return "<div>Couldn't load SVG</div>"

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
        else [("style", Text.intercalate "" $ map snd style_cls)]
    vals' = style_combined <> cls'

convertMediaAttributeImageSize :: Attr -> Attr
convertMediaAttributeImageSize (id, cls, vals) = (id, cls, vals_processed)
  where
    (height, vals') = partition (\x -> fst x == "height") vals
    height_attr =
      if null height
        then []
        else [("style", "height:" <> snd (head height) <> ";")]
    (width, vals'') = partition (\x -> fst x == "width") vals'
    width_attr =
      if null width
        then []
        else [("style", "width:" <> snd (head width) <> ";")]
    vals_processed = vals'' ++ height_attr ++ width_attr

-- | small wrapper around @RawInline (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toHtml :: Text.Text -> Inline
toHtml = RawInline (Format "html")

-- | Mimic pandoc for handling the 'width' and 'height' attributes of images.
-- That is, transfer 'width' and 'height' attribute values to css style values
-- and add them to the 'style' attribute value.
transformImageSize :: [(Text.Text, Text.Text)] -> [(Text.Text, Text.Text)]
transformImageSize attributes =
  let style :: [Text.Text]
      style =
        Text.splitOn ";" $
        fromMaybe "" $ snd <$> find (\(k, _) -> k == "style") attributes
      unstyled :: [(Text.Text, Text.Text)]
      unstyled = filter (\(k, _) -> k /= "style") attributes
      unsized =
        filter (\(k, _) -> k /= "width") $
        filter (\(k, _) -> k /= "height") unstyled
      size =
        ( snd <$> find (\(k, _) -> k == "width") unstyled
        , snd <$> find (\(k, _) -> k == "height") unstyled)
      sizeStyle =
        case size of
          (Just w, Just h) -> ["width:" <> w, "height:" <> h]
          (Just w, Nothing) -> ["width:" <> w, "height:auto"]
          (Nothing, Just h) -> ["width:auto", "height:" <> h]
          (Nothing, Nothing) -> []
      css = style <> sizeStyle
      styleAttr = ("style", Text.intercalate ";" $ reverse $ "" : css)
   in if null css
        then unstyled
        else styleAttr : unsized

extractFigures :: Pandoc -> Decker Pandoc
extractFigures pandoc = return $ walk extractFigure pandoc

extractFigure :: Block -> Block
extractFigure (Para content) =
  case content of
    [Span attr inner@(RawInline (Format "html") "<figure>":_)] ->
      Div attr [Plain inner]
    a -> Para a
extractFigure b = b

-- | Retrieves the start attribute for videos to append it to the url
retrieveVideoStart ::
     [(Text.Text, Text.Text)] -> ([(Text.Text, Text.Text)], Maybe Text.Text)
retrieveVideoStart attributes = (attributeRest, urlStartMarker)
  where
    attributeRest = filter (\(k, _) -> k /= "start") attributes
    urlStartMarker = snd <$> find (\(k, _) -> k == "start") attributes
