{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Filter
  ( RowLayout(..)
  , OutputFormat(..)
  , transformImageSize
  , lazyLoadImage
  , iframeExtensions
  , audioExtensions
  , videoExtensions
  , processPandoc
  , processSlides
  , renderMediaTags
  ) where

import Common
import Control.Exception

import Control.Monad.Loops as Loop
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Default ()
import Data.List
import Data.List.Extra (for)
import Data.List.Split
import Data.Maybe
import Data.Tuple.Select
import Development.Shake (Action)
import Network.HTTP.Conduit hiding (InternalException)
import Network.HTTP.Simple
import qualified Network.URI as U
import Network.URI (parseURI, uriScheme)
import System.Directory
import System.FilePath
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H
       ((!), audio, iframe, iframe, img, stringTag, toValue, video)
import qualified Text.Blaze.Html5.Attributes as A
       (alt, class_, id, title)
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Read

processPandoc ::
     (Pandoc -> Decker Pandoc)
  -> FilePath
  -> Disposition
  -> Provisioning
  -> Pandoc
  -> Action Pandoc
processPandoc transform base disp prov pandoc =
  evalStateT (transform pandoc) (DeckerState base disp prov 0 [] [])

isBoxDelim :: Block -> Bool
isBoxDelim (Header 2 _ _) = True
isBoxDelim _ = False

-- | Slide layouts are rows of one ore more columns.
data RowLayout = RowLayout
  { lname :: String
  , rows :: [Row]
  } deriving (Eq, Show)

-- | A row consists of one or more column. 
data Row
  = SingleColumn String
  | MultiColumn [String]
  deriving (Eq, Show)

type Area = [Block]

type AreaMap = [(String, Area)]

rowLayouts :: [RowLayout]
rowLayouts =
  [ RowLayout
      "columns"
      [ SingleColumn "top"
      , MultiColumn ["left", "center", "right"]
      , SingleColumn "bottom"
      ]
  ]

rowAreas :: Row -> [String]
rowAreas (SingleColumn area) = [area]
rowAreas (MultiColumn areas) = areas

layoutAreas :: RowLayout -> [String]
layoutAreas l = concatMap rowAreas $ rows l

hasRowLayout :: Block -> Maybe RowLayout
hasRowLayout block =
  attribValue "layout" block >>= (\l -> find ((==) l . lname) rowLayouts)

renderRow :: AreaMap -> Row -> Maybe Block
renderRow areaMap (SingleColumn area) =
  lookup area areaMap >>= Just . Div ("", ["single-column-row"], [])
renderRow areaMap (MultiColumn areas) =
  Just $
  Div ("", ["multi-column-row", "multi-column-row-" ++ show (length areas)], []) $
  mapMaybe renderArea (zip [1 ..] areas)
  where
    renderArea (i, area) = lookup area areaMap >>= Just . renderColumn . (i, )

renderColumn :: (Int, [Block]) -> Block
renderColumn (i, blocks) =
  let grow = fromMaybe (1 :: Int) $ lookup "grow" (keyvals blocks) >>= readMaybe
  in Div
       ( ""
       , ["grow-" ++ show grow, "column", "column-" ++ show i]
       , keyvals blocks)
       blocks

renderLayout :: AreaMap -> RowLayout -> [Block]
renderLayout areaMap l = mapMaybe (renderRow areaMap) (rows l)

slideAreas :: [String] -> [Block] -> AreaMap
slideAreas names blocks =
  mapMaybe (\area -> firstClass names (head area) >>= Just . (, area)) $
  filter (not . null) $ split (keepDelimsL $ whenElt (hasAnyClass names)) blocks

layoutSlide :: Slide -> Decker Slide
layoutSlide slide@(Slide (Just header) body) = do
  disp <- gets disposition
  case disp of
    Disposition _ Latex -> return slide
    Disposition _ Html -> do
      case hasRowLayout header of
        Just layout ->
          let names = layoutAreas layout
              areas = slideAreas names body
          in return $ Slide (Just header) $ renderLayout areas layout
        Nothing -> return slide
layoutSlide slide = return slide

class HasAttrib a where
  attributes :: a -> Attr

instance HasAttrib Slide where
  attributes (Slide (Just header) _) = attributes header
  attributes _ = nullAttr

instance HasAttrib Block where
  attributes (Div attribs _) = attribs
  attributes (Header 1 attribs _) = attribs
  attributes (CodeBlock attribs _) = attribs
  attributes (Para [Image attribs _ _]) = attribs
  attributes _ = nullAttr

instance HasAttrib Inline where
  attributes (Code attribs _) = attribs
  attributes (Link attribs _ _) = attribs
  attributes (Image attribs _ _) = attribs
  attributes (Span attribs _) = attribs
  attributes _ = nullAttr

instance HasAttrib [Block] where
  attributes (first:_) = attributes first
  attributes _ = nullAttr

instance HasAttrib (Maybe Block) where
  attributes (Just block) = attributes block
  attributes Nothing = nullAttr

classes :: HasAttrib a => a -> [String]
classes = sel2 . attributes

keyvals :: HasAttrib a => a -> [(String, String)]
keyvals = sel3 . attributes

hasClass :: HasAttrib a => String -> a -> Bool
hasClass which = elem which . classes

hasAnyClass :: HasAttrib a => [String] -> a -> Bool
hasAnyClass which = isJust . firstClass which

firstClass :: HasAttrib a => [String] -> a -> Maybe String
firstClass which fragment = listToMaybe $ filter (`hasClass` fragment) which

attribValue :: HasAttrib a => String -> a -> Maybe String
attribValue which = lookup which . keyvals

dropByClass :: HasAttrib a => [String] -> [a] -> [a]
dropByClass which = filter (not . any (`elem` which) . classes)

-- | Split join columns with CSS3. Must be performed after `wrapBoxes`.
splitJoinColumns :: Slide -> Decker Slide
splitJoinColumns slide@(Slide header body) = do
  disp <- gets disposition
  case disp of
    Disposition _ Latex -> return slide
    Disposition _ Html -> do
      return $ Slide header $ concatMap wrapRow rowBlocks
  where
    rowBlocks =
      split (keepDelimsL $ whenElt (hasAnyClass ["split", "join"])) body
    wrapRow row@(first:_)
      | hasClass "split" first = [Div ("", ["css-columns"], []) row]
    wrapRow row = row

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

handleBackground :: Slide -> Decker Slide
handleBackground = handleBackgroundRevealJs

-- Transform inline image or video elements within the header line with
-- background attributes of the respective section. 
handleBackgroundRevealJs :: Slide -> Decker Slide
handleBackgroundRevealJs slide@(Slide Nothing blocks) = return slide
handleBackgroundRevealJs slide@(Slide header blocks) =
  case header of
    Just (Header 1 (headerId, headerClasses, headerAttributes) inlines) ->
      case query allImages inlines of
        Image (_, imageClasses, imageAttributes) _ (imageSrc, _):_ ->
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
wrapBoxesOne :: Slide -> Decker Slide
wrapBoxesOne slide@(Slide header body) = do
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

-- A slide has maybe a header followed by zero or more blocks.
data Slide = Slide
  { header :: Maybe Block
  , body :: [Block]
  } deriving (Eq, Show)

isSlideSeparator :: Block -> Bool
isSlideSeparator (Header 1 _ _) = True
isSlideSeparator HorizontalRule = True
isSlideSeparator _ = False

-- Converts blocks to slides. Slides start at H1 headers or at horizontal rules.
-- A horizontal rule followed by a H1 header collapses to one slide.
toSlides :: [Block] -> [Slide]
toSlides blocks = map extractHeader $ filter (not . null) slideBlocks
  where
    slideBlocks =
      split (keepDelimsL $ whenElt isSlideSeparator) $ killEmpties blocks
  -- Deconstruct a list of blocks into a Slide
    extractHeader (header@(Header 1 _ _):bs) = Slide (Just header) bs
    extractHeader (HorizontalRule:bs) = extractHeader bs
    extractHeader blocks = Slide Nothing blocks
  -- Remove redundant slide markers
    killEmpties (HorizontalRule:header@(Header _ _ _):blocks) =
      header : killEmpties blocks
    killEmpties (b:bs) = b : killEmpties bs
    killEmpties [] = []

-- Render slides as a list of Blocks. Always separate slides with a horizontal
-- rule. Slides with the `notes` class are wrapped in DIV and are used as
-- spreaker notes by RevalJs.
fromSlides :: [Slide] -> [Block]
fromSlides = concatMap prependHeader
  where
    prependHeader (Slide (Just header) body)
      | hasClass "notes" header = [Div (attributes header) (header : body)]
    prependHeader (Slide (Just header) body) = HorizontalRule : header : body
    prependHeader (Slide Nothing body) = HorizontalRule : body

-- | Map over all active slides in a deck. 
mapSlides :: (Slide -> Decker Slide) -> Pandoc -> Decker Pandoc
mapSlides action (Pandoc meta blocks) = do
  slides <- selectActiveContent (toSlides blocks)
  (Pandoc meta . fromSlides) <$> mapM action slides

selectActiveSlideContent :: Slide -> Decker Slide
selectActiveSlideContent (Slide header body) =
  Slide header <$> selectActiveContent body

-- | Slide specific processing.
processSlides :: Pandoc -> Decker Pandoc
processSlides = mapSlides (concatM actions)
  where
    actions :: [Slide -> Decker Slide]
    actions =
      [ wrapBoxesOne
      , selectActiveSlideContent
      , splitJoinColumns
      , layoutSlide
      , handleBackground
      ]

selectActiveContent :: HasAttrib a => [a] -> Decker [a]
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
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:", "https:"]
    Nothing -> False

cachePandocImages :: FilePath -> Inline -> IO Inline
cachePandocImages base image@(Image _ _ (url, _))
  | isHttpUri url = do
    cacheImageIO url base
    return image
  | otherwise = return image
cachePandocImages _ inline = return inline

-- | Downloads the image behind the URI and saves it locally. Returns the path of
-- the cached file relative to the base directory.
cacheImageIO :: String -> FilePath -> IO ()
cacheImageIO uri cacheDir = do
  request <- parseRequest uri
  result <- httpLBS request
  let body = getResponseBody result
  let cacheFile = cacheDir </> escapeToFilePath uri
  createDirectoryIfMissing True cacheDir
  L8.writeFile cacheFile body

renderMediaTags :: Pandoc -> Decker Pandoc
renderMediaTags pandoc = do
  disp <- gets disposition
  return $ walk (renderImageAudioVideoTag disp) pandoc

-- | File extensions that signify video content.
videoExtensions :: [String]
videoExtensions =
  [".mp4", ".m4v", ".webm", ".ogg", ".avi", ".dv", ".mp2", ".mov", ".qt"]

-- | File extensions that signify audio content.
audioExtensions :: [String]
audioExtensions = [".m4a", ".mp3", ".ogg", ".wav"]

-- | File extensions that signify iframe content.
iframeExtensions :: [String]
iframeExtensions = [".html", ".html", ".pdf"]

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
renderImageAudioVideoTag :: Disposition -> Inline -> Inline
renderImageAudioVideoTag disp (Image (ident, cls, values) inlines (url, tit)) =
  RawInline (Format "html") (renderHtml imageVideoTag)
  where
    imageVideoTag =
      if "iframe" `elem` cls
        then mediaTag (iframe "Browser does not support iframe.")
        else case classifyFilePath url of
               VideoMedia -> mediaTag (video "Browser does not support video.")
               AudioMedia -> mediaTag (audio "Browser does not support audio.")
               IframeMedia ->
                 mediaTag (iframe "Browser does not support iframe.")
               ImageMedia -> mediaTag img
    appendAttr element (key, value) =
      element ! customAttribute (stringTag key) (toValue value)
    mediaTag tag =
      ifNotEmpty A.id ident $
      ifNotEmpty A.class_ (unwords cls) $
      ifNotEmpty A.alt (stringify inlines) $
      ifNotEmpty A.title tit $ foldl appendAttr tag transformedValues
    ifNotEmpty attr value element =
      if value == ""
        then element
        else element ! attr (toValue value)
    srcAttr =
      if disp == Disposition Deck Html
        then "data-src"
        else "src"
    transformedValues = (lazyLoad . transformImageSize) values
    lazyLoad vs = (srcAttr, url) : vs
renderImageAudioVideoTag _ inline = inline

-- | Mimic pandoc for handling the 'width' and 'height' attributes of images.
-- That is, transfer 'width' and 'height' attribute values to css style values
-- and add them to the 'style' attribute value.
transformImageSize :: [(String, String)] -> [(String, String)]
transformImageSize attributes =
  let style :: [String]
      style =
        delete "" $
        split (dropDelims $ oneOf ";") $
        maybe "" snd (find (\(k, _) -> k == "style") attributes)
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

-- | Moves the `src` attribute to `data-src` to enable reveal.js lazy loading.
lazyLoadImage :: Inline -> IO Inline
lazyLoadImage (Image (ident, cls, values) inlines (url, tit)) = do
  let kvs = ("data-src", url) : [kv | kv <- values, "data-src" /= fst kv]
  return (Image (ident, cls, kvs) inlines ("", tit))
lazyLoadImage inline = return inline
