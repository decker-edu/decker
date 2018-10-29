{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Filter
  ( Layout(..)
  , OutputFormat(..)
  , Disposition(..)
  , processPandoc
  , hasAttrib
  , blockClasses
  , makeSlides
  , makeBoxes
  , useCachedImages
  , escapeToFilePath
  , cachePandocImages
  , extractLocalImagePathes
  , renderMediaTags
  , extractFigures
  , iframeExtensions
  , audioExtensions
  , videoExtensions
  , convertMediaAttributes
  ) where

import Common
import Exception
import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Default ()
import Data.List
import Data.List.Split
import Data.Maybe
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
  ( (!)
  , audio
  , iframe
  , iframe
  , img
  , stringTag
  , toValue
  , video
  )
import Text.Blaze.Html5.Attributes as A (alt, class_, id, title)
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Read hiding (lift)

processPandoc ::
     (Pandoc -> Decker Pandoc)
  -> FilePath
  -> Disposition
  -> Provisioning
  -> Pandoc
  -> Action Pandoc
processPandoc transform base disp prov pandoc =
  evalStateT (transform pandoc) (DeckerState base disp prov 0 [] [])

isSlideHeader :: Block -> Bool
isSlideHeader (Header 1 _ _) = True
isSlideHeader HorizontalRule = True
isSlideHeader _ = False

isBoxDelim :: Block -> Bool
isBoxDelim (Header 2 _ _) = True
isBoxDelim _ = False

hasClass :: String -> Block -> Bool
hasClass which = elem which . blockClasses

hasAnyClass :: [String] -> Block -> Bool
hasAnyClass which = isJust . firstClass which

firstClass :: [String] -> Block -> Maybe String
firstClass which block = listToMaybe $ filter (`hasClass` block) which

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
  hasAttrib "layout" block >>= (\l -> find ((==) l . lname) rowLayouts)

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
  let grow =
        fromMaybe (1 :: Int) $ lookup "grow" (blockKeyvals blocks) >>= readMaybe
   in Div
        ( ""
        , ["grow-" ++ show grow, "column", "column-" ++ show i]
        , blockKeyvals blocks)
        blocks

blockKeyvals :: [Block] -> [(String, String)]
blockKeyvals (first:_) =
  let (_, _, kv) = blockAttribs first
   in kv
blockKeyvals [] = []

renderLayout :: AreaMap -> RowLayout -> [Block]
renderLayout areaMap l = mapMaybe (renderRow areaMap) (rows l)

slideAreas :: [String] -> [Block] -> AreaMap
slideAreas names blocks =
  mapMaybe (\area -> firstClass names (head area) >>= Just . (, area)) $
  filter (not . null) $ split (keepDelimsL $ whenElt (hasAnyClass names)) blocks

layoutSlides :: Slide -> Slide
layoutSlides slide@(header, body) =
  case hasRowLayout header of
    Just l ->
      let names = layoutAreas l
          areas = slideAreas names body
       in (header, renderLayout areas l)
    Nothing -> slide

hasAttrib :: String -> Block -> Maybe String
hasAttrib which (Div (_, _, keyvals) _) = lookup which keyvals
hasAttrib which (Header 1 (_, _, keyvals) _) = lookup which keyvals
hasAttrib which (CodeBlock (_, _, keyvals) _) = lookup which keyvals
hasAttrib which (Para [Image (_, _, keyvals) _ _]) = lookup which keyvals
hasAttrib _ _ = Nothing

blockClasses :: Block -> [String]
blockClasses (Div (_, classes, _) _) = classes
blockClasses (Header 1 (_, classes, _) _) = classes
blockClasses (CodeBlock (_, classes, _) _) = classes
blockClasses (Para [Image (_, classes, _) _ _]) = classes
blockClasses _ = []

blockAttribs :: Block -> (String, [String], [(String, String)])
blockAttribs (Div attribs _) = attribs
blockAttribs (Header 1 attribs _) = attribs
blockAttribs (CodeBlock attribs _) = attribs
blockAttribs (Para [Image attribs _ _]) = attribs
blockAttribs _ = ("", [], [])

-- | Split join columns with CSS3. Must be performed after `wrapBoxes`.
splitJoinColumns :: Slide -> Slide
splitJoinColumns (header, body) = (header, concatMap wrapRow rowBlocks)
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

-- Transform inline image or video elements within the header line with
-- background attributes of the respective section. 
setSlideBackground :: Slide -> Slide
setSlideBackground slide@(Header 1 (headerId, headerClasses, headerAttributes) inlines, slideBody) =
  case query allImages inlines of
    Image (_, imageClasses, imageAttributes) _ (imageSrc, _):_ ->
      ( Header
          1
          ( headerId
          , headerClasses ++ imageClasses
          , srcAttribute imageSrc :
            headerAttributes ++ map transform imageAttributes)
          (walk zapImages inlines)
      , slideBody)
    _ -> slide
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
setSlideBackground slide = slide

-- | Wrap boxes around H2 headers and the following content. All attributes are
-- promoted from the H2 header to the enclosing DIV.
wrapBoxes :: Slide -> Slide
wrapBoxes (header, body) = (header, concatMap wrap boxes)
  where
    boxes = split (keepDelimsL $ whenElt isBoxDelim) body
    wrap (Header 2 (id_, cls, kvs) text:blocks) =
      [ Div
          ("", "box" : cls, kvs)
          (Header 2 (id_, deFragment cls, kvs) text : blocks)
      ]
    wrap box = box

-- | Wrap H1 headers with class notes into a DIV and promote all header
-- attributes to the DIV.
wrapNoteRevealjs :: Slide -> Slide
wrapNoteRevealjs (header@(Header 1 (id_, cls, kvs) _), body)
  | "notes" `elem` cls = (Div (id_, cls, kvs) (header : body), [])
wrapNoteRevealjs slide = slide

type Slide = (Block, [Block])

-- | Map over all slides in a deck. A slide has always a header followed by zero
-- or more blocks.
mapSlides :: (Slide -> Slide) -> Pandoc -> Pandoc
mapSlides func (Pandoc meta blocks) =
  Pandoc meta (concatMap (prependHeader . func) slides)
  where
    slideBlocks = split (keepDelimsL $ whenElt isSlideHeader) blocks
    slides = map extractHeader $ filter (not . null) slideBlocks
    extractHeader (header@(Header 1 _ _):bs) = (header, bs)
    extractHeader (rule@HorizontalRule:bs) = extractHeader bs
    extractHeader bs = (HorizontalRule, bs)
    prependHeader (header, bs) = header : bs

makeSlides :: Pandoc -> Decker Pandoc
makeSlides pandoc = do
  disp <- gets disposition
  let chain =
        case disp of
          Disposition Deck Html ->
            layoutSlides .
            splitJoinColumns . setSlideBackground . wrapBoxes . wrapNoteRevealjs
                -- TODO: Maybe we need some handout specific structure
          Disposition Handout Html ->
            layoutSlides . splitJoinColumns . wrapBoxes . wrapNoteRevealjs
                -- TODO: Maybe we need some latex specific structure
          Disposition Handout Pdf -> Prelude.id
                -- TODO: Probably not much to do here
          Disposition Page Html -> Prelude.id
                -- TODO: Probably not much to do here
          Disposition Page Pdf -> Prelude.id
          Disposition Deck Pdf ->
            throw $
            InternalException "PDF slide decks via LaTeX are not supported"
  return $ mapSlides chain pandoc

makeBoxes :: Pandoc -> Pandoc
makeBoxes = walk (mapSlides wrapBoxes)

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
  pandoc' <- lift $ walkM (renderMediaTag disp) pandoc
  return $ pandoc'

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

-- | File extensions that signify images
imageExtensions :: [String]
imageExtensions =
  [".jpg", ".jpeg", ".png", ".gif", ".tif", ".tiff", ".bmp", ".svg"]

-- | File-extensions that should be treated as 3D model and will be shown with Mario's viewer
-- in an iframe
meshExtensions :: [String]
meshExtensions = [".off", ".obj", ".stl"]

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
    ext
      | ext `elem` meshExtensions -> MeshMedia
    _ -> ImageMedia

-- Renders an image with a video reference to a video tag in raw HTML. Faithfully
-- transfers attributes to the video tag.
renderMediaTag :: Disposition -> Inline -> Action Inline
renderMediaTag disp (Image attrs@(ident, cls, values) [] (url, tit)) = do
  rendered <- liftIO imageVideoTag
  return $ rendered
  where
    imageVideoTag =
      if (uriPathExtension url) `elem` [".svg"] && "embed" `elem` cls
        then do
          fileContent <- catch (readFile url) svgLoadErrorHandler
          return $
            if attrs /= nullAttr
              then Span (ident, cls', values') [toHtml fileContent]
              else toHtml fileContent
        else do
          return $
            toHtml $
            renderHtml $
            if "iframe" `elem` cls
              then mediaTag (iframe "Browser does not support iframe.")
              else createMediaTag
    createMediaTag =
      case classifyFilePath url of
        VideoMedia -> mediaTag (video "Browser does not support video.")
        AudioMedia -> mediaTag (audio "Browser does not support audio.")
        IframeMedia -> mediaTag (iframe "Browser does not support iframe.")
        MeshMedia -> mediaTag (iframe "Browser does not support iframe.")
        ImageMedia -> mediaTag img
    appendAttr element (key, value) =
      element ! customAttribute (stringTag key) (toValue value)
    mediaTag tag =
      ifNotEmpty A.id ident $
      ifNotEmpty class_ (unwords cls') $
      ifNotEmpty title tit $ foldl appendAttr tag ((srcAttr, src) : values')
    ifNotEmpty attr value element =
      if value == ""
        then element
        else element ! attr (toValue value)
    srcAttr =
      if disp == Disposition Deck Html
        then "data-src"
        else "src"
    src =
      if extension `elem` meshExtensions
        then "demos" </> "mview" </> "mview.html?model=.." </> ".." </> url
        else url
    extension = uriPathExtension url
    (_, cls', values') =
      if (uriPathExtension url) `elem` [".svg"] && "embed" `elem` cls
        then convertMediaAttributes
               (ident, cls, ("style", "display:inline-block;") : values)
        else convertMediaAttributes attrs
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
renderMediaTag _ inline = do
  return inline

svgLoadErrorHandler :: IOException -> IO String
svgLoadErrorHandler e = do
  return "<div>Couldn't load SVG</div>"

-- | Converts attributes 
convertMediaAttributes :: Attr -> Attr
convertMediaAttributes attrs =
  convertMediaAttributeGatherStyle $
  convertMediaAttributeImageSize $ convertMediaAttributeAutoplay attrs

convertMediaAttributeAutoplay :: Attr -> Attr
convertMediaAttributeAutoplay (id, cls, vals) = (id, cls', vals')
  where
    (autoplay_cls, cls') = partition (== "autoplay") cls
    vals' =
      vals ++
      if null autoplay_cls
        then []
        else [("data-autoplay", "true")]

convertMediaAttributeControls :: Attr -> Attr
convertMediaAttributeControls (id, cls, vals) = (id, cls', vals')
  where
    (autoplay_cls, cls') = partition (== "controls") cls
    vals' =
      vals ++
      if null autoplay_cls
        then []
        else [("controls", "true")]

convertMediaAttributeGatherStyle :: Attr -> Attr
convertMediaAttributeGatherStyle (id, cls, vals) = (id, cls, vals')
  where
    (style_cls, cls') = partition (\x -> (fst x) == "style") vals
    style_combined =
      if null style_cls
        then []
        else [("style", intercalate "" $ map snd style_cls)]
    vals' = style_combined ++ cls'

convertMediaAttributeImageSize :: Attr -> Attr
convertMediaAttributeImageSize (id, cls, vals) = (id, cls, vals_processed)
  where
    (height, vals') = partition (\x -> (fst x) == "height") vals
    height_attr =
      if null height
        then []
        else [("style", "height:" ++ (snd (height !! 0)) ++ ";")]
    (width, vals'') = partition (\x -> (fst x) == "width") vals'
    width_attr =
      if null width
        then []
        else [("style", "width:" ++ (snd (width !! 0)) ++ ";")]
    vals_processed = vals'' ++ height_attr ++ width_attr

-- | small wrapper around @RawInline (Format "html")@
--   as this is less line-noise in the filters and the
--   intent is more clear.
toHtml :: String -> Inline
toHtml = RawInline (Format "html")

extractFigures :: Pandoc -> Decker Pandoc
extractFigures pandoc = do
  return $ walk extractFigure pandoc

extractFigure :: Block -> Block
extractFigure (Para content) =
  case content of
    [Span attr inner@((RawInline (Format "html") "<figure>"):otherContent)] ->
      Div attr [Plain inner]
    a -> Para a
extractFigure b = b
