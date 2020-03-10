{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the new Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker where

import Text.Decker.Internal.Meta
import Text.Decker.Project.Project

import Control.Monad.Catch
import Data.Digest.Pure.MD5
import qualified Data.List as List
import qualified Data.Text as Text
import Relude
import System.Directory
import System.FilePath
import Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable)
import Text.Pandoc
import Text.Pandoc.Walk
import Text.URI (URI)
import qualified Text.URI as URI

-- | WriterOptions and the document meta data are available to all
-- filters. 
data FilterState = FilterState
  { options :: WriterOptions
  , meta :: Meta
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

-- | An associative list representing element attributes.
type AttrMap = [(Text, Text)]

-- | The first set contains the transformed attributes that will be extracted
-- and added to the HTML elements. The second set contains the source
-- attributes as parsed from the Markdown attribute markup. Many functions
-- inside the Attrib monad manipulate one or both attribute sets. 
type AttribState = (Attr, Attr)

-- | The Attrib monad.
type Attrib = StateT AttribState Filter

-- | Extracts the attributes that have been transformed so far. The attributes
-- are removed.
extractAttr :: Attrib Attr
extractAttr = do
  (result, remaining) <- get
  put (nullAttr, remaining)
  return result

-- | Removes all values associated with key from the list.
rmKey :: Eq a => a -> [(a, b)] -> [(a, b)]
rmKey key = filter ((/= key) . fst)

-- | Removes all instaces of value from the list.
rmClass :: Text -> [Text] -> [Text]
rmClass cls = filter (/= cls)

-- | Alters the value at key. Can be used to add, change, or remove key value
-- pairs from an associative list. (See Data.Map.Strict.alter).
alterKey :: Eq a => (Maybe b -> Maybe b) -> a -> [(a, b)] -> [(a, b)]
alterKey f key kvs =
  case f $ List.lookup key kvs of
    Just value -> (key, value) : rmKey key kvs
    Nothing -> rmKey key kvs

-- | Adds a CSS style value pair to the attribute map. If a style attribute
-- does not yet exist, it is created. The value of an existing style attribute 
-- is ammended on the left. 
addStyle :: (Text, Text) -> AttrMap -> AttrMap
addStyle (k, v) = alterKey addOne "style"
  where
    addOne style = Just $ fromMaybe "" style <> k <> ":" <> v <> ";"

-- | Adds a CSS style value pair to the target attributes.
injectStyle :: (Text, Text) -> Attrib ()
injectStyle (key, value) = modify transform
  where
    transform ((id', cs', kvs'), attr) =
      ((id', cs', addStyle (key, value) kvs'), attr)

-- | Pushes an additional attribute to the source side of the attribute state.
-- An existing attribute with the same key is overwritten.
pushAttribute :: (Text, Text) -> Attrib ()
pushAttribute (key, value) = modify transform
  where
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, alterKey replace key kvs))
    replace _ = Just value

-- | Removea the attribute key from the source attribute map and adds it as a
-- CSS style value to the target style attribute. Mainly used to translate
-- witdth an height attributes into CSS style setting.
takeStyle :: Text -> Attrib ()
takeStyle key = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      case List.lookup key kvs of
        Just value ->
          ((id', cs', addStyle (key, value) kvs'), (id, cs, rmKey key kvs))
        Nothing -> state

-- | Translates width and height attributes into CSS style values if they
-- exist.
takeSize :: Attrib ()
takeSize = do
  takeStyle "width"
  takeStyle "height"

takeId :: Attrib ()
takeId = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id, cs', kvs'), ("", cs, kvs))

takeCss :: Attrib ()
takeCss = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (css, rest) = List.partition (isCss . fst) kvs
          added =
            foldl'
              (\result (k, v) -> addStyle (Text.drop 4 k, v) result)
              kvs'
              css
       in ((id', cs', added), (id, cs, rest))
    isCss key = Text.isPrefixOf "css:" key && Text.length key > 4

coreAttribs :: [Text]
coreAttribs = ["id", "class", "title", "style"]

dropCore :: Attrib ()
dropCore = modify transform
  where
    transform (attr', (id, cs, kvs)) =
      (attr', (id, cs, filter ((`notElem` coreAttribs) . fst) kvs))

passI18n :: Attrib ()
passI18n = modify transform
  where
    transform ((id', cs', kvs'), (id, cs, kvs)) =
      let (pass, rest) = List.partition ((`elem` ["dir", "xml:lang"]) . fst) kvs
       in ((id', cs', kvs' <> pass), (id, cs, rest))

takeData :: Attrib ()
takeData = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs', map (first ("data-" <>)) kvs <> kvs'), (id, cs, []))

takeAllClasses :: Attrib ()
takeAllClasses = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs <> cs', kvs'), (id, [], kvs))

injectBorder = do
  border <- getMetaBoolOrElse "decker.filter.border" False <$> lift (gets meta)
  when border $ injectStyle ("border", "2px solid magenta")

videoClasses = ["autoplay", "controls", "loop", "muted"]

takeVideoClasses :: Attrib ()
takeVideoClasses = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vcs, rest) = List.partition (`elem` videoClasses) cs
       in ((id', cs', map (, "1") vcs <> kvs'), (id, rest, kvs))

videoAttribs = ["poster", "preload"]

passVideoAttribs :: Attrib ()
passVideoAttribs = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vkvs, rest) = List.partition ((`elem` videoAttribs) . fst) kvs
       in ((id', cs', vkvs <> kvs'), (id, cs, rest))

-- | Applies a filter to each pair of successive elements in a list. The filter
-- may consume the elements and return a list of transformed elements, or it
-- may reject the pair and return nothing.
pairwise :: ((a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
pairwise f (x:y:zs) = do
  match <- f (x, y)
  case match of
    Just rs -> (rs ++) <$> pairwise f zs
    Nothing -> (x :) <$> pairwise f (y : zs)
pairwise _ xs = return xs

-- | Applies a filter to each triplet of successive elements in a list.
-- The filter may consume the elements and return a list of transformed elements,
-- or it may reject the triplet and return nothing.
tripletwise :: ((a, a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
tripletwise f (w:x:y:zs) = do
  match <- f (w, x, y)
  case match of
    Just rs -> (rs ++) <$> tripletwise f zs
    Nothing -> (w :) <$> tripletwise f (x : y : zs)
tripletwise _ xs = return xs

-- | Runs the document through the four increaingly detailed filter stages. The
-- matching granularity ranges from list of blocks to single inline elements.
mediaFilter :: WriterOptions -> Pandoc -> IO Pandoc
mediaFilter options pandoc =
  runFilter options mediaBlockListFilter pandoc >>=
  runFilter options mediaInlineListFilter >>=
  runFilter options mediaBlockFilter >>=
  runFilter options mediaInlineFilter

captionLabel = "Caption:"

-- | Filters lists of Blocks that can match in pairs or triplets. 
--
-- For example: Match a paragraph containing just an image followed by a
-- paragraph starting with the string "Image: " as an image that is to be
-- placed in a figure block with a caption.
mediaBlockListFilter :: [Block] -> Filter [Block]
mediaBlockListFilter blocks =
  tripletwise filterTriplets blocks >>= pairwise filterPairs
  where
    filterPairs :: (Block, Block) -> Filter (Maybe [Block])
    -- An image followed by an explicit caption paragraph.
    filterPairs ((Para [image@Image {}]), Para (Str captionLabel:caption)) =
      Just <$> (transformImage image caption >>= renderHtml)
    -- Any number of consecutive images in a masonry row.
    filterPairs (LineBlock lines, Para (Str captionLabel:caption))
      | oneImagePerLine lines =
        Just <$> (transformImages (concat lines) caption >>= renderHtml)
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Block, Block, Block) -> Filter (Maybe [Block])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Filters lists of Inlines that can match in pairs or triplets
mediaInlineListFilter :: [Inline] -> Filter [Inline]
mediaInlineListFilter inlines =
  tripletwise filterTriplets inlines >>= pairwise filterPairs
  where
    filterPairs :: (Inline, Inline) -> Filter (Maybe [Inline])
    filterPairs (img1@Image {}, img2@Image {}) =
      Just <$> (transformImages [img1, img2] [] >>= renderHtml)
      -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Inline, Inline, Inline) -> Filter (Maybe [Inline])
    filterTriplets (img1@Image {}, img2@Image {}, img3@Image {}) =
      Just <$> (transformImages [img1, img2, img3] [] >>= renderHtml)
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
-- A solitary image in a paragraph with a possible caption.
mediaBlockFilter (Para [image@(Image _ caption _)]) =
  transformImage image caption >>= renderHtml
-- Any number of consecutive images in a masonry row.
mediaBlockFilter (LineBlock lines)
  | oneImagePerLine lines = transformImages (concat lines) [] >>= renderHtml
-- Default filter
mediaBlockFilter block = return block

oneImagePerLine :: [[Inline]] -> Bool
oneImagePerLine inlines = all isImage $ concat inlines

isImage Image {} = True
isImage _ = False

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
-- An inline image with a possible caption.
mediaInlineFilter image@(Image _ caption _) =
  transformImage image caption >>= renderHtml
-- Default filter
mediaInlineFilter inline = return inline

-- | Runs a filter on a Pandoc document. The options are used to rewrite document
-- fragments to HTML or back to Markdown. The meta data may be transformed by
-- the filter. The filter runs in the Filter monad and has access to options
-- and meta data via `gets` and `puts`.
runFilter ::
     Walkable a Pandoc
  => WriterOptions
  -> (a -> Filter a)
  -> Pandoc
  -> IO Pandoc
runFilter options filter pandoc@(Pandoc meta _) = do
  (Pandoc _ blocks, FilterState _ meta) <-
    runStateT (walkM filter pandoc) (FilterState options meta)
  return $ Pandoc meta blocks

-- Runs a filter on any Walkable structure. Does not carry transformed meta
-- data over if chained. Mainly for writing tests.
runFilter' ::
     Walkable a b => WriterOptions -> Meta -> (a -> Filter a) -> b -> IO b
runFilter' options meta filter x =
  evalStateT (walkM filter x) (FilterState options meta)

-- | File-extensions that should be treated as image
imageExt = ["jpg", "jpeg", "png", "gif", "tif", "tiff", "bmp", "svg"]

videoExt = ["mp4", "mov", "ogg", "avi"]

svgExt = ["svg"]

iframeExt = ["html", "html"]

mviewExt = ["off"]

-- | Renders a list of inlines back to markdown.
toMarkdown :: [Inline] -> Filter Text
toMarkdown [] = return ""
toMarkdown inlines = do
  FilterState options meta <- get
  case runPure (writeHtml5String options (Pandoc meta [Plain inlines])) of
    Right text -> return text
    Left err -> bug $ PandocException $ "BUG: " <> show err

-- | Renders a list of inlines to HTML.
inlinesToHtml :: [Inline] -> Filter Html
inlinesToHtml [] = return $ H.span ""
inlinesToHtml inlines = do
  FilterState options meta <- get
  case runPure (writeHtml5 options (Pandoc meta [Plain inlines])) of
    Right html -> return html
    Left err -> bug $ PandocException $ "BUG: " <> show err

class RawHtml a where
  rawHtml :: Text -> a

instance RawHtml Inline where
  rawHtml = RawInline (Format "html5")

instance RawHtml [Inline] where
  rawHtml text = [RawInline (Format "html5") text]

instance RawHtml Block where
  rawHtml = RawBlock (Format "html5")

instance RawHtml [Block] where
  rawHtml text = [RawBlock (Format "html5") text]

data MediaType
  = ImageT
  | VideoT
  | AudioT
  | IframeT
  | CodeT
  | PdfT
  | EmbedSvgT
  | OffT
  | RenderT

extIn :: Maybe Text -> [Text] -> Bool
extIn (Just ext) list = ext `elem` list
extIn Nothing _ = False

renderHtml :: RawHtml a => Html -> Filter a
renderHtml html = do
  pretty <- getMetaBoolOrElse "decker.filter.pretty" False <$> gets meta
  return $
    rawHtml $
    toText $
    if pretty
      then toText $ Pretty.renderHtml html
      else fromLazy $ Text.renderHtml html

runAttr :: Attr -> Attrib a -> Filter a
runAttr attr attrAction = evalStateT attrAction (nullAttr, attr)

-- | Transforms a URL and handles local and remote URLs differently.
transformUrl :: Text -> Filter URI
transformUrl url = do
  uri <- URI.mkURI url
  case URI.uriScheme uri of
    Just rtext
      | URI.unRText rtext /= "file" -> processRemoteUri uri
    _ -> processLocalUri uri

-- | Adds a remote URL to the `decker.filter.links` list in the meta data.
processRemoteUri :: URI -> Filter URI
processRemoteUri uri = do
  modifyMeta (addStringToMetaList "decker.filter.links" (URI.render uri))
  return uri

-- | Applies the modification function f to the meta data in the filter
-- state.
modifyMeta :: (Meta -> Meta) -> Filter ()
modifyMeta f = modify (\s -> s {meta = f (meta s)})

processLocalUri :: URI -> Filter URI
processLocalUri uri
  -- | The absolute (!) document directory from which this is called.
 = do
  cwd <- liftIO $ getCurrentDirectory
  baseDir <- toString <$> getMeta "decker.base-dir" "."
  -- | The absolute (!) project directory from which this is called.
  projectDir <- toString <$> getMeta "decker.project-dir" (toText cwd)
  -- | The absolute (!) public directory where everything is published to.
  publicDir <-
    toString <$> getMeta "decker.public-dir" (toText $ cwd </> "public")
  -- | The path component from the URI
  urlPath <- toString <$> uriPath uri
  -- | Interpret urlPath either project relative or document relative,
  -- depending on the leading slash.
  let relPath =
        normalise $
        if hasDrive urlPath
          then dropDrive urlPath
          else makeRelative projectDir baseDir </> urlPath
  let basePath = projectDir </> baseDir
  let targetPath = publicDir </> relPath
  let sourcePath = projectDir </> relPath
  let publicRelPath = makeRelativeTo basePath sourcePath
  publicUri <- setUriPath (toText publicRelPath) uri
  let publicUrl = URI.render publicUri
  storeResourceInfo sourcePath targetPath publicUrl
  return publicUri

setMeta :: Text -> Text -> Filter ()
setMeta key value =
  modify (\s -> s {meta = setMetaValue key (MetaString value) (meta s)})

getMeta :: Text -> Text -> Filter Text
getMeta key def = getMetaTextOrElse key def <$> gets meta

storeResourceInfo :: FilePath -> FilePath -> Text -> Filter ()
storeResourceInfo source target url = do
  let key = "decker" <.> "filter" <.> "resources" <.> hash9String target
  setMeta (toText $ key <.> "source") $ toText source
  setMeta (toText $ key <.> "target") $ toText target
  setMeta (toText $ key <.> "url") url

transformImage :: Inline -> [Inline] -> Filter Html
transformImage (Image attr@(_, classes, _) _ (url, _)) caption = do
  uri <- transformUrl url
  let ext = uriPathExtension uri
  runAttr attr $
    if | extIn ext svgExt && "embed" `elem` classes -> svgHtml uri caption
       | extIn ext mviewExt || "mview" `elem` classes -> mviewHtml uri caption
       | extIn ext iframeExt || "iframe" `elem` classes ->
         iframeHtml uri caption
       | extIn ext imageExt || "image" `elem` classes -> imageHtml uri caption
       | extIn ext videoExt || "video" `elem` classes -> videoHtml uri caption
       | otherwise -> imageHtml uri caption
transformImage inline _ =
  bug $ InternalException ("transformImage: no match for: " <> show inline)

-- Lines up a list of images in a div element. Use with flexbox css.
transformImages :: [Inline] -> [Inline] -> Filter Html
transformImages images caption = do
  imageRow <-
    mapM
      (\img@(Image _ caption _) -> H.div <$> transformImage img caption)
      images
  if null caption
    then return $
         H.div ! A.class_ "image-row" ! A.style "border:2px solid cyan;" $
         toHtml imageRow
    else do
      captionHtml <- inlinesToHtml caption
      return $
        H.figure ! A.style "border:2px solid cyan;" $ do
          H.div ! A.class_ "image-row" $ toHtml imageRow
          H.figcaption captionHtml

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) = foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (H.toValue v))

mkVideoTag :: Text -> Attr -> Html
mkVideoTag url (id, cs, kvs) =
  H.video !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
  H.dataAttribute "src" (H.preEscapedToValue url) !*
  kvs $
  ""

mkIframeTag :: Text -> Attr -> Html
mkIframeTag url (id, cs, kvs) =
  H.iframe !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
  H.customAttribute "allow" "fullscreen" !
  H.dataAttribute "src" (H.preEscapedToValue url) !*
  kvs $
  ""

mkImageTag :: Text -> Attr -> Html
mkImageTag url (id, cs, kvs) =
  H.img !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
  H.dataAttribute "src" (H.preEscapedToValue url) !*
  kvs

mkFigureTag :: Html -> Html -> Attr -> Html
mkFigureTag content caption (id, cs, kvs) =
  H.figure !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !*
  kvs $ do
    content
    H.figcaption ! A.class_ "decker" $ caption

takeUsual = do
  takeId
  takeAllClasses
  takeSize
  takeCss
  dropCore
  passI18n
  takeData

imageHtml :: URI -> [Inline] -> Attrib Html
imageHtml uri caption =
  case caption of
    [] -> do
      injectBorder >> takeUsual
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- injectStyle ("width", "100%") >> extractAttr
      let imageTag = mkImageTag (URI.render uri) imgAttr
      injectBorder >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

svgHtml :: URI -> [Inline] -> Attrib Html
svgHtml uri caption =
  case caption of
    [] -> do
      injectBorder >> takeUsual
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- injectStyle ("width", "100%") >> extractAttr
      let imageTag = mkImageTag (URI.render uri) imgAttr
      injectBorder >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

mviewHtml :: URI -> [Inline] -> Attrib Html
mviewHtml uri caption = do
  let model = URI.render uri
  pushAttribute ("model", model)
  mviewUri <- URI.mkURI "/support/vendor/mview/mview.html"
  iframeHtml mviewUri caption

iframeHtml :: URI -> [Inline] -> Attrib Html
iframeHtml uri caption =
  case caption of
    [] -> do
      iframeAttr <- injectBorder >> takeUsual >> extractAttr
      return $ mkIframeTag (URI.render uri) iframeAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      figureAttr <-
        injectBorder >> takeId >> takeAllClasses >> takeCss >> dropCore >>
        passI18n >>
        extractAttr
      iframeAttr <-
        injectStyle ("width", "100%") >> takeSize >> takeData >> extractAttr
      let iframeTag = mkIframeTag (URI.render uri) iframeAttr
      return $ mkFigureTag iframeTag captionHtml figureAttr

mediaFragment :: Attrib Text
mediaFragment = do
  (result, (id, cs, kvs)) <- get
  let start = fromMaybe "" $ List.lookup "start" kvs
      stop = fromMaybe "" $ List.lookup "stop" kvs
  put (result, (id, cs, rmKey "start" $ rmKey "stop" kvs))
  return $
    if Text.null start && Text.null stop
      then ""
      else "t=" <> start <> "," <> stop

videoHtml :: URI -> [Inline] -> Attrib Html
videoHtml uri caption = do
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  case caption of
    [] -> do
      injectBorder >> takeVideoClasses >> passVideoAttribs >> takeUsual
      mkVideoTag videoUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      videoAttr <-
        takeVideoClasses >> passVideoAttribs >> injectStyle ("width", "100%") >>
        extractAttr
      let videoTag = mkVideoTag videoUri videoAttr
      figureAttr <- injectBorder >> takeUsual >> extractAttr
      return $ mkFigureTag videoTag captionHtml figureAttr

uriPathExtension :: URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing

uriPath :: MonadThrow m => URI -> m Text
uriPath uri =
  return $
  URI.render
    URI.emptyURI
      { URI.uriPath = URI.uriPath uri
      , URI.uriAuthority = Left (URI.isPathAbsolute uri)
      }

setUriPath :: MonadThrow m => Text -> URI -> m URI
setUriPath path uri = do
  pathUri <- URI.mkURI path
  return
    uri
      { URI.uriPath = URI.uriPath pathUri
      , URI.uriAuthority =
          case URI.uriAuthority uri of
            Left _ -> Left $ URI.isPathAbsolute pathUri
            auth -> auth
      }

hash9String :: String -> String
hash9String text = take 9 $ show $ md5 $ encodeUtf8 text

hash9 :: Text -> Text
hash9 text = Text.pack $ take 9 $ show $ md5 $ encodeUtf8 text
