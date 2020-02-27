{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker where

import Control.Monad.Loops
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable)
import Text.Decker.Internal.Exception
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

-- import Text.URI.Lens
-- | Some WriterOptions and the document meta data are available to all
-- filters. 
data FilterState =
  FilterState WriterOptions
              Meta

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

type FilterFunc a = a -> Filter (Maybe a)

type InlineFilter = FilterFunc Inline

type BlockFilter = FilterFunc Block

-- | Assemble a Pandoc Attrib inside a filter.
type ClassSet = Set Text

type AttrMap = Map Text Text

type Attrib = StateT (Text, ClassSet, AttrMap) Filter

setId :: Text -> Attrib ()
setId newId = modify (\(id, cs, kvs) -> (newId, cs, kvs))

addClass :: Text -> Attrib ()
addClass cls = modify (\(id, cs, kvs) -> (id, Set.insert cls cs, kvs))

addClasses :: [Text] -> Attrib ()
addClasses cls =
  modify (\(id, cs, kvs) -> (id, Set.union (Set.fromList cls) cs, kvs))

addKeyValue :: (Text, Text) -> Attrib ()
addKeyValue (k, v) = modify (\(id, cs, kvs) -> (id, cs, Map.insert k v kvs))

addKeyValues :: [(Text, Text)] -> Attrib ()
addKeyValues kvl =
  modify (\(id, cs, kvs) -> (id, cs, Map.union kvs (Map.fromList kvl)))

addStyle :: (Text, Text) -> Attrib ()
addStyle (k, v) =
  modify (\(id, cls, kvs) -> (id, cls, Map.alter addOne "style" kvs))
  where
    addOne style = Just $ k <> ":" <> v <> ";" <> (fromMaybe "" style)

assembleAttr :: (Attrib a) -> Filter Attr
assembleAttr action = do
  (id, cls, kvs) <- execStateT action ("", Set.empty, Map.empty)
  return (id, Set.toList cls, Map.toList kvs)

assemble :: [(Attr -> Attrib Attr)] -> Attr -> Filter Attr
assemble actions attr = do
  (id, cls, kvs) <-
    execStateT ((concatM actions) attr) ("", Set.empty, Map.empty)
  return (id, Set.toList cls, Map.toList kvs)

rmKey :: Text -> [(Text, Text)] -> [(Text, Text)]
rmKey key = filter ((/= key) . fst)

rmClass :: Text -> [Text] -> [Text]
rmClass cls = filter ((/= cls))

takeStyle :: Text -> Attr -> Attrib Attr
takeStyle key (id, cs, kvs) =
  case List.lookup key kvs of
    Just value -> do
      addStyle (key, value)
      return (id, cs, rmKey key kvs)
    Nothing -> return (id, cs, kvs)

takeSize :: Attr -> Attrib Attr
takeSize attr = takeStyle "width" attr >>= takeStyle "height"

takeId :: Attr -> Attrib Attr
takeId (id, cls, kvs) = do
  setId id
  return ("", cls, kvs)

takeCss :: Attr -> Attrib Attr
takeCss (id, cls, kvs) = do
  let (css, rest) = List.partition (isCss . fst) kvs
  mapM_ (\(k, v) -> addStyle ((Text.drop 4 k), v)) css
  return ("", cls, rest)
  where
    isCss key = Text.isPrefixOf "css:" key && Text.length key > 4

takeData :: Attr -> Attrib Attr
takeData (id, cls, kvs) = do
  addKeyValues $ map (\(k, v) -> ("data-" <> k, v)) kvs
  return ("", cls, [])

takeAllClasses :: Attr -> Attrib Attr
takeAllClasses (id, cls, kvs) = do
  addClasses cls
  return (id, [], kvs)

-- | Apply a filter to each pair of successive elements in a list. The filter
-- may consume the elements and return a list of transformed elements, or it
-- may reject the pair and return nothing.
pairwise :: ((a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
pairwise f (x:y:zs) = do
  match <- f (x, y)
  case match of
    Just rs -> (rs ++) <$> pairwise f zs
    Nothing -> (x :) <$> pairwise f (y : zs)
pairwise _ xs = return xs

-- | Apply a filter to each triplet of successive elements in a list.
-- The filter may consume the elements and return a list of transformed elements,
-- or it may reject the triplet and return nothing.
tripletwise :: ((a, a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
tripletwise f (w:x:y:zs) = do
  match <- f (w, x, y)
  case match of
    Just rs -> (rs ++) <$> tripletwise f zs
    Nothing -> (x :) <$> tripletwise f (y : zs)
tripletwise _ xs = return xs

-- | Runs the document through the four increaingly detailed filter stages. The
-- matchng granularity ranges from list of blocks to single inline elements.
mediaFilter :: WriterOptions -> Pandoc -> IO Pandoc
mediaFilter options pandoc = do
  runFilter options mediaBlockListFilter pandoc >>=
    runFilter options mediaInlineListFilter >>=
    runFilter options mediaBlockFilter >>=
    runFilter options mediaInlineFilter

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
    filterPairs ((Para [image@Image {}]), Para (Str "Image:":caption)) =
      transformImage image caption
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
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Inline, Inline, Inline) -> Filter (Maybe [Inline])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
mediaBlockFilter para@(Para [(Image {})]) = return para
mediaBlockFilter block = return block

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
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

videoExt = [".mp4"]

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

transformImage :: RawHtml a => Inline -> [Inline] -> Filter (Maybe a)
transformImage (Image attr@(id, classes, values) inlines (url, title)) caption = do
  uri <- URI.mkURI url
  let ext = uriPathExtension uri
  html <-
    if | extIn ext imageExt || "image" `elem` classes ->
         imageHtml uri attr caption
       | extIn ext videoExt || "video" `elem` classes ->
         videoHtml uri attr caption
       | otherwise -> imageHtml uri attr caption
  return $ Just $ rawHtml $ fromLazy $ renderHtml html
transformImage inline _ =
  bug $ InternalException ("transformImage: no match for: " <> show inline)

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) html kvs =
  foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (H.toValue v)) html kvs

mkImageTag :: Text -> Attr -> Html
mkImageTag url (id, cs, kvs) =
  H.img !? (not (Text.null id), A.id (H.toValue id)) !?
  (not (null cs), A.class_ (H.toValue cs)) !
  H.dataAttribute "src" (H.toValue url) !*
  kvs

mkFigureTag :: Html -> Html -> Attr -> Html
mkFigureTag content caption (id, cs, kvs) =
  H.figure !? (not (Text.null id), A.id (H.toValue id)) !?
  (not (null cs), A.class_ (H.toValue cs)) !*
  kvs $ do
    content
    H.figcaption caption

imageHtml :: URI.URI -> Attr -> [Inline] -> Filter Html
imageHtml uri attr caption = do
  captionHtml <- inlinesToHtml caption
  case caption of
    [] -> do
      attr <-
        assemble [takeId, takeAllClasses, takeSize, takeCss, takeData] attr
      return $ mkImageTag (URI.render uri) attr
    caption -> do
      figureAttr <-
        assemble [takeId, takeAllClasses, takeSize, takeCss, takeData] attr
      return $
        mkFigureTag
          (mkImageTag (URI.render uri) nullAttr)
          captionHtml
          figureAttr

videoHtml :: URI.URI -> Attr -> [Inline] -> Filter Html
videoHtml uri attr@(id, classes, values) caption = return H.img

uriPathExtension :: URI.URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing
