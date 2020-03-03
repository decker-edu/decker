{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker where

import Text.Decker.Internal.Meta

import Control.Monad.Loops
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Relude
import Text.Blaze.Html
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty
import qualified Text.Blaze.Html.Renderer.Text as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (Attributable)
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

-- import Text.URI.Lens
-- | Some WriterOptions and the document meta data are available to all
-- filters. 
data FilterState = FilterState
  { options :: WriterOptions
  , meta :: Meta
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

type FilterFunc a = a -> Filter (Maybe a)

type InlineFilter = FilterFunc Inline

type BlockFilter = FilterFunc Block

-- | Assemble a Pandoc Attrib inside a filter.
type ClassSet = Set Text

type AttrMap = Map Text Text

type AttrMap' = [(Text, Text)]

type FastAttrib = (Text, ClassSet, AttrMap)

type Attrib = StateT FastAttrib Filter

type AttribState = (Attr, Attr)

type Attrib' = StateT AttribState Filter

extractAttr :: Attrib' Attr
extractAttr = do
  (result, remaining) <- get
  put (nullAttr, remaining)
  return result

remainingAttr :: Attrib' Attr
remainingAttr = snd <$> get

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
    addOne style = Just $ k <> ":" <> v <> ";" <> fromMaybe "" style

assembleAttr :: (Attrib a) -> Filter Attr
assembleAttr action = do
  (id, cls, kvs) <- execStateT action ("", Set.empty, Map.empty)
  return (id, Set.toList cls, Map.toList kvs)

assemble :: [(Attr -> Attrib Attr)] -> Attr -> Filter Attr
assemble actions attr = do
  (id, cls, kvs) <- execStateT (concatM actions attr) ("", Set.empty, Map.empty)
  return (id, Set.toList cls, Map.toList kvs)

assemble' :: [(Attr -> Attrib Attr)] -> Attr -> Filter (Attr, Attr)
assemble' actions attr = do
  (attr', (id, cls, kvs)) <-
    runStateT (concatM actions attr) ("", Set.empty, Map.empty)
  return ((id, Set.toList cls, Map.toList kvs), attr')

rmKey :: Eq a => a -> [(a, b)] -> [(a, b)]
rmKey key = filter ((/= key) . fst)

rmClass :: Text -> [Text] -> [Text]
rmClass cls = filter (/= cls)

alterKey :: Eq a => (Maybe b -> Maybe b) -> a -> [(a, b)] -> [(a, b)]
alterKey f key kvs =
  case f $ List.lookup key kvs of
    Just value -> (key, value) : rmKey key kvs
    Nothing -> rmKey key kvs

addStyle' :: (Text, Text) -> AttrMap' -> AttrMap'
addStyle' (k, v) kvs = alterKey addOne "style" kvs
  where
    addOne style = Just $ k <> ":" <> v <> ";" <> fromMaybe "" style

takeStyle' :: Text -> Attrib' ()
takeStyle' key = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      case List.lookup key kvs of
        Just value ->
          ((id', cs', addStyle' (key, value) kvs'), (id, cs, rmKey key kvs))
        Nothing -> state

takeStyle :: Text -> Attr -> Attrib Attr
takeStyle key (id, cs, kvs) =
  case List.lookup key kvs of
    Just value -> do
      addStyle (key, value)
      return (id, cs, rmKey key kvs)
    Nothing -> return (id, cs, kvs)

takeSize' :: Attrib' ()
takeSize' = do
  takeStyle' "width"
  takeStyle' "height"

takeSize :: Attr -> Attrib Attr
takeSize attr = takeStyle "width" attr >>= takeStyle "height"

takeId' :: Attrib' ()
takeId' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id, cs', kvs'), ("", cs, kvs))

takeId :: Attr -> Attrib Attr
takeId (id, cls, kvs) = do
  setId id
  return ("", cls, kvs)

takeCss' :: Attrib' ()
takeCss' = do
    modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (css, rest) = List.partition (isCss . fst) kvs
          added =
            foldl'
              (\result (k, v) -> addStyle' (Text.drop 4 k, v) result)
              kvs'
              css
       in ((id', cs', added), (id, cs, rest))
    isCss key = Text.isPrefixOf "css:" key && Text.length key > 4

takeCss :: Attr -> Attrib Attr
takeCss (id, cls, kvs) = do
  let (css, rest) = List.partition (isCss . fst) kvs
  mapM_ (\(k, v) -> addStyle (Text.drop 4 k, v)) css
  return ("", cls, rest)
  where
    isCss key = Text.isPrefixOf "css:" key && Text.length key > 4

takeData' :: Attrib' ()
takeData' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs', map (first ("data-" <>)) kvs <> kvs'), (id, cs, []))

takeData :: Attr -> Attrib Attr
takeData (id, cls, kvs) = do
  addKeyValues $ map (first ("data-" <>)) kvs
  return ("", cls, [])

takeAllClasses' :: Attrib' ()
takeAllClasses' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      ((id', cs <> cs', kvs'), (id, [], kvs))

takeAllClasses :: Attr -> Attrib Attr
takeAllClasses (id, cls, kvs) = do
  addClasses cls
  return (id, [], kvs)

videoClasses = ["autoplay", "controls", "loop", "muted"]

takeVideoClasses' :: Attrib' ()
takeVideoClasses' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vcs, rest) = List.partition (`elem` videoClasses) cs
       in ((id', cs', map (, "1") vcs <> kvs'), (id, rest, kvs))

takeVideoClasses :: Attr -> Attrib Attr
takeVideoClasses (id, cls, kvs) = do
  mapM_ (addKeyValue . (, "1")) $ filter (`elem` videoClasses) cls
  return (id, filter (`notElem` videoClasses) cls, kvs)

videoAttribs = ["poster", "preload"]

takeVideoAttribs' :: Attrib' ()
takeVideoAttribs' = modify transform
  where
    transform state@((id', cs', kvs'), (id, cs, kvs)) =
      let (vkvs, rest) = List.partition ((`elem` videoAttribs) . fst) kvs
       in ((id', cs', vkvs <> kvs'), (id, cs, rest))

takeVideoAttribs :: Attr -> Attrib Attr
takeVideoAttribs (id, cls, kvs) = do
  let (va, oa) = List.partition ((`elem` videoAttribs) . fst) kvs
  addKeyValues va
  return (id, cls, oa)

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
    Nothing -> (w :) <$> tripletwise f (x : y : zs)
tripletwise _ xs = return xs

-- | Runs the document through the four increaingly detailed filter stages. The
-- matchng granularity ranges from list of blocks to single inline elements.
mediaFilter :: WriterOptions -> Pandoc -> IO Pandoc
mediaFilter options pandoc =
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
mediaBlockListFilter blocks = pairwise filterPairs blocks
  --tripletwise filterTriplets blocks >>= pairwise filterPairs
  where
    filterPairs :: (Block, Block) -> Filter (Maybe [Block])
    -- An image followed by an explicit caption paragraph.
    filterPairs ((Para [image@Image {}]), Para (Str "Image:":caption)) =
      Just <$> transformImage image caption
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Block, Block, Block) -> Filter (Maybe [Block])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Filters lists of Inlines that can match in pairs or triplets
mediaInlineListFilter :: [Inline] -> Filter [Inline]
mediaInlineListFilter inlines = pairwise filterPairs inlines
  --tripletwise filterTriplets inlines >>= pairwise filterPairs
  where
    filterPairs :: (Inline, Inline) -> Filter (Maybe [Inline])
    -- Default filter
    filterPairs (x, y) = return Nothing
    filterTriplets :: (Inline, Inline, Inline) -> Filter (Maybe [Inline])
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
-- A solitary image in a paragraph with a possible caption.
mediaBlockFilter (Para [image@(Image _ caption _)]) = do
  transformImage image caption
-- Default filter
mediaBlockFilter block = return block

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
-- An inline image with a possible caption.
mediaInlineFilter image@(Image _ caption _) = do
  transformImage image caption
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
    do runStateT (walkM filter pandoc) (FilterState options meta)
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

runAttr :: Attr -> Attrib' a -> Filter a
runAttr attr attrAction = evalStateT attrAction (nullAttr, attr)

transformImage :: RawHtml a => Inline -> [Inline] -> Filter a
transformImage (Image attr@(_, classes, _) _ (url, _)) caption = do
  uri <- URI.mkURI url
  let ext = uriPathExtension uri
  html <-
    runAttr attr $
    if | extIn ext imageExt || "image" `elem` classes ->
         imageHtml' uri caption
       | extIn ext videoExt || "video" `elem` classes ->
         videoHtml' uri caption
       | otherwise -> imageHtml' uri caption
  renderHtml html
transformImage inline _ =
  bug $ InternalException ("transformImage: no match for: " <> show inline)

instance ToValue [Text] where
  toValue ts = toValue $ Text.intercalate " " ts

(!*) :: Attributable h => h -> [(Text, Text)] -> h
(!*) html =
  foldl' (\h (k, v) -> h ! customAttribute (H.textTag k) (H.toValue v)) html

mkVideoTag :: Text -> Attr -> Html
mkVideoTag url (id, cs, kvs) =
  H.video !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
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

takeUsual' = do
  print "-------------"
  print =<< get
  takeId'
  print =<< get
  takeAllClasses'
  print =<< get
  takeSize'
  print =<< get
  takeCss'
  print =<< get
  takeData'
  print =<< get

imageHtml' :: URI.URI -> [Inline] -> Attrib' Html
imageHtml' uri caption =
  case caption of
    [] -> do
      takeUsual'
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      let imageTag = mkImageTag (URI.render uri) nullAttr
      takeUsual'
      mkFigureTag imageTag captionHtml <$> extractAttr

imageHtml :: URI.URI -> Attr -> [Inline] -> Filter Html
imageHtml uri attr caption =
  case caption of
    [] -> do
      attr <-
        assemble [takeId, takeAllClasses, takeSize, takeCss, takeData] attr
      return $ mkImageTag (URI.render uri) attr
    caption -> do
      captionHtml <- inlinesToHtml caption
      figureAttr <-
        assemble [takeId, takeAllClasses, takeSize, takeCss, takeData] attr
      return $
        mkFigureTag
          (mkImageTag (URI.render uri) nullAttr)
          captionHtml
          figureAttr

mediaFragment' :: Attrib' Text
mediaFragment' = do
  (result, (id, cs, kvs)) <- get
  let start = fromMaybe "" $ List.lookup "start" kvs
      stop = fromMaybe "" $ List.lookup "stop" kvs
  put (result, (id, cs, rmKey "start" $ rmKey "stop" kvs))
  return $
    if Text.null start && Text.null stop
      then ""
      else ("t=" <> start <> "," <> stop)

mediaFragment :: Attr -> (Text, Attr)
mediaFragment attr@(id, cs, kvs) =
  let start = fromMaybe "" $ List.lookup "start" kvs
      stop = fromMaybe "" $ List.lookup "stop" kvs
   in if Text.null start && Text.null stop
        then ("", attr)
        else ( "t=" <> start <> "," <> stop
             , (id, cs, rmKey "start" $ rmKey "stop" kvs))

videoHtml' :: URI.URI -> [Inline] -> Attrib' Html
videoHtml' uri caption = do
  mediaFrag <- mediaFragment'
  let videoUri = (URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag})
  case caption of
    [] -> do
      takeVideoClasses' >> takeVideoAttribs' >> takeUsual'
      mkVideoTag videoUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      videoAttr <- takeVideoClasses' >> takeVideoAttribs' >> extractAttr
      let videoTag = mkVideoTag videoUri videoAttr
      figureAttr <- takeUsual' >> extractAttr
      return $ mkFigureTag videoTag captionHtml figureAttr

videoHtml :: URI.URI -> Attr -> [Inline] -> Filter Html
videoHtml uri imgAttr@(id, classes, values) caption = do
  let (mediaFrag, attr) = mediaFragment imgAttr
      videoUri = (URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag})
  case caption of
    [] -> do
      videoAttr <-
        assemble
          [ takeId
          , takeVideoClasses
          , takeVideoAttribs
          , takeAllClasses
          , takeSize
          , takeCss
          , takeData
          ]
          attr
      return $ mkVideoTag videoUri videoAttr
    caption -> do
      captionHtml <- inlinesToHtml caption
      (videoAttr, remaining) <-
        assemble' [takeVideoClasses, takeVideoAttribs] attr
      figureAttr <-
        assemble [takeId, takeAllClasses, takeSize, takeCss, takeData] remaining
      return $
        mkFigureTag (mkVideoTag videoUri videoAttr) captionHtml figureAttr

uriPathExtension :: URI.URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ Text.splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing
