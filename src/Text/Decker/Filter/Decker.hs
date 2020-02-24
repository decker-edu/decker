{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker where

import Control.Lens
import Data.Text (splitOn)
import Relude
import System.FilePath
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Decker.Internal.Exception
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI
import Text.URI.Lens

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
  fst <$> runStateT (walkM filter x) (FilterState options meta)

-- | File-extensions that should be treated as image
imageExt = ["jpg", "jpeg", "png", "gif", "tif", "tiff", "bmp", "svg"]

videoExt = [".mp4"]

checkExtension extensions path = takeExtension path `elem` extensions

isPlainImage url = checkExtension imageExt $ toString url

filterPlainImage :: Inline -> Filter (Maybe Inline)
filterPlainImage (Image attr [] (url, title))
  | isPlainImage url = return $ Just $ Image attr [] (url, title)
filterPlainImage (Image attr inlines (url, title))
  | isPlainImage url = do
    caption <- toMarkdown inlines
    let html =
          H.figure $ do
            H.img ! A.src (H.toValue url)
            H.figcaption (toHtml caption)
    return $ Just $ RawInline (Format "html5") (fromLazy $ renderHtml html)
filterPlainImage image = return Nothing

-- | Renders a list of inlines back to markdown.
toMarkdown :: [Inline] -> Filter Text
toMarkdown inlines = do
  FilterState options meta <- get
  case runPure (writeHtml5String options (Pandoc meta [Para inlines])) of
    Right text -> return text
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
  innerHtml <-
    if | extIn ext imageExt || "image" `elem` classes -> imageHtml uri attr
       | extIn ext videoExt || "video" `elem` classes -> videoHtml uri attr
       | otherwise -> imageHtml uri attr
  outerHtml <-
    case (inlines, caption) of
      ([], []) -> return innerHtml
      (caption, []) -> return innerHtml
      (_, caption) -> return innerHtml
  return $ Just $ rawHtml "Generated HTML"
transformImage inline _ =
  bug $ InternalException ("transformImage: no match for: " <> show inline)

imageHtml :: URI.URI -> Attr -> Filter Html
imageHtml uri attr@(id, classes, values) = return H.img

videoHtml :: URI.URI -> Attr -> Filter Html
videoHtml uri attr@(id, classes, values) = return H.img

uriPathExtension :: URI.URI -> Maybe Text
uriPathExtension uri =
  case URI.uriPath uri of
    (Just (False, pieces)) ->
      listToMaybe $ reverse $ splitOn "." $ URI.unRText $ last pieces
    _ -> Nothing
