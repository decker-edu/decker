{-# LANGUAGE NoImplicitPrelude #-}

-- | This is the new Decker filter for Pandoc.
--
-- All decker specific meta data is embedded into the document meta data under
-- the `decker` key. Information gathered during the filter run is appended
-- under the `decker` key in the meta data of the resulting document.
module Text.Decker.Filter.Decker where

import Text.Decker.Filter.Attrib
import Text.Decker.Filter.CRC32
import Text.Decker.Filter.Header
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Filter.Streaming
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

import Control.Monad.Catch
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Relude
import System.Directory
import System.FilePath
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc hiding (lookupMeta)
import Text.Pandoc.Walk
import Text.Printf
import Text.URI (URI)
import qualified Text.URI as URI

-- | Applies a filter to each pair of successive elements in a list. The filter
-- may consume the elements and return a list of transformed elements, or it
-- may reject the pair and return Nothing.
pairwise :: ((a, a) -> Filter (Maybe [a])) -> [a] -> Filter [a]
pairwise f (x:y:zs) = do
  match <- f (x, y)
  case match of
    Just rs -> (rs ++) <$> pairwise f zs
    Nothing -> (x :) <$> pairwise f (y : zs)
pairwise _ xs = return xs

-- | Applies a filter to each triplet of successive elements in a list.
-- The filter may consume the elements and return a list of transformed elements,
-- or it may reject the triplet and return Nothing.
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
  runFilter options transformHeader1 pandoc >>=
  runFilter options mediaBlockListFilter >>=
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
    filterPairs ((Para [image@Image {}]), Para (Str "Caption:":caption)) =
      Just . single . Para . single <$> transformImage image caption
    -- An code block followed by an explicit caption paragraph.
    filterPairs (code@CodeBlock {}, Para (Str "Caption:":caption)) =
      Just . single <$> transformCodeBlock code caption
    -- Any number of consecutive images in a masonry row.
    filterPairs (LineBlock lines, Para (Str "Caption:":caption))
      | oneImagePerLine lines =
        Just . single <$> transformImages (concat lines) caption
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
    -- Default filter
    filterTriplets (x, y, z) = return Nothing

-- | Match a single Block element
mediaBlockFilter :: Block -> Filter Block
-- A level one header.
mediaBlockFilter header@(Header 1 attr text) = transformHeader1 header
-- A solitary image in a paragraph with a possible caption.
mediaBlockFilter (Para [image@(Image _ caption _)]) =
  Para . single <$> transformImage image caption
-- A solitary code block in a paragraph with a possible caption.
mediaBlockFilter (code@CodeBlock {}) = transformCodeBlock code []
-- Any number of consecutive images in a masonry row.
mediaBlockFilter (LineBlock lines)
  | oneImagePerLine lines = transformImages (concat lines) []
-- Default filter
mediaBlockFilter block = return block

oneImagePerLine :: [[Inline]] -> Bool
oneImagePerLine inlines = all isImage $ concat inlines

isImage Image {} = True
isImage _ = False

-- | Matches a single Inline element
mediaInlineFilter :: Inline -> Filter Inline
-- An inline image with a possible caption.
mediaInlineFilter image@(Image _ caption _) = transformImage image caption
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

extIn :: Maybe Text -> [Text] -> Bool
extIn (Just ext) list = ext `elem` list
extIn Nothing _ = False

-- | Generates an HTML error message for Image inlines from the image
-- source and the actual exception. The Image element is rendered back
-- to Markdown format and included in the error message.
inlineError :: Inline -> SomeException -> Filter Inline
inlineError img@Image {} (SomeException e) = do
  imgMarkup <- inlinesToMarkdown [img]
  renderHtml $
    H.div ! A.class_ "decker image error" $ do
      H.h2 ! A.class_ "title" $ do
        H.i ! A.class_ "fa fa-exclamation-triangle" $ ""
        H.text " Decker error"
      H.p ! A.class_ "message" $ toHtml (displayException e)
      H.p $ H.text "encountered while processing"
      H.pre ! A.class_ "markup" $ H.code ! A.class_ "markup" $ toHtml imgMarkup
inlineError _ _ = bug $ InternalException "inlineError: non image argument "

blockError :: Block -> SomeException -> Filter Block
blockError code@CodeBlock {} (SomeException e) = do
  codeMarkup <- blocksToMarkdown [code]
  renderHtml $
    H.div ! A.class_ "decker image error" $ do
      H.h2 ! A.class_ "title" $ do
        H.i ! A.class_ "fa fa-exclamation-triangle" $ ""
        H.text " Decker error"
      H.p ! A.class_ "message" $ toHtml (displayException e)
      H.p $ H.text "encountered while processing"
      H.pre ! A.class_ "markup" $ H.code ! A.class_ "markup" $ toHtml codeMarkup
blockError _ _ = bug $ InternalException "blockError: non code argument "

imageTransformers :: Map MediaT (URI -> [Inline] -> Attrib Html)
imageTransformers =
  Map.fromList
    [ (EmbedSvgT, svgHtml)
    , (PdfT, objectHtml "application/pdf")
    , (MviewT, mviewHtml)
    , (IframeT, iframeHtml)
    , (ImageT, imageHtml)
    , (VideoT, videoHtml)
    , (StreamT, streamHtml')
    , (AudioT, audioHtml)
    , (RenderT, renderCodeHtml)
    ]

transformImage :: Inline -> [Inline] -> Filter Inline
transformImage image@(Image attr@(_, classes, _) _ (url, _)) caption =
  handle (inlineError image) $ do
    uri <- URI.mkURI url
    let mediaType = classifyMedia uri attr
    case Map.lookup mediaType imageTransformers of
      Just transform -> runAttr attr (transform uri caption) >>= renderHtml
      Nothing -> return image
transformImage inline _ = return inline

-- Lines up a list of images in a div element. Use with flexbox css.
transformImages :: [Inline] -> [Inline] -> Filter Block
transformImages images caption = do
  imageRow <-
    mapM (\img@(Image _ caption _) -> transformImage img caption) images
  if null caption
    then renderHtml $
         H.div ! A.class_ "decker image-row" $ toHtml $ map toHtml imageRow
    else do
      captionHtml <- inlinesToHtml caption
      renderHtml $
        H.figure ! A.class_ "decker" $ do
          H.div ! A.class_ "decker image-row" $ toHtml $ map toHtml imageRow
          H.figcaption captionHtml

language cls = find (`elem` ["dot", "gnuplot", "tex"]) cls

-- TODO this is incomplete
--   - captions are just swallowed but never rendered.
--   - caption recognition is disabled for now (see mediaBlockListFilter)
transformCodeBlock :: Block -> [Inline] -> Filter Block
transformCodeBlock code@(CodeBlock attr@(_, classes, _) text) caption =
  handle (blockError code) $
  case language classes of
    Just ext
      | "render" `elem` classes -> do
        transient <-
          lookupMetaOrFail "decker.directories.transient" <$> gets meta
        project <- lookupMetaOrFail "decker.directories.project" <$> gets meta
        runAttr attr (transform project transient ext) >>= renderHtml
    _ -> return code
  where
    transform :: FilePath -> FilePath -> Text -> Attrib Html
    transform project transient ext = do
      dropClass ext
      let crc = printf "%08x" (calc_crc32 $ toString text)
      let relPath =
            deckerFiles </> "code" </> intercalate "-" ["code", crc] <.>
            toString ext
      let absPath = project </> relPath
      exists <- liftIO $ doesFileExist absPath
      unless exists $
        liftIO $ do
          createDirectoryIfMissing True (project </> deckerFiles </> "code")
          Text.writeFile absPath text
      uri <- lift $ URI.mkURI ("/" <> toText relPath)
      renderCodeHtml uri caption
transformCodeBlock block _ = return block

mkAudioTag :: Text -> Attr -> Html
mkAudioTag url (id, cs, kvs) =
  H.audio !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
  H.dataAttribute "src" (H.preEscapedToValue url) !*
  kvs $
  ""

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

mkObjectTag :: Text -> Text -> Attr -> Html
mkObjectTag url mime (id, cs, kvs) =
  H.object !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker" : cs)) !
  A.type_ "application/pdf" !
  A.data_ (H.preEscapedToValue url) !*
  kvs $
  ""

mkSvgTag :: Text -> Attr -> Html
mkSvgTag svg (id, cs, kvs) =
  H.span !? (not (Text.null id), A.id (H.toValue id)) !
  A.class_ (H.toValue ("decker svg" : cs)) !*
  kvs $
  H.preEscapedText svg

audioHtml :: URI -> [Inline] -> Attrib Html
audioHtml uri caption = do
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let audioUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  let audioAttribs =
        takeClasses identity ["controls", "loop", "muted"] >>
        passAttribs identity ["controls", "loop", "muted", "preload"]
  case caption of
    [] -> do
      injectBorder >> takeAutoplay >> audioAttribs >> takeSize >> takeUsual
      mkAudioTag audioUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      audioAttr <- takeAutoplay >> audioAttribs >> extractAttr
      let audioTag = mkAudioTag audioUri audioAttr
      figureAttr <- injectBorder >> takeSize >> takeUsual >> extractAttr
      return $ mkFigureTag audioTag captionHtml figureAttr

isPercent = Text.isSuffixOf "%"

imageHtml :: URI -> [Inline] -> Attrib Html
imageHtml uri caption = do
  uri <- lift $ transformUri uri ""
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let imageTag = mkImageTag (URI.render uri) imgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

objectHtml :: Text -> URI -> [Inline] -> Attrib Html
objectHtml mime uri caption = do
  uri <- lift $ transformUri uri ""
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkObjectTag (URI.render uri) mime <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      objAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let imageTag = mkObjectTag (URI.render uri) mime objAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr

svgHtml :: URI -> [Inline] -> Attrib Html
svgHtml uri caption = do
  uri <- lift $ transformUri uri ""
  svg <- lift $ readLocalUri uri
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkSvgTag svg <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      svgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let svgTag = mkSvgTag svg svgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag svgTag captionHtml <$> extractAttr

mviewHtml :: URI -> [Inline] -> Attrib Html
mviewHtml uri caption = do
  uri <- lift $ transformUri uri ""
  let model = URI.render uri
  pushAttribute ("model", model)
  -- specify mview URL project relative
  mviewUri <- URI.mkURI "public:support/mview/mview.html"
  iframeHtml mviewUri caption

iframeHtml :: URI -> [Inline] -> Attrib Html
iframeHtml uri caption = do
  uri <- lift $ transformUri uri ""
  case caption of
    [] -> do
      iframeAttr <- injectBorder >> takeSize >> takeUsual >> extractAttr
      return $ mkIframeTag (URI.render uri) iframeAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      figureAttr <-
        injectBorder >> takeSizeIf isPercent >> takeId >> takeAllClasses >>
        takeCss >>
        dropCore >>
        passI18n >>
        extractAttr
      iframeAttr <- takeSizeIf (not . isPercent) >> takeData >> extractAttr
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
  uri <- lift $ transformUri uri ""
  mediaFrag <- mediaFragment
  let videoUri =
        if Text.null mediaFrag
          then URI.render uri
          else URI.render uri {URI.uriFragment = URI.mkFragment mediaFrag}
  case caption of
    [] -> do
      injectBorder >> takeAutoplay >> takeVideoClasses >> passVideoAttribs >>
        takeSize >>
        takeUsual
      mkVideoTag videoUri <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      videoAttr <-
        takeSizeIf (not . isPercent) >> takeAutoplay >> takeVideoClasses >>
        passVideoAttribs >>
        extractAttr
      let videoTag = mkVideoTag videoUri videoAttr
      figureAttr <-
        injectBorder >> takeSizeIf isPercent >> takeUsual >> extractAttr
      return $ mkFigureTag videoTag captionHtml figureAttr

-- | Render an SVG image from the code linked to here.
renderCodeHtml :: URI -> [Inline] -> Attrib Html
renderCodeHtml uri caption = do
  uri <- lift $ transformUri uri "svg"
  case caption of
    [] -> do
      injectBorder >> takeSize >> takeUsual
      mkImageTag (URI.render uri) <$> extractAttr
    caption -> do
      captionHtml <- lift $ inlinesToHtml caption
      imgAttr <- takeSizeIf (not . isPercent) >> extractAttr
      let imageTag = mkImageTag (URI.render uri) imgAttr
      injectBorder >> takeSizeIf isPercent >> takeUsual
      mkFigureTag imageTag captionHtml <$> extractAttr
