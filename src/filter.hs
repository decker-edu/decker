{-# LANGUAGE OverloadedStrings #-}

module Filter
       (expandMacros, makeSlides, filterNotes, useCachedImages,
        escapeToFilePath, cachePandocImages, extractLocalImagePathes)
       where

import Data.Default ()
import Data.List.Split
import qualified Data.Map as Map (Map, fromList, lookup)
import Data.Maybe
import Debug.Trace
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H (div, figure, iframe, p, toValue, (!))
import Text.Blaze.Html5.Attributes as A
       (class_, height, src, style, width)
import Text.Pandoc.Definition ()
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import Text.Printf
import Text.Read
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.URI

type MacroFunc = [String] -> Attr -> Target -> Format -> Meta -> Inline

-- iframe resizing, see:
-- https://css-tricks.com/NetMag/FluidWidthVideo/Article-FluidWidthVideo.php
-- YouTube links: iv_load_policy=3 disables annotations, rel=0 disables related
-- videos. See:
-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
embedYoutubeHtml
  :: [String] -> Attr -> Target -> Inline
embedYoutubeHtml args attr (vid,_) =
  RawInline (Format "HTML")
            (renderHtml html)
  where url =
          printf "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=1&rel=0&modestbranding=1&autohide=1"
                 vid :: String
        vidWidthStr = macroArg 0 args "560"
        vidHeightStr = macroArg 1 args "315"
        vidWidth = readDefault 560.0 vidWidthStr :: Float
        vidHeight = readDefault 315.0 vidHeightStr :: Float
        wrapperStyle =
          printf "position:relative;padding-top:25px;padding-bottom:%f%%;height:0;"
                 (vidHeight / vidWidth * 100.0) :: String
        iframeStyle =
          "position:absolute;top:0;left:0;width:100%;height:100%;" :: String
        figureStyle (_,_,kv) =
          foldl (\s (k,v) -> s ++ printf "%s:%s;" k v :: String) "" kv
        figureClass (_,cls,_) = unwords cls
        html =
          H.figure ! class_ (toValue (figureClass attr)) !
          style (toValue (figureStyle attr)) $
          H.div ! style (toValue wrapperStyle) $
          iframe ! style (toValue iframeStyle) ! width (toValue vidWidthStr) !
          height (toValue vidHeightStr) !
          src (toValue url) !
          customAttribute "frameborder" "0" !
          customAttribute "allowfullscreen" "" $
          p ""

youtube :: MacroFunc
youtube args attr target (Format "html") _ = embedYoutubeHtml args attr target
youtube args attr target (Format "revealjs") _ =
  embedYoutubeHtml args attr target
youtube _ attr (vid,_) _ _ =
  Link nullAttr
       [Image attr
              [Str text]
              (imageUrl,"")]
       (videoUrl,"")
  where videoUrl =
          printf "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=0&rel=0&modestbranding=1&autohide=1"
                 vid :: String
        imageUrl =
          printf "http://img.youtube.com/vi/%s/maxresdefault.jpg" vid :: String
        text = printf "YouTube: %s" vid :: String

metaValue :: MacroFunc
metaValue _ _ (key,_) _ meta =
  case splitOn "." key of
    [] -> Str key
    k:ks -> lookup' ks (lookupMeta k meta)
  where lookup'
          :: [String] -> Maybe MetaValue -> Inline
        lookup' [] (Just (MetaString s)) = Str s
        lookup' [] (Just (MetaInlines i)) = Span nullAttr i
        lookup' (k:ks) (Just (MetaMap metaMap)) =
          lookup' ks (Map.lookup k metaMap)
        lookup' _ _ = Strikeout [Str key]

type MacroMap = Map.Map String MacroFunc

macroMap :: MacroMap
macroMap = Map.fromList [("meta",metaValue),("youtube",youtube)]

readDefault :: Read a
            => a -> String -> a
readDefault default_ string = fromMaybe default_ (readMaybe string)

macroArg :: Int -> [String] -> String -> String
macroArg n args default_ =
  if length args > n
     then args !! n
     else default_

parseMacro :: String -> Maybe [String]
parseMacro (pre:invocation)
  | pre == ':' = Just (words invocation)
parseMacro _ = Nothing

onlyStrings :: [Inline] -> [String]
onlyStrings = reverse . foldl only []
  where only ss (Str s) = s : ss
        only ss _ = ss

expand
  :: Inline -> Format -> Meta -> Maybe Inline
expand (Image attr text target) format meta =
  expand_ attr text target format meta
expand (Link attr text target) format meta =
  expand_ attr text target format meta
expand x _ _ = Just x

expand_
  :: Attr -> [Inline] -> Target -> Format -> Meta -> Maybe Inline
expand_ attr text target format meta =
  do name:args <- (parseMacro . unwords . onlyStrings) text
     func <- Map.lookup name macroMap
     return (func args attr target format meta)

expandInlineMacros
  :: Format -> Meta -> Inline -> Inline
expandInlineMacros format meta inline =
  fromMaybe inline (expand inline format meta)

expandMacros :: Maybe Format -> Pandoc -> Pandoc
expandMacros (Just format) doc@(Pandoc meta _) =
  walk (expandInlineMacros format meta) doc
expandMacros _ doc = doc

isSlideHeader :: Block -> Bool
isSlideHeader (Header level _ _) = level == 1
isSlideHeader _ = False

isBoxDelim :: Block -> Bool
isBoxDelim (Header level _ _) = level >= 2
isBoxDelim HorizontalRule = True
isBoxDelim _ = False

-- Column break is either "###" or "---"
isColumnBreak :: Block -> Bool
isColumnBreak (Header level _ _) = level == 3
isColumnBreak HorizontalRule = True
isColumnBreak _ = False

columnClass :: Attr
columnClass = ("",["column"],[])

-- Splits the body of a slide into any number of columns.
splitColumns :: [Block] -> [Block]
splitColumns slide@(header:body) =
  let columns = splitWhen isColumnBreak body
      count = length columns
  in if count > 1
        then header :
             concatMap (\(column,n) ->
                          [Div (""
                               ,["slide-column"
                                ,printf "column-%d" n
                                ,printf "columns-%d" count]
                               ,[])
                               column])
                       (Prelude.zip columns
                                    [(1 :: Int) ..])
        else slide
splitColumns [] = []

-- All fragment related classes from reveal.js have to be moved to the enclosing
-- DIV element. Otherwise to many fragments are produced.fragmentRelated :: [String]
fragmentRelated =
  ["fragment"
  ,"grow"
  ,"shrink"
  ,"roll-in"
  ,"fade-in"
  ,"fade-out"
  ,"current-visible"
  ," highlight-current-blue"
  ,"highlight-red"
  ,"highlight-green"
  ,"highlight-blu"]

deFragment :: [String] -> [String]
deFragment = filter (`notElem` fragmentRelated)

wrapBoxes :: [Block] -> [Block]
wrapBoxes (header:body) = header : concatMap wrap boxes
  where boxes = split (keepDelimsL $ whenElt isBoxDelim) body
        wrap (Header 2 (id_,cls,kvs) text:blocks) =
          [Div (id_ ++ "-box","box" : cls,[])
               (Header 2 (id_,deFragment cls,kvs) text : blocks)]
        wrap box = box
wrapBoxes [] = []

-- Wrap headers with class notes into a DIV and promote all header attributes
-- to the DIV.
wrapNoteRevealjs :: [Block] -> [Block]
wrapNoteRevealjs slide@(Header 1 (id_,cls,kvs) inlines:body)
  | "notes" `elem` cls = [Div (id_,cls,kvs) slide]
wrapNoteRevealjs slide = slide

-- Wrap headers with class notes into a DIV and promote all header attributes
-- to the DIV.
wrapNoteBeamer :: [Block] -> [Block]
wrapNoteBeamer slide@(Header 1 (_,cls,_) _:_)
  | "notes" `elem` cls = [Div nullAttr slide]
wrapNoteBeamer slide = slide

mapSlides
  :: ([Block] -> [Block]) -> Pandoc -> Pandoc
mapSlides func (Pandoc meta blocks) = Pandoc meta (concatMap func slides)
  where slides = split (keepDelimsL $ whenElt isSlideHeader) blocks

makeSlides :: Maybe Format -> Pandoc -> Pandoc
makeSlides (Just (Format "revealjs")) =
  walk (mapSlides splitColumns) .
  walk (mapSlides wrapBoxes) . walk (mapSlides wrapNoteRevealjs)
makeSlides (Just (Format "beamer")) =
  walk (mapSlides splitColumns) .
  walk (mapSlides wrapBoxes) . walk (mapSlides wrapNoteBeamer)
makeSlides _ = id

-- Only consider slides that have the 'notes' class in their header. In all
-- others pick only the boxes that are tagged as notes.
filterSlides :: [Block] -> [Block]
filterSlides slide@(Header 1 (_,cls,_) _:_)
  | "notes" `elem` cls = slide
filterSlides (_:body) = concatMap filter boxes
  where boxes = split (keepDelimsL $ whenElt isBoxDelim) body
        filter box@(Header _ (_,cls,_) _:_)
          | "notes" `elem` cls = box
        filter _ = []
filterSlides _ = []

filterNotes :: Maybe Format -> Pandoc -> Pandoc
filterNotes (Just (Format _)) = walk (mapSlides filterSlides)
filterNotes _ = id

escapeToFilePath :: String -> FilePath
escapeToFilePath = map repl
  where repl c =
          if c `elem` [':','!','/']
             then '|'
             else c

useCachedImages :: FilePath -> Inline -> IO Inline
useCachedImages cacheDir img@(Image (ident,cls,values) inlines (url,title)) =
  do let cached = cacheDir </> escapeToFilePath url
     exists <- doesFileExist cached
     if exists
        then return (Image (ident,"cached" : cls,values)
                           inlines
                           (cached,title))
        else return img
useCachedImages _ inline = return inline

localImagePath :: Inline -> [FilePath]
localImagePath (Image _ _ (url, _)) = if isHttpUri url then [] else [url]
localImagePath _ = []

extractLocalImagePathes :: Pandoc -> [FilePath]
extractLocalImagePathes pandoc =
  Text.Pandoc.Walk.query localImagePath pandoc

isHttpUri :: String -> Bool
isHttpUri url =
  case parseURI url of
    Just uri -> uriScheme uri `elem` ["http:","https:"]
    Nothing -> False

cachePandocImages
  :: FilePath -> Inline -> IO Inline
cachePandocImages base img@(Image _ _ (url,_))
  | isHttpUri url =
    do cacheImageIO url base
       return img
  | otherwise = return img

cachePandocImages _ inline = return inline

-- | Download the image behind the URI and save it locally. Return the path of
-- the cached file relative to the base directory.
cacheImageIO
  :: String -> FilePath -> IO ()
cacheImageIO uri cacheDir =
  do request <- parseRequest uri
     result <- httpLBS $ request
     let body = getResponseBody result
     let cacheFile = cacheDir </> escapeToFilePath uri
     createDirectoryIfMissing True cacheDir
     L8.writeFile cacheFile body
