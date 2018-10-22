{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Macro
  ( expandDeckerMacros
  ) where

import Common
import Control.Monad.State
import Data.List (isInfixOf, isPrefixOf)
import Data.List.Split
import qualified Data.Map as Map (Map, fromList, lookup)
import Data.Maybe
import Data.Text (pack, replace, unpack)
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H ((!), div, figure, iframe, iframe, p, toValue)
import Text.Blaze.Html5.Attributes as A (class_, height, src, style, width)
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Read

type MacroAction = [String] -> Attr -> Target -> Meta -> Decker Inline

-- iframe resizing, see:
-- https://css-tricks.com/NetMag/FluidWidthVideo/Article-FluidWidthVideo.php
-- YouTube links: iv_load_policy=3 disables annotations, rel=0 disables related
-- videos. See:
-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
embedYoutubeHtml :: [String] -> Attr -> Target -> Inline
embedYoutubeHtml args attr (vid, _) =
  RawInline (Format "html") (renderHtml html)
  where
    url =
      printf
        "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=1&rel=0&modestbranding=1&autohide=1"
        vid :: String
    vidWidthStr = macroArg 0 args "560"
    vidHeightStr = macroArg 1 args "315"
    vidWidth = readDefault 560.0 vidWidthStr :: Float
    vidHeight = readDefault 315.0 vidHeightStr :: Float
    wrapperStyle =
      printf
        "position:relative;padding-top:25px;padding-bottom:%f%%;height:0;"
        (vidHeight / vidWidth * 100.0) :: String
    iframeStyle =
      "position:absolute;top:0;left:0;width:100%;height:100%;" :: String
    figureStyle (_, _, kv) =
      foldl (\s (k, v) -> s ++ printf "%s:%s;" k v :: String) "" kv
    figureClass (_, cls, _) = unwords cls
    html =
      H.figure ! class_ (toValue (figureClass attr)) !
      style (toValue (figureStyle attr)) $
      H.div ! style (toValue wrapperStyle) $
      iframe ! style (toValue iframeStyle) ! width (toValue vidWidthStr) !
      height (toValue vidHeightStr) !
      src (toValue url) !
      customAttribute "frameborder" "0" !
      customAttribute "allowfullscreen" "" $
      H.p ""

embedYoutubePdf :: [String] -> Attr -> Target -> Inline
embedYoutubePdf _ attr (vid, _) =
  Link nullAttr [Image attr [Str text] (imageUrl, "")] (videoUrl, "")
  where
    videoUrl =
      printf
        "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=0&rel=0&modestbranding=1&autohide=1"
        vid :: String
    imageUrl =
      printf "http://img.youtube.com/vi/%s/maxresdefault.jpg" vid :: String
    text = printf "YouTube: %s" vid :: String

-- For vimeo embedding settings see: 
-- https://vimeo.zendesk.com/hc/en-us/articles/224983008-Setting-default-quality-for-embedded-videos
embedVimeoHtml :: [String] -> Attr -> Target -> Inline
embedVimeoHtml args attr (vid, _) = RawInline (Format "html") (renderHtml html)
  where
    url = printf "https://player.vimeo.com/video/%s?quality=1080p" vid :: String
    vidWidthStr = macroArg 0 args "560"
    vidHeightStr = macroArg 1 args "315"
    vidWidth = readDefault 560.0 vidWidthStr :: Float
    vidHeight = readDefault 315.0 vidHeightStr :: Float
    wrapperStyle =
      printf
        "position:relative;padding-top:25px;padding-bottom:%f%%;height:0;"
        (vidHeight / vidWidth * 100.0) :: String
    iframeStyle =
      "position:absolute;top:0;left:0;width:100%;height:100%;" :: String
    figureStyle (_, _, kv) =
      foldl (\s (k, v) -> s ++ printf "%s:%s;" k v :: String) "" kv
    figureClass (_, cls, _) = unwords cls
    html =
      H.figure ! class_ (toValue (figureClass attr)) !
      style (toValue (figureStyle attr)) $
      H.div ! style (toValue wrapperStyle) $
      iframe ! style (toValue iframeStyle) ! width (toValue vidWidthStr) !
      height (toValue vidHeightStr) !
      src (toValue url) !
      customAttribute "frameborder" "0" !
      customAttribute "allowfullscreen" "" $
      H.p ""

youtube :: MacroAction
youtube args attr target _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html -> return $ embedYoutubeHtml args attr target
    Disposition _ Pdf -> return $ embedYoutubePdf args attr target

vimeo :: MacroAction
vimeo args attr target _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html -> return $ embedVimeoHtml args attr target

fontAwesome :: MacroAction
fontAwesome _ _ (iconName, _) _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html ->
      return $
      RawInline (Format "html") $ "<i class=\"fa fa-" ++ iconName ++ "\"></i>"
    Disposition _ Pdf -> return $ Str $ "[" ++ iconName ++ "]"

metaValue :: MacroAction
metaValue _ _ (key, _) meta =
  case splitOn "." key of
    [] -> return $ Str key
    k:ks -> return $ lookup' ks (lookupMeta k meta)
  where
    lookup' :: [String] -> Maybe MetaValue -> Inline
    lookup' [] (Just (MetaString s)) = Str s
    lookup' [] (Just (MetaInlines i)) = Span nullAttr i
    lookup' (k:ks) (Just (MetaMap metaMap)) = lookup' ks (Map.lookup k metaMap)
    lookup' _ _ = Strikeout [Str key]

type MacroMap = Map.Map String MacroAction

macroMap :: MacroMap
macroMap =
  Map.fromList
    [ ("meta", metaValue)
    , ("youtube", youtube)
    , ("fa", fontAwesome)
    , ("vimeo", vimeo)
    ]

readDefault :: Read a => a -> String -> a
readDefault default_ string = fromMaybe default_ (readMaybe string)

macroArg :: Int -> [String] -> String -> String
macroArg n args default_ =
  if length args > n
    then args !! n
    else default_

-- parse e.g. [:youtube](...) and return Just [youtube]
parseMacro :: String -> Maybe [String]
parseMacro (pre:invocation)
  | pre == ':' = Just (words invocation)
parseMacro _ = Nothing

-- lookup e.g. "youtube" in macroMap
expandInlineMacros :: Meta -> Inline -> Decker Inline
expandInlineMacros meta inline@(Link attr text target) =
  case parseMacro $ stringify text of
    Just (name:args) ->
      case Map.lookup name macroMap of
        Just macro -> macro args attr target meta
        Nothing -> return inline
    _ -> return inline
expandInlineMacros meta inline@(Image attr _ (url, tit))
  -- Problem: "url" points to a path in the file system 
  -- (appending the actual content in the brackets to directory)
  -- hacky workaround: split at youtube-indicator and take last element
  -- Also: hardcoded lookup of "youtube" is not useful
  -- TODO: generalize for more than just youtube
  -- Use Attributes:
  -- ![](vnd.youtube://Wji-BZ0oCwg){#id .video width="75%"}
  -- attr@(ident, cls, values)
  -- ident = id
  -- cls = .video (unwords cls) results in class="video"
  -- values = width="75%"
  -- if "vnd.youtube://" `isPrefixOf` url
  -- State: 22.10.18 too much boilerplate for youtube and vimeo. generalize!
 =
  case findEmbeddingClass inline of
    Just "youtube" ->
      case Map.lookup "youtube" macroMap of
        Just macro -> macro [] attr (code, tit) meta
            -- where code = (last . splitOn "vnd.youtube://") url
          where code = unpack $ replace "vnd.youtube://" "" (pack url)
        Nothing -> return inline
    Just "vimeo" ->
      case Map.lookup "vimeo" macroMap of
        Just macro -> macro [] attr (code, tit) meta
          where code = unpack $ replace "vimeo://" "" (pack url)
        Nothing -> return inline
    Nothing -> return inline
expandInlineMacros _ inline = return inline

-- Check inline for special embedding content if inline is Image
-- TODO: look at url for prefixes for youtube, vimeo
-- look at attributes for content type (image, video, dot, iframe)
-- also put [:include] in here
findEmbeddingClass :: Inline -> Maybe String
findEmbeddingClass inline@(Image attr text (url, tit))
  | "vnd.youtube://" `isPrefixOf` url = Just "youtube"
  | "vimeo://" `isPrefixOf` url = Just "vimeo"
  | otherwise = Nothing
  -- if "vnd.youtube://" `isPrefixOf` url
    -- then Just "youtube"
    -- else Nothing

expandDeckerMacros :: Pandoc -> Decker Pandoc
expandDeckerMacros doc@(Pandoc meta _) = walkM (expandInlineMacros meta) doc
