{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Filter.Macro
  ( expandDeckerMacros
  , embedWebVideosHtml
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta

import Control.Monad.State
import Data.List (find)
import qualified Data.Map as Map (Map, fromList, lookup)
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Text.Blaze (customAttribute)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 as H ((!), div, figure, iframe, iframe, p, toValue)
import Text.Blaze.Html5.Attributes as A (class_, height, src, style, width)
import Text.Pandoc
import Text.Pandoc.Definition ()
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Printf
import Text.Read

type MacroAction = [Text.Text] -> Attr -> Target -> Meta -> Decker Inline

-- iframe resizing, see:
-- https://css-tricks.com/NetMag/FluidWidthVideo/Article-FluidWidthVideo.php
-- YouTube links: iv_load_policy=3 disables annotations, rel=0 disables related
-- videos. See:
-- https://developers.google.com/youtube/player_parameters?hl=de#IFrame_Player_API
-- For vimeo embedding settings see:
-- https://vimeo.zendesk.com/hc/en-us/articles/360001494447-Using-Player-Parameters
-- For twitch embedding settings see:
-- https://dev.twitch.tv/docs/embed/video-and-clips/ and:
-- https://dev.twitch.tv/docs/embed/everything/
embedWebVideosHtml :: Text.Text -> [Text.Text] -> Attr -> Target -> Inline
embedWebVideosHtml page args attr@(_, _, kv) (vid, _) =
  RawInline (Format "html") (LazyText.toStrict $ renderHtml html)
  where
    start =
      case find (\(x, y) -> x == "t" || x == "start") kv of
        Just (_, time) -> time
        _ -> "0"
    autoplay =
      case find (\(x, y) -> Text.isInfixOf "autoplay" x) kv of
        Just (_, b) -> b
        _ -> "0"
    url =
      case page of
        "youtube" ->
          printf
            "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=1&rel=0&modestbranding=1&autohide=1&start=%s"
            vid
            start :: String
        "vimeo" ->
          printf
            "https://player.vimeo.com/video/%s?quality=autop&muted=0#t=%s"
            vid
            start :: String
        "twitch" ->
          printf "https://player.twitch.tv/?channel=%s&autoplay=1&muted=1" vid :: String
        "veer" ->
          printf "https://h5.veer.tv/player?vid=%s&amp;utm_medium=embed" vid :: String
        "veer-photo" ->
          printf
            "https://h5.veer.tv/photo-player?pid=%s&amp;utm_medium=embed"
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
    figureClass (_, cls, _) = Text.unwords cls
    html =
      H.figure ! class_ (toValueT (figureClass attr)) !
      style (toValue (figureStyle attr)) $
      H.div ! style (toValue wrapperStyle) $
      iframe ! style (toValue iframeStyle) ! width (toValue vidWidthStr) !
      height (toValue vidHeightStr) !
      src (toValue url) !
      customAttribute "frameborder" "0" !
      auto !
      customAttribute "allowfullscreen" "" $
      H.p ""
    auto =
      if (autoplay == "1" || autoplay == "true")
        then (customAttribute "data-autoplay" "")
        else mempty

toValueT = toValue . Text.unpack

-- Twitch thumbnail from https://www.twitch.tv/p/brand/social-media
-- Twitch channels unfortunately have no fixed thumbnail
embedWebVideosPdf :: Text.Text -> [Text.Text] -> Attr -> Target -> Inline
embedWebVideosPdf page _ attr (vid, _) =
  Link
    nullAttr
    [Image attr [Str $ Text.pack text] (Text.pack imageUrl, "")]
    (Text.pack videoUrl, "")
  where
    videoUrl =
      case page of
        "youtube" ->
          printf
            "https://www.youtube.com/embed/%s?iv_load_policy=3&disablekb=1&rel=0&modestbranding=1&autohide=1"
            vid :: String
        "vimeo" ->
          printf
            "https://player.vimeo.com/video/%s?quality=autop&autoplay=0&muted=1"
            vid :: String
        "twitch" ->
          printf "https://player.twitch.tv/?channel=%s&autoplay=0&muted=1" vid :: String
    text =
      case page of
        "youtube" -> printf "YouTube: %s" vid :: String
        "vimeo" -> printf "Vimeo: %s" vid :: String
        "twitch" -> printf "Twitch: %s" vid :: String
    imageUrl =
      case page of
        "youtube" ->
          printf "http://img.youtube.com/vi/%s/maxresdefault.jpg" vid :: String
        "vimeo" ->
          printf "https://i.vimeocdn.com/video/%s_560x315.jpg" vid :: String
        "twitch" ->
          "https://www.twitch.tv/p/assets/uploads/glitch_solo_750x422.png"

webVideo :: Text.Text -> MacroAction
webVideo page args attr target _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html -> return $ embedWebVideosHtml page args attr target
    Disposition _ Latex -> return $ embedWebVideosPdf page args attr target

fontAwesome :: Text.Text -> MacroAction
fontAwesome which _ _ (iconName, _) _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html ->
      return $
      RawInline (Format "html") $
      Text.concat ["<i class=\"", which, " fa-", iconName, "\"></i>"]
    Disposition _ Latex -> return $ Str $ "[" <> iconName <> "]"

horizontalSpace :: MacroAction
horizontalSpace _ _ (space, _) _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html ->
      return $
      RawInline (Format "html") $
      Text.pack $
      printf "<span style=\"display:inline-block; width:%s;\"></span>" space
    Disposition _ Latex -> return $ Str $ "[" <> space <> "]"

verticalSpace :: MacroAction
verticalSpace _ _ (space, _) _ = do
  disp <- gets disposition
  case disp of
    Disposition _ Html ->
      return $
      RawInline (Format "html") $
      Text.pack $
      printf "<div style=\"display:block; clear:both; height:%s;\"></div>" space
    Disposition _ Latex -> return $ Str $ "[" <> space <> "]"

metaValue :: MacroAction
metaValue _ _ (key, _) meta =
  return $ fromMaybe (Strikeout [Str key]) (getMetaValue key meta >>= toInline)
  where
    toInline (MetaBool False) = Just $ Str "false"
    toInline (MetaBool True) = Just $ Str "true"
    toInline (MetaString s) = Just $ Str s
    toInline (MetaInlines i) = Just $ Span nullAttr i
    toInline _ = Nothing

type MacroMap = Map.Map Text.Text MacroAction

macroMap :: MacroMap
macroMap =
  Map.fromList
    [ ("meta", metaValue)
    , ("fa", fontAwesome "fas")
    , ("fas", fontAwesome "fas")
    , ("far", fontAwesome "far")
    , ("fab", fontAwesome "fab")
    --, ("youtube", webVideo "youtube")
    --, ("vimeo", webVideo "vimeo")
    --, ("twitch", webVideo "twitch")
    , ("veer", webVideo "veer")
    , ("veer-photo", webVideo "veer-photo")
    , ("hspace", horizontalSpace)
    , ("vspace", verticalSpace)
    ]

readDefault :: Read a => a -> Text.Text -> a
readDefault default_ string =
  fromMaybe default_ (readMaybe $ Text.unpack string)

macroArg :: Int -> [Text.Text] -> Text.Text -> Text.Text
macroArg n args default_ =
  if length args > n
    then args !! n
    else default_

parseMacro :: Text.Text -> Maybe [Text.Text]
parseMacro invocation = Text.words <$> Text.stripPrefix ":" invocation

-- lookup e.g. "youtube" in macroMap and return MacroAction/Decker Inline
expandInlineMacros :: Meta -> Inline -> Decker Inline
expandInlineMacros meta inline@(Link attr text target) =
  case parseMacro $ stringify text of
    Just (name:args) ->
      case Map.lookup name macroMap of
        Just macro -> macro args attr target meta
        Nothing -> return inline
    _ -> return inline
expandInlineMacros meta inline@(Image attr _ (url, tit))
  -- For the case of web videos
 =
  case findEmbeddingType inline of
    Just str ->
      case Map.lookup str macroMap of
        Just macro -> macro [] attr (code, tit) meta
        -- TODO: Find a way to do this without needing Data.Text and the whole pack/unpack effort
          where code = Text.replace (str <> "://") "" url
        Nothing -> return inline
    Nothing -> return inline
expandInlineMacros _ inline = return inline

-- Check inline for special embedding content (currently only web videos) if
-- inline is Image
findEmbeddingType :: Inline -> Maybe Text.Text
findEmbeddingType inline@(Image attr text (url, tit))
  | "youtube://" `Text.isPrefixOf` url = Just "youtube"
  | "vimeo://" `Text.isPrefixOf` url = Just "vimeo"
  | "twitch://" `Text.isPrefixOf` url = Just "twitch"
  | "veer://" `Text.isPrefixOf` url = Just "veer"
  | "veer-photo://" `Text.isPrefixOf` url = Just "veer-photo"
  | otherwise = Nothing

expandDeckerMacros :: Pandoc -> Decker Pandoc
expandDeckerMacros doc@(Pandoc meta _) = walkM (expandInlineMacros meta) doc
