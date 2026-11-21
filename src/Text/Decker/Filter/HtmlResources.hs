{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Decker.Filter.HtmlResources where

import Relude
import Text.HTML.TagSoup
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Meta
import Text.Decker.Internal.URI
import Text.Decker.Filter.Local (transformUri)
import Text.URI qualified as URI

-- | Scans HTML text for image/media references, adds them to the resources list,
-- and returns the HTML with adjusted paths.
scanHtmlResources :: Text -> Filter Text
scanHtmlResources html = do
  let tags = parseTags html
  transformedTags <- mapM transformTag tags
  return $ renderTags transformedTags

  where
    transformTag :: Tag Text -> Filter (Tag Text)
    transformTag (TagOpen name attrs) = do
      newAttrs <- mapM (transformAttr name) attrs
      return $ TagOpen name newAttrs
    transformTag tag = return tag

    transformAttr :: Text -> Attribute Text -> Filter (Attribute Text)
    transformAttr tagName (attrName, attrValue)
      | isPathAttr tagName attrName = do
          uri <- URI.mkURI attrValue
          -- Only transform local URIs to avoid breaking remote links
          if not (isRemoteUri uri)
            then do
              turi <- transformUri uri ""
              return (attrName, URI.render turi)
            else return (attrName, attrValue)
      | otherwise = return (attrName, attrValue)

    -- Attributes that typically contain paths to assets
    isPathAttr :: Text -> Text -> Bool
    isPathAttr _ "src" = True
    isPathAttr _ "href" = True
    isPathAttr "link" "href" = True
    isPathAttr "img" "src" = True
    isPathAttr "video" "src" = True
    isPathAttr "audio" "src" = True
    isPathAttr "source" "src" = True
    isPathAttr _ _ = False

    isRemoteUri :: URI.URI -> Bool
    isRemoteUri uri =
      case URI.uriScheme uri of
        Just s -> URI.unRText s `notElem` ["file", "public"]
        Nothing -> URI.isPathAbsolute uri
