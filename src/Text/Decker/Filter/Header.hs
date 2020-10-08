{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Header where

import Data.Maybe
import Data.Monoid
import Relude
import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

transformHeader1 :: Block -> Filter Block
transformHeader1 h1@(Header 1 headAttr inlines)
  | containsImage inlines = do
    disp <- gets dispo
    (buildMediaHeader disp $ lastImage inlines) -- >>= putThrough "buildMediaHeader"
  where
    buildMediaHeader (Disposition Deck Html) (Image imgAttr alt (url, title), rest) = do
      uri <- transformUrl url ""
      runAttrOn headAttr imgAttr $
        case classifyMedia uri imgAttr of
          ImageT -> do
            injectAttribute ("data-background-image", URI.render uri)
            passAttribs
              ("data-background-" <>)
              ["size", "position", "repeat", "opacity"]
            attr <- extractAttr
            return $ Header 1 attr rest
          VideoT -> do
            injectAttribute ("data-background-video", URI.render uri)
            takeClasses ("data-background-video-" <>) ["loop", "muted"]
            passAttribs ("data-background-" <>) ["size", "opacity"]
            passAttribs ("data-background-video-" <>) ["loop", "muted"]
            attr <- extractAttr
            return $ Header 1 attr rest
          IframeT -> do
            injectAttribute ("data-background-iframe", URI.render uri)
            takeClasses ("data-background-" <>) ["interactive"]
            passAttribs ("data-background-" <>) ["interactive"]
            attr <- extractAttr
            return $ Header 1 attr rest
          PdfT -> do
            injectAttribute ("data-background-iframe", URI.render uri)
            takeClasses ("data-background-" <>) ["interactive"]
            passAttribs ("data-background-" <>) ["interactive"]
            attr <- extractAttr
            return $ Header 1 attr rest
          _ -> return h1
    buildMediaHeader (Disposition Handout Html) (img@(Image imgAttr alt (url, title)), rest) = do
      uri <- transformUrl url ""
      runAttrOn headAttr imgAttr $ do
        attr <- extractAttr
        return $ Div nullAttr [Header 1 attr rest, Para [img]]
    buildMediaHeader _ _ =
      bug $ InternalException "transformHeader: no last image in header"
transformHeader1 h1@(Header 1 (id, cls, kvs) inlines) = do
  local <- adjustAttribPaths bgAttribs kvs
  return (Header 1 (id, cls, local) inlines)
transformHeader1 h@Header {} = return h
transformHeader1 block = return block

containsImage :: [Inline] -> Bool
containsImage = getAny . query check
  where
    check Image {} = Any True
    check _ = Any False

lastImage :: [Inline] -> (Inline, [Inline])
lastImage inlines =
  (fromJust $ listToMaybe $ reverse $ query image inlines, zapImages inlines)
  where
    image i@(Image {}) = [i]
    image _ = []

zapImages :: [Inline] -> [Inline]
zapImages = walk zap
  where
    zap Image {} = Space
    zap inline = inline
