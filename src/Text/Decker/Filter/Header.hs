{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Text.Decker.Filter.Header where

import qualified Data.List as List
import Data.Maybe
import Data.Monoid

import Relude

import Text.Decker.Filter.Attrib
import Text.Decker.Filter.Local
import Text.Decker.Filter.Monad
import Text.Decker.Internal.Exception
import Text.Pandoc
import Text.Pandoc.Walk
import qualified Text.URI as URI

transformHeader1 :: Block -> Filter Block
transformHeader1 h1@(Header 1 headAttr inlines)
  | containsImage inlines =
    (buildHeader $ lastImage inlines) -- >>= putThrough "buildHeader"
  where
    buildHeader img@(Image imgAttr alt (url, title), rest) = do
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
    buildHeader _ =
      bug $ InternalException "transformHeader: no last image in header"
transformHeader1 h1@(Header 1 (id, cls, kvs) inlines) = do
  local <- adjustAttribs bgAttribs kvs
  return (Header 1 (id, cls, local) inlines)
  where
    adjustAttrib :: (Text, Text) -> Filter (Text, Text)
    adjustAttrib (k, v) = (k, ) <$> (URI.render <$> transformUrl v "")
    -- Adjusts the values of all key value attributes that are listed in keys.
    adjustAttribs :: [Text] -> [(Text, Text)] -> Filter [(Text, Text)]
    adjustAttribs keys kvs = do
      let (paths, other) = List.partition ((`elem` keys) . fst) kvs
      local <- mapM adjustAttrib paths
      return $ local <> other
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
