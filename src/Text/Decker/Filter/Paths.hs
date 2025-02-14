{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Paths
  ( adjustResourcePathsA,
    adjustResourcePaths,
  )
where

import Control.Monad
import qualified Data.List as List
import Development.Shake hiding (Resource)
import Relude
import Text.Decker.Filter.Attrib
import Text.Decker.Internal.URI
import Text.Pandoc
import Text.Pandoc.Walk

-- |  Traverses the pandoc AST and adjusts local resource paths. Paths are
--  considered in these places:
--
--  1. Url field on Image elements
--  2. src and data-src attributes on Image and CodeBlock elements
--  3. data-backgound-* attributes in Header 1 elements
--
--  Adjusts the image url and all source attributes. Which source is used
--  and how is decided by the media plugin. Calling need is the
--  responsibility of the media plugin.
adjustResourcePaths :: FilePath -> Pandoc -> Pandoc
adjustResourcePaths base = walk adjustInline . walk adjustBlock
  where
    adjustInline :: Inline -> Inline
    adjustInline (Image (id, cls, kvs) alt (url, title)) =
      let localUrl = adjustUrl url
          localAttr = adjustAttribs srcAttribs kvs
       in Image (id, cls, localAttr) alt (localUrl, title)
    adjustInline inline = inline
    adjustUrl :: Text -> Text
    adjustUrl url = toText $ makeProjectPath base (toString url)
    -- Adjusts code block and header attributes.
    adjustBlock :: Block -> Block
    adjustBlock (CodeBlock (id, cls, kvs) text) =
      let local = adjustAttribs srcAttribs kvs
       in CodeBlock (id, cls, local) text
    adjustBlock (Header 1 (id, cls, kvs) inlines) =
      let local = adjustAttribs bgAttribs kvs
       in Header 1 (id, cls, local) inlines
    adjustBlock block = block
    -- Adjusts the value of one attribute.
    adjustAttrib :: (Text, Text) -> (Text, Text)
    adjustAttrib (k, v) = (k, toText $ makeProjectPath base (toString v))
    -- Adjusts the values of all key value attributes that are listed in keys.
    adjustAttribs :: [Text] -> [(Text, Text)] -> [(Text, Text)]
    adjustAttribs keys kvs = do
      let (paths, other) = List.partition ((`elem` keys) . fst) kvs
          local = map adjustAttrib paths
       in local <> other

adjustResourcePathsA :: FilePath -> Pandoc -> Action Pandoc
adjustResourcePathsA base pandoc = do
  walkM adjustInline pandoc >>= walkM adjustBlock
  where
    adjustInline :: Inline -> Action Inline
    adjustInline (Image (id, cls, kvs) alt (url, title)) = do
      localUrl <- adjustUrl url
      localAttr <- adjustAttribs srcAttribs kvs
      return $ Image (id, cls, localAttr) alt (localUrl, title)
    adjustInline inline = return inline
    adjustUrl :: Text -> Action Text
    adjustUrl url = liftIO $ makeProjectUriPath base url
    -- Adjusts code block and header attributes.
    adjustBlock :: Block -> Action Block
    adjustBlock (CodeBlock (id, cls, kvs) text) = do
      local <- adjustAttribs srcAttribs kvs
      return $ CodeBlock (id, cls, local) text
    adjustBlock (Header 1 (id, cls, kvs) inlines) = do
      local <- adjustAttribs bgAttribs kvs
      return $ Header 1 (id, cls, local) inlines
    adjustBlock block = return block
    -- Adjusts the value of one attribute.
    adjustAttrib :: (Text, Text) -> Action (Text, Text)
    adjustAttrib (k, v) = (k,) <$> liftIO (makeProjectUriPath base v)
    -- Adjusts the values of all key value attributes that are listed in keys.
    adjustAttribs :: [Text] -> [(Text, Text)] -> Action [(Text, Text)]
    adjustAttribs keys kvs = do
      let (paths, other) = List.partition ((`elem` keys) . fst) kvs
      local <- mapM adjustAttrib paths
      return $ local <> other
