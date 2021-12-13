{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Monad where

import Relude
import Text.Decker.Internal.Common
import Text.Decker.Internal.Meta
import Text.Pandoc hiding (lookupMeta)

-- |  WriterOptions and the document meta data are available to all
--  filters.
data FilterState = FilterState
  { meta :: Meta,
    dispo :: Disposition
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

-- | Lookup meta value
lookupMetaF :: FromMetaValue a => Text -> Filter (Maybe a)
lookupMetaF key = do
  m <- gets meta
  return $ lookupMeta key m
