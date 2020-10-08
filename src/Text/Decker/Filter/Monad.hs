{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Monad where

import Relude
import Text.Decker.Internal.Common
import Text.Pandoc

-- |  WriterOptions and the document meta data are available to all
--  filters.
data FilterState = FilterState
  { options :: WriterOptions,
    meta :: Meta,
    dispo :: Disposition
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO
