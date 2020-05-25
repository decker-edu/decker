{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Monad where

import Relude
import Text.Pandoc

-- | WriterOptions and the document meta data are available to all
-- filters. 
data FilterState = FilterState
  { options :: WriterOptions
  , meta :: Meta
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO
