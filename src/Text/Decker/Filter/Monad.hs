{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Monad where

import Control.Concurrent.MVar
import Relude
import Text.Decker.Internal.Common
import Text.Decker.Internal.Diagnostics (Diagnostic)
import Text.Decker.Internal.Meta
import Text.Pandoc hiding (lookupMeta)

-- |  WriterOptions and the document meta data are available to all
--  filters.
data FilterState = FilterState
  { meta :: Meta,
    dispo :: Disposition,
    codeMutex :: MVar Int,
    -- a cache for fragment templates
    templates :: TVar (Map String (Template Text)),
    -- diagnostics reported by the filter, in order
    filterDiagnostics :: [Diagnostic]
  }

-- | All filters live in the Filter monad.
type Filter = StateT FilterState IO

-- | Records a diagnostic. It is reported when the filter has finished (see
-- 'Text.Decker.Filter.Decker2.runNewFilter').
reportF :: Diagnostic -> Filter ()
reportF diagnostic =
  modify (\s -> s {filterDiagnostics = filterDiagnostics s <> [diagnostic]})

-- | Lookup meta value
lookupMetaF :: FromMetaValue a => Text -> Filter (Maybe a)
lookupMetaF key = do
  m <- gets meta
  return $ lookupMeta key m
