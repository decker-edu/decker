{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Filter.Run where

import Development.Shake (Action, need)
import Relude
import System.FilePath (takeDirectory)
import Text.Decker.Filter.Filter (Disposition)
import Text.Decker.Filter.Monad (Filter, FilterState (..))
import Text.Decker.Internal.Meta
import Text.Pandoc (Pandoc)
import Text.Pandoc.Definition (Pandoc (..))
import qualified Data.Map as Map
import Text.Pandoc.Walk (Walkable)

-- | Runs a new style decker filter. That means
--
-- 1. Put the decker path info into the documents meta data
-- 2. Run the filter.
-- 3. Take the resource info from the document meta data and
--    a) Call need on every dependency.
--    b) Provision the resources (copy to public)
-- 4. Remove all traces of this from the meta data
runDeckerFilter :: (Pandoc -> IO Pandoc) -> FilePath -> Pandoc -> Action Pandoc
runDeckerFilter filter docPath pandoc@(Pandoc docMeta blocks) = do
  -- liftIO $ putStrLn $ "docPath: " <> docPath
  let deckerMeta =
        setMetaValue "decker.doc-path" docPath
          $ setMetaValue "decker.base-dir" (takeDirectory docPath) docMeta
  (Pandoc resultMeta resultBlocks) <- liftIO $ filter (Pandoc deckerMeta blocks)
  need (lookupMetaOrElse [] "decker.filter.resources" resultMeta)
  --   putNormal $ lookupMetaOrElse [] "decker.filter.resources" resultMeta
  return (Pandoc docMeta resultBlocks)

