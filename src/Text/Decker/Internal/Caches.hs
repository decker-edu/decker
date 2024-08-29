module Text.Decker.Internal.Caches where

import Data.IORef ()
import Data.String ()
import Data.Text qualified as Text
import Development.Shake
import Text.Decker.Internal.Common
import Text.Decker.Project.Project
import Text.Decker.Reader.Markdown
import Text.Decker.Resource.Template
import Text.Pandoc hiding (lookupMeta)

type ParamCache a = FilePath -> Action a

type Cache a = Action a

prepCaches :: Rules (Cache Meta, Cache Targets, ParamCache (Template Text.Text))
prepCaches = do
  targets <- liftIO targetsFile
  getGlobalMeta <- ($ deckerMetaFile) <$> newCache readDeckerMeta
  getDeps <- ($ targets) <$> newCache readTargetsFile
  getTemplate <-
    newCache
      ( \path -> do
          meta <- getGlobalMeta
          (template, needed) <- liftIO $ readTemplate meta path
          need needed
          return template
      )
  -- targetsFile %> \targetFile -> do
  --   alwaysRerun
  --   meta <- getGlobalMeta
  --   scanTargetsToFile meta targetFile
  return (getGlobalMeta, getDeps, getTemplate)
