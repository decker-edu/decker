module Text.Decker.Internal.Caches where

import Data.IORef ()
import Data.String ()
import qualified Data.Text as Text
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
  getGlobalMeta <- ($ deckerMetaFile) <$> newCache readDeckerMeta
  getDeps <- ($ targetsFile) <$> newCache readTargetsFile
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
