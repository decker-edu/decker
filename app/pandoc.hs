module Pandoc (pandoc) where

import Text.Pandoc
import Control.Exception

pandoc :: WriterOptions -> FilePath -> FilePath -> IO ()
pandoc options inPath outPath = do
  pandoc <- readFile inPath >>= readMarkdown def
  writeHtmlString options pandoc >>= writeFile outPath

