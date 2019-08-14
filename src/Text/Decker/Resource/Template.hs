module Text.Decker.Resource.Template
  ( readTemplates
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Project.Project
import Text.Decker.Resource.Zip

import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe
import Development.Shake
import System.FilePath
import Text.Pandoc

templates =
  [ "template/deck.html"
  , "template/deck.md"
  , "template/handout.html"
  , "template/handout.tex"
  , "template/page.html"
  , "template/page.tex"
  ]

readTemplates :: FilePath -> Bool -> IO [(FilePath, BS.ByteString)]
readTemplates root devRun =
  if devRun
    then readTemplatesFs (root </> "resource")
    else readTemplatesZip

readTemplatesFs :: FilePath -> IO [(FilePath, BS.ByteString)]
readTemplatesFs dir = do
  foldM readTemplate [] templates
  where
    readTemplate list path = do
      content <- BS.readFile (dir </> path)
      return $ (path, content) : list

readTemplatesZip :: IO [(FilePath, BS.ByteString)]
readTemplatesZip = extractResourceEntryList templates
