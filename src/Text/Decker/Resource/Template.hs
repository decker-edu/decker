module Text.Decker.Resource.Template
  ( readTemplates
  , Template(..)
  ) where

import Text.Decker.Resource.Zip

import Control.Monad
import qualified Data.ByteString as BS
import System.FilePath

templates =
  [ "template/deck.html"
  , "template/deck.md"
  , "template/handout.html"
  , "template/handout.tex"
  , "template/page.html"
  , "template/page.tex"
  ]

data Template =
  Template BS.ByteString
           (Maybe FilePath)
  deriving (Show)

readTemplates :: FilePath -> Bool -> IO [(FilePath, Template)]
readTemplates root devRun =
  if devRun
    then readTemplatesFs (root </> "resource")
    else readTemplatesZip

readTemplatesFs :: FilePath -> IO [(FilePath, Template)]
readTemplatesFs dir = foldM readTemplate [] templates
  where
    readTemplate list path = do
      let file = dir </> path
      content <- BS.readFile file
      return $ (path, Template content (Just file)) : list

readTemplatesZip :: IO [(FilePath, Template)]
readTemplatesZip =
  map (\(f, c) -> (f, Template c Nothing)) <$>
  extractResourceEntryList templates
