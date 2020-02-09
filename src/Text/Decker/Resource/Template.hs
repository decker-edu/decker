module Text.Decker.Resource.Template
  ( readTemplates
  , DeckerTemplate(..)
  ) where

import Text.Decker.Resource.Zip

import Control.Monad
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import System.FilePath

templates =
  [ "template/deck.html"
  , "template/deck.md"
  , "template/handout.html"
  , "template/handout.tex"
  , "template/page.html"
  , "template/page.tex"
  ]

data DeckerTemplate =
  DeckerTemplate Text.Text
           (Maybe FilePath)
  deriving (Show)

readTemplates :: FilePath -> Bool -> IO [(FilePath, DeckerTemplate)]
readTemplates root devRun =
  if devRun
    then readTemplatesFs (root </> "resource")
    else readTemplatesZip

readTemplatesFs :: FilePath -> IO [(FilePath, DeckerTemplate)]
readTemplatesFs dir = foldM readTemplate [] templates
  where
    readTemplate list path = do
      let file = dir </> path
      content <- Text.readFile file
      return $ (path, DeckerTemplate content (Just file)) : list

readTemplatesZip :: IO [(FilePath, DeckerTemplate)]
readTemplatesZip =
  map (\(f, content) -> (f, DeckerTemplate (Text.decodeUtf8 content) Nothing)) <$>
  extractResourceEntryList templates
