module Text.Decker.Resource.Template
  ( getTemplate
  ) where

import Text.Decker.Internal.Common
import Text.Decker.Project.Project

import Data.Maybe
import Development.Shake
import System.FilePath
import Text.Pandoc

getTemplate :: Meta -> Disposition -> Action String
getTemplate meta disp = do
  let templateOverridePath =
        case templateFromMeta meta of
          Just template -> Just $ template </> getTemplateFileName disp
          Nothing -> Nothing
  if isJust templateOverridePath
    then do
      let templateOverridePath' = fromJust templateOverridePath
      need [templateOverridePath']
      liftIO $ readFile templateOverridePath'
    else liftIO $ getResourceString ("template" </> (getTemplateFileName disp))

-- | Determines which template file name to use
-- for a certain disposition type
getTemplateFileName :: Disposition -> String
getTemplateFileName (Disposition Deck Html) = "deck.html"
getTemplateFileName (Disposition Deck Latex) = "deck.tex"
getTemplateFileName (Disposition Page Html) = "page.html"
getTemplateFileName (Disposition Page Latex) = "page.tex"
getTemplateFileName (Disposition Handout Html) = "handout.html"
getTemplateFileName (Disposition Handout Latex) = "handout.tex"
