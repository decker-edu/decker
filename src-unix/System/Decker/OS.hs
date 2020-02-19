{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  , chrome
  ) where

import Text.Decker.Internal.Common
import System.Directory
import System.Environment
import System.FilePath

defaultProvisioning :: Provisioning
defaultProvisioning = SymLink

urlPath :: FilePath -> FilePath
urlPath path = path

-- Look for chrome executable on $PATH
chromeExecutable :: IO (Either String String)
chromeExecutable = do
  chr <- findExecutable "chrome"
  case chr of
    Just c -> return $ Right c
    Nothing ->
      return $
      Left
        "'chrome' is not on $PATH. Please make sure 'chrome' is pointing to your Google Chrome installation."

-- Check for MacOS standard installation locations
-- /Applications/Google Chrome.app
-- /Users/<username>/Applications/Google Chrome.app
chrome :: IO (Either String String)
chrome = do
  localExists <- localChrome >>= \h -> doesFileExist h
  globalExists <- doesFileExist chromeLocation
  if globalExists
    then return $ Right chromeCommand
    else if localExists
           then localChromeCommand
           else do
             exe <- chromeExecutable
             case exe of
               Right c -> return $ Right c
               Left msg ->
                 return $
                 Left
                   ("MacOS: Google Chrome.app was not found in /Applications or /User/<username>/Applications. Please install Google Chrome.\n" ++
                    "Generic Unix: " ++ msg)
  where
    localChrome = fmap (\h -> h ++ chromeLocation) getHomeDirectory
    localChromeCommand =
      fmap (\h -> Right (h ++ chromeCommand)) getHomeDirectory
    chromeLocation =
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    chromeCommand =
      "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome"
