{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( defaultProvisioning
  , urlPath
  , preextractedResourceFolder
  , chrome
  ) where

import Common
import System.Directory
import System.Environment
import System.FilePath

defaultProvisioning :: Provisioning
defaultProvisioning = SymLink

urlPath :: FilePath -> FilePath
urlPath path = path

preextractedResourceFolder :: IO FilePath
preextractedResourceFolder = do
  exep <- getExecutablePath
  return $ joinPath [(takeDirectory exep), "..", "Resources", "resource"]

-- Look for chromium executable on $PATH
chromeExecutable :: IO String
chromeExecutable = do
  chrium <- findExecutable "chromium"
  chr <- findExecutable "chrome"
  case (chrium, chr) of
    (Just c, _) -> return c
    (_, Just c) -> return c
    (Nothing, Nothing) ->
      error
        "Neither chrome nor chromium are on $PATH. Please make sure Google Chrome is installed to use 'decker pdf'."

chrome :: IO String
chrome = do
  localExists <- localChrome >>= \h -> doesFileExist h
  globalExists <- doesFileExist chromeLocation
  if globalExists
    then return chromeCommand
    else if localExists
           then localChromeCommand
           else chromeExecutable
  where
    localChrome = fmap (\h -> h ++ chromeLocation) getHomeDirectory
    localChromeCommand = fmap (\h -> h ++ chromeCommand) getHomeDirectory
    chromeLocation =
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    chromeCommand =
      "/Applications/Google\\ Chrome.app/Contents/MacOS/Google\\ Chrome"
