module Pdf
  ( launchChrome
  ) where

import System.Decker.OS

import Control.Exception
import Data.List
import System.Directory
import System.Exit
import System.IO
import System.Process

{- 
TODO: 
    find chrome/chromium location from inside the System.Decker.OS modules
    use decker pdf to make call to headless chrome
    chrome --headless --disable-gpu --print-to-pdf="<path>" http://0.0.0.0:8888/example-deck.html?print-pdf#/
    
    - Windows:
    windows seems to be able to find chrome with "start chrome"
    so all that is needed is
    start chrome --headless --disable-gpu ...
    
    replace call to decktape
    what about the handout stuff?

   windows can't use "proc" and needs a concrete path
   so the createProcess call looks like
   (_,_,_,ph) <- 
        createProcess(
          shell "start chrome --headless --disable-gpu --print-to-pdf=C:\\Users\\armin\\out.pdf http://0.0.0.0:8888/example-deck.html?print-pdf#/")

    - Mac:
    user level: getHomeDirectory -> "/Users/<name>/Applications/"
    system level: "/Applications/Google Chrome.app/..."

    - Linux:
    require that chrome or chromium is on path

-}
pdfOption :: FilePath -> [Char]
pdfOption path = "--print-to-pdf=" ++ path

modifySrc :: FilePath -> FilePath
modifySrc path = path ++ "?print-pdf#/"

chromeOptions :: FilePath -> FilePath -> [String]
chromeOptions src out =
  ["--headless", "--disable-gpu", pdfOption out, modifySrc src]

launchChrome :: FilePath -> FilePath -> IO (Either String String)
launchChrome src out = do
  command <- chrome
  let options = unwords (chromeOptions src out)
  -- print options
  case command of
    Left msg -> return $ Left msg
    Right cmd -> do
      (_, _, _, ph) <-
        createProcess (shell $ cmd ++ " " ++ options) {std_err = CreatePipe}
      code <- waitForProcess ph
      case code of
        ExitFailure _ ->
          return $
          Left
            ("Google Chrome is most likely not installed. " ++
             "Please install Google Chrome to use 'decker pdf' or 'decker pdf-decks'")
        ExitSuccess -> return $ Right ("Completed: " ++ src ++ " -> " ++ out)
