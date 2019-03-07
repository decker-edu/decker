module Pdf
  ( getDirs
  , launchChrome
  ) where

import System.Decker.OS

import System.Directory
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
getDirs :: IO ()
getDirs
  -- dirs <- getXdgDirectoryList
 = do
  home <- getHomeDirectory
  print home
  let apps = "/Applications"
  contents <- listDirectory apps
  print contents

-- This will be what is imported from the OS module
chrome :: FilePath
chrome = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

pdfPath = "test.pdf"

pdfOption :: FilePath -> [Char]
pdfOption path = "--print-to-pdf=" ++ path

launchChrome :: IO ()
launchChrome = do
  (_, _, _, ph2) <-
    createProcess
      (proc
         chrome
         [ "--headless"
         , "--disable-gpu"
         , pdfOption pdfPath
         , "http://0.0.0.0:8888/example-deck.html?print-pdf#/"
         ])
  waitForProcess ph2
  print "test"
