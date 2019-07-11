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

chromeOptions :: FilePath -> FilePath -> [String]
chromeOptions src out =
  ["--headless", "--virtual-time-budget=5000", "--disable-gpu", pdfOption out, modifySrc src]
  where
    modifySrc path = path ++ "?print-pdf#/"
    pdfOption path = "--print-to-pdf=" ++ path

launchChrome :: FilePath -> FilePath -> IO (Either String String)
launchChrome src out = do
  command <- chrome
  let options = unwords (chromeOptions src out)
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
