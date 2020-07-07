{-- Author: Jan-Philipp Stauffert <jan-philipp.stauffert@uni-wuerzburg.de.de> --}
module System.Decker.OS
  ( chrome
  ) where

-- start chrome from cmd
chrome :: IO (Either String String)
chrome = return $ Right "start chrome"
