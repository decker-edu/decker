{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Main
  ( main
  ) where

import Project
import Server
import Control.Concurrent
import Control.Monad
import System.Process

main :: IO ()
main = do
  let port = 9999
  dirs <- projectDirectories
  server <- startHttpServer dirs port
  callCommand $ "open http://localhost:" ++ show port
  forever $ do
    getLine
    reloadClients server
