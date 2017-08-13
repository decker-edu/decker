{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
{-# LANGUAGE OverloadedStrings #-}

module Server
  ( startHttpServer
  , stopHttpServer
  , reloadClients
  , Server
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import Data.Text
import Network.WebSockets
import Network.WebSockets.Snap
import Project
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Directory
import System.FilePath.Posix
import System.Random

serverConfig dirs port = do
  let logDir = Project.log dirs
  let accessLog = logDir </> "server-access.log"
  let errorLog = logDir </> "server-error.log"
  createDirectoryIfMissing True logDir
  return
    (setPort port $
     setAccessLog (ConfigFileLog accessLog) $
     setErrorLog (ConfigFileLog errorLog) defaultConfig :: Config Snap a)

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type ServerState = [Client]

type Server = (ThreadId, MVar ServerState)

initState :: IO (MVar ServerState)
initState = newMVar []

addClient :: MVar ServerState -> Client -> IO ()
addClient state client = modifyMVar_ state add
  where
    add clients = return (client : clients)

removeClient :: MVar ServerState -> Int -> IO ()
removeClient state cid = modifyMVar_ state remove
  where
    remove clients = return [c | c <- clients, cid /= fst c]

reloadAll :: MVar ServerState -> IO ()
reloadAll state = withMVar state $ mapM_ send
  where
    send :: Client -> IO ()
    send (cid, conn) = sendTextData conn ("reload!" :: Text)

runHttpServer :: MVar ServerState -> ProjectDirs -> Int -> IO ()
runHttpServer state dirs port = do
  let documentRoot = public dirs
  config <- serverConfig dirs port
  simpleHttpServe config $
    route
      [ ("/reload", runWebSocketsSnap $ reloader state)
      , ( "/reload.html"
        , serveFile $ Project.project dirs </> "test" </> "reload.html")
      , ( "/reload.js"
        , serveFile $ Project.project dirs </> "test" </> "reload.js")
      , ("/", serveDirectory documentRoot)
      ]

-- Starts a server in a new thread and returns the thread id.
startHttpServer :: ProjectDirs -> Int -> IO Server
startHttpServer dirs port = do
  state <- initState
  threadId <- forkIO $ runHttpServer state dirs port
  return (threadId, state)

reloadClients :: Server -> IO ()
reloadClients = reloadAll . snd

stopHttpServer :: Server -> IO ()
stopHttpServer = killThread . fst

-- connect :: Connection -> IO ()
-- just keep it open
reloader :: MVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    -- putStrLn $ "reloader request from " ++ show cid
    forever (receiveData connection :: IO Text)
