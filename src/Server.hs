{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Server
  ( startHttpServer
  , stopHttpServer
  , reloadClients
  , Server
  ) where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Text
import Network.WebSockets
import Network.WebSockets.Snap
import Project
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.Directory
import System.FilePath
import System.Random

-- Logging and port configuration for the server.
serverConfig :: ProjectDirs -> Int -> IO (Config Snap a)
serverConfig dirs port = do
  let logDir = dirs ^. logging
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
reloadAll state = withMVar state $ mapM_ reload
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("reload!" :: Text)

-- Runs the server. Never returns.
runHttpServer :: MVar ServerState -> ProjectDirs -> Int -> IO ()
runHttpServer state dirs port = do
  let documentRoot = dirs ^. public
  config <- serverConfig dirs port
  handle (\(SomeException e) -> print e) $
    simpleHttpServe config $
    route
      [ ("/reload", runWebSocketsSnap $ reloader state)
      , ( "/reload.html" -- Just for testing the thing.
        , serveFile $ dirs ^. project </> "test" </> "reload.html")
      , ("/", serveDirectoryNoCaching documentRoot)
      ]

serveDirectoryNoCaching :: MonadSnap m => FilePath -> m ()
serveDirectoryNoCaching directory = do
  serveDirectory directory
  modifyResponse $ addHeader "Cache-Control" "no-cache,no-store,must-revalidate"
  modifyResponse $ addHeader "Pragma" "no-cache"
  modifyResponse $ addHeader "Expires" "0"

-- | Starts a server in a new thread and returns the thread id.
startHttpServer :: ProjectDirs -> Int -> IO Server
startHttpServer dirs port = do
  state <- initState
  threadId <- forkIO $ runHttpServer state dirs port
  return (threadId, state)

-- | Sends a reload messsage to all attached clients
reloadClients :: Server -> IO ()
reloadClients = reloadAll . snd

-- | Kills the server.
stopHttpServer :: Server -> IO ()
stopHttpServer = killThread . fst

-- Accepts a request and adds the connection to the client list. Then reads
-- reads the connection forever. Removes the client from the list on disconnect.
reloader :: MVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO -- Use a random number as client id.
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    forever (receiveData connection :: IO Text)
