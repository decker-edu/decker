{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Text.Decker.Server.Server
  ( startHttpServer
  , stopHttpServer
  , reloadClients
  , Server
  ) where

import Text.Decker.Project.Project
import Text.Decker.Server.Dachdecker (login)

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.UTF8
import Data.Maybe
import Data.Text
import Network.WebSockets
import Network.WebSockets.Snap
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
  let supportPath =
        fromString $ "/" ++ makeRelativeTo (dirs ^. public) (dirs ^. support)
  let documentRoot = dirs ^. public
  devRun <- isDevelopmentRun
  let supportRoot =
        if devRun
          then dirs ^. project </> "resource" </> "support"
          else dirs ^. support
  config <- serverConfig dirs port
  let routes =
        route
          [ ("/reload", runWebSocketsSnap $ reloader state)
          , ("/dachdecker", method POST $ serveDachdecker)
          , ( "/reload.html"
            , serveFile $ dirs ^. project </> "test" </> "reload.html")
          , (supportPath, serveDirectoryNoCaching supportRoot)
          , ("/", method PUT $ saveAnnotation (dirs ^. project))
          , ("/", method GET $ serveDirectoryNoCaching documentRoot)
          ]
  let tryRun port 0 = fail "decker server: All ports already in use"
  let tryRun port tries =
        catch
          (simpleHttpServe (setPort port config) routes)
          (\(SomeException e) -> do
             putStrLn
               ("decker server: Port " ++
                show port ++ "already in use, trying port " ++ show (port + 1))
             tryRun (port + 1) (tries - 1))
  tryRun port 10

-- | Save the request body in the project directory under the request path. But
-- only if the request path ends on "-annot.json" and the local directory
-- already exists. 
saveAnnotation :: MonadSnap m => FilePath -> m ()
saveAnnotation root = do
  path <- getsRequest rqPathInfo
  if BS.isSuffixOf "-annot.json" path
    then do
      let destination = root </> toString path
      body <- readRequestBody 10000000
      exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
      if exists
        then do
          liftIO $ BSL.writeFile destination body
          writeText $ pack ("annotation stored at: " ++ destination)
        else modifyResponse $
             setResponseStatus 500 "Destination directory does not exist"
    else modifyResponse $ setResponseStatus 500 "Illegal path suffix"

serveDirectoryNoCaching :: MonadSnap m => FilePath -> m ()
serveDirectoryNoCaching directory = do
  serveDirectory directory
  modifyResponse $ addHeader "Cache-Control" "no-cache,no-store,must-revalidate"
  modifyResponse $ addHeader "Pragma" "no-cache"
  modifyResponse $ addHeader "Expires" "0"

serveDachdecker :: Snap ()
serveDachdecker = do
  dachdeckerUrl <- liftIO getDachdeckerUrl
  username <- getPostParam "user"
  password <- getPostParam "password"
  maybeToken <-
    if isJust username && isJust password
      then liftIO $
           login (toString $ fromJust username) (toString $ fromJust password)
      else do
        liftIO $ putStrLn "Missing either username or password"
        return Nothing
  case maybeToken of
    Just token ->
      writeText $
      pack
        ("{\"token\": \"" ++
         token ++ "\",\"server\": \"" ++ dachdeckerUrl ++ "\"}")
    Nothing -> liftIO $ putStrLn "Error logging into the Dachdecker server"

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

-- Accepts a request and adds the connection to the client list. Then reads the
-- connection forever. Removes the client from the list on disconnect.
reloader :: MVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO -- Use a random number as client id.
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    forever (receiveData connection :: IO Text)
