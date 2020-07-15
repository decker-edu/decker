{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Server.Server
  ( startHttpServer
  , stopHttpServer
  , reloadClients
  , Server
  ) where

-- TODO is this still used?
-- import Text.Decker.Server.Dachdecker (login)
import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.UTF8
import Data.List
import qualified Data.Set as Set
import qualified Data.Text as Text

import Network.WebSockets
import Network.WebSockets.Snap

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

import System.Directory
import System.FilePath.Posix
import System.Random

import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper

-- Logging and port configuration for the server.
serverConfig :: Int -> IO (Config Snap a)
serverConfig port = do
  let accessLog = transientDir </> "server-access.log"
  let errorLog = transientDir </> "server-error.log"
  createDirectoryIfMissing True transientDir
  return
    (setPort port $
     setAccessLog (ConfigFileLog accessLog) $
     setErrorLog (ConfigFileLog errorLog) defaultConfig :: Config Snap a)

-- | Clients are identified by integer ids
type Client = (Int, Connection)

type ServerState = ([Client], Set.Set FilePath)

type Server = (ThreadId, MVar ServerState)

initState :: IO (MVar ServerState)
initState = newMVar ([], Set.fromList ["index.html"])

addClient :: MVar ServerState -> Client -> IO ()
addClient state client = modifyMVar_ state add
  where
    add (clients, pages) = return (client : clients, pages)

removeClient :: MVar ServerState -> Int -> IO ()
removeClient state cid = modifyMVar_ state remove
  where
    remove (clients, pages) = return ([c | c <- clients, cid /= fst c], pages)

addPage :: MVar ServerState -> FilePath -> IO ()
addPage state page = modifyMVar_ state add
  where
    add (clients, pages) =
      return
        ( clients
        , if ".html" `isSuffixOf` page
            then Set.insert page pages
            else pages)

reloadAll :: MVar ServerState -> IO ()
reloadAll state = withMVar state (mapM_ reload . fst)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("reload!" :: Text.Text)

-- Runs the server. Never returns.
runHttpServer :: MVar ServerState -> Int -> IO ()
runHttpServer state port = do
  devRun <- isDevelopmentRun
  let supportRoot =
        if devRun
          then devSupportDir
          else supportDir
  config <- serverConfig port
  let routes =
        route
          [ ("/reload", runWebSocketsSnap $ reloader state)
                 -- , ("/dachdecker", method POST $ serveDachdecker)
          , ( "/reload.html"
            , serveFile $ "test" </> "reload.html")
          , (fromString supportPath, serveDirectoryNoCaching state supportRoot)
          , ("/", method PUT $ saveAnnotation)
          , ("/", method GET $ serveDirectoryNoCaching state publicDir)
          ]
  let tryRun port 0 = fail "decker server: All ports already in use"
      tryRun port tries =
        catchAll
          (simpleHttpServe (setPort port config) routes)
          (\_ -> do
             putStrLn
               ("decker server: Port " ++
                show port ++ "already in use, trying port " ++ show (port + 1))
             tryRun (port + 1) (tries - 1))
  startUpdater state
  tryRun port 10

tenSeconds = 10 * 10 ^ 6

-- | Sends a ping message to all connected browsers.
pingAll :: MVar ServerState -> IO ()
pingAll state = withMVar state (mapM_ reload . fst)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("ping!" :: Text.Text)

-- Safari times out on web sockets to save energy. Prevent this by sending pings
-- from the server to all connected browsers. Once every 10 seconds should do
-- it. This starts a pinger in a separate thread. The thread runs until the
-- server dies.
startUpdater :: MVar ServerState -> IO ()
startUpdater state = do
  forkIO $
    forever $ do
      threadDelay tenSeconds
      pingAll state
  return ()

-- | Save the request body in the project directory under the request path. But
-- only if the request path ends on "-annot.json" and the local directory
-- already exists. 
saveAnnotation :: MonadSnap m => m ()
saveAnnotation = do
  path <- getsRequest rqPathInfo
  liftIO $ putStrLn $ "saving: " <> toString path
  if BS.isSuffixOf "-annot.json" path ||
     BS.isSuffixOf "-annot.png" path || BS.isSuffixOf "-annot.pkdrawing" path
    then do
      let destination = toString path
      body <- readRequestBody 10000000
      exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
      if exists
        then do
          liftIO $ BSL.writeFile destination body
          writeText $ Text.pack ("annotation stored at: " ++ destination)
        else modifyResponse $
             setResponseStatus 500 "Destination directory does not exist"
    else modifyResponse $ setResponseStatus 500 "Illegal path suffix"

serveDirectoryNoCaching :: MonadSnap m => MVar ServerState -> FilePath -> m ()
serveDirectoryNoCaching state directory = do
  serveDirectory directory
  modifyResponse $ addHeader "Cache-Control" "no-cache,no-store,must-revalidate"
  modifyResponse $ addHeader "Pragma" "no-cache"
  modifyResponse $ addHeader "Expires" "0"
  path <- getsRequest rqPathInfo
  liftIO $ addPage state (toString path)

-- TODO is this still used?
{-
 -serveDachdecker :: Snap ()
 -serveDachdecker = do
 -  dachdeckerUrl <- liftIO getDachdeckerUrl
 -  username <- getPostParam "user"
 -  password <- getPostParam "password"
 -  maybeToken <-
 -    if isJust username && isJust password
 -      then liftIO $
 -           login (toString $ fromJust username) (toString $ fromJust password)
 -      else do
 -        liftIO $ putStrLn "Missing either username or password"
 -        return Nothing
 -  case maybeToken of
 -    Just token ->
 -      writeText $
 -      Text.pack
 -        ("{\"token\": \"" ++
 -         token ++ "\",\"server\": \"" ++ dachdeckerUrl ++ "\"}")
 -    Nothing -> liftIO $ putStrLn "Error logging into the Dachdecker server"
 -}
-- | Starts a server in a new thread and returns the thread id.
startHttpServer :: Int -> IO Server
startHttpServer port = do
  state <- initState
  threadId <- forkIO $ runHttpServer state port
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
    forever (receiveData connection :: IO Text.Text)
