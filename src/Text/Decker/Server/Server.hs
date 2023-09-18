{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Server
  ( reloadClients,
    runHttpServer,
    aPort,
    aBind,
  )
where

import Control.Concurrent
-- import Data.List

import Control.Concurrent.STM (modifyTVar)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Network.HTTP.Types
import Network.Mime
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Static
import Network.WebSockets
import Relude
import System.Directory
import System.Directory qualified as Dir
import System.FilePath.Posix
import System.Random
import Text.Decker.Internal.Common
import Text.Decker.Project.ActionContext
import Text.Decker.Resource.Resource
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import Text.Printf
import Web.Scotty.Trans as Scotty

addClient :: TVar ServerState -> Client -> IO ()
addClient tvar client =
  atomically $ modifyTVar tvar add
  where
    add (ServerState clients pages) =
      ServerState (client : clients) pages

removeClient :: TVar ServerState -> Int -> IO ()
removeClient tvar cid =
  atomically $ modifyTVar tvar remove
  where
    remove (ServerState clients pages) =
      ServerState [c | c <- clients, cid /= fst c] pages

addPage :: AppActionM ()
addPage = do
  tvar <- asks serverState
  page <- requestPathString
  atomically $ modifyTVar tvar (add page)
  where
    add page (ServerState clients pages) =
      ServerState
        clients
        ( if ".html" `isSuffixOf` page
            then Set.insert page pages
            else pages
        )

reloadClients :: TVar ServerState -> IO ()
reloadClients tvar = do
  state <- readTVarIO tvar
  mapM_ reload (state ^. clients)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("reload!" :: Text.Text)

aPort :: Flags -> Bool
aPort (PortFlag _) = True
aPort _ = False

aBind :: Flags -> Bool
aBind (BindFlag _) = True
aBind _ = False

uploadable = ["-manip.json", "-annot.json", "-times.json", "-transcript.json", "-recording.vtt", "-poll.json"]

-- Runs the server. Never returns.
runHttpServer :: ActionContext -> IO ()
runHttpServer context = do
  let meta = context ^. globalMeta
  let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort (context ^. extra)
  let BindFlag bind = fromMaybe (BindFlag "localhost") $ find aBind (context ^. extra)
  exists <- liftIO $ Dir.doesFileExist indexSource
  when exists $
    putStrLn $
      printf "Generated index at: http://%s:%d/index-generated.html" bind port
  putStrLn $ printf "Index at: http://%s:%d/index.html\n" bind port
  sources <- liftIO $ deckerResources meta
  putStrLn $ "Loading resources from: " <> show sources
  let state = context ^. server
  let chan = context ^. actionChan
  let server = Server chan state
  let opts = Scotty.Options 0 (setPort port $ setHost (fromString bind) defaultSettings)
  startUpdater state
  scottyOptsT opts (useState server) $ do
    Scotty.get "/" $ redirect "index.html"
    Scotty.options (regex "^/(.*)$") $ headDirectory publicDir
    Scotty.get (fromString supportPath) $ serveSupport context
    Scotty.get (regex "^/recordings/(.*)$") listRecordings
    Scotty.put (regex "^/replace/(.*)$") $ uploadRecording False
    Scotty.put (regex "^/append/(.*)$") $ uploadRecording True
    Scotty.put (regex "^/(.*)$") $ uploadResource uploadable
    middleware $ websocketsOr defaultConnectionOptions $ reloader state
    middleware $ staticPolicy (noDots >-> addBase publicDir)
    middleware $ staticPolicy (noDots >-> addBase privateDir)

useState state x = runReaderT x state

--       route
--         [ ("/reload", runWebSocketsSnap $ reloader state),
--           ("/reload.html", serveFile $ "test" </> "reload.html"),
--         ]
-- startUpdater state
-- config <- serverConfig port bind
-- simpleHttpServe config routes

tenSeconds = 10 * 10 ^ 6

-- |  Sends a ping message to all connected browsers.
pingAll :: TVar ServerState -> IO ()
pingAll tvar = do
  state <- readTVarIO tvar
  mapM_ reload (state ^. clients)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("ping!" :: Text.Text)

-- Safari times out on web sockets to save energy. Prevent this by sending pings
-- from the server to all connected browsers. Once every 10 seconds should do
-- it. This starts a pinger in a separate thread. The thread runs until the
-- server dies.
startUpdater :: TVar ServerState -> IO ()
startUpdater state = do
  forkIO $
    forever $ do
      threadDelay tenSeconds
      pingAll state
  return ()

-- | Save the request body in the project directory under the request path. But
-- only if the request path ends on one of the suffixes and the local directory
-- already exists. Do this atomically.
uploadResource :: [String] -> AppActionM ()
uploadResource suffixes = do
  destination <- param "1"
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && any (`isSuffixOf` destination) suffixes
    then do
      tmp <- liftIO $ uniqueTransientFileName destination
      reader <- bodyReader
      liftIO $ do
        writeBody tmp reader
        renamePath tmp destination
    else do
      text "ERROR: directory does not exist or file (suffix) is not uploadable"
      status status406

headDirectory :: FilePath -> AppActionM ()
headDirectory directory = do
  path <- param "1"
  exists <- liftIO $ doesFileExist (directory </> path)
  if exists
    then status status200
    else status status204

-- serveDirectoryWith config directory
-- where
--   config = defaultDirectoryConfig {preServeHook = \_ -> modifyResponse nukeBody}
--   nukeBody res = res {rspBody = Stream return}

-- | Serves all files in the directory. If it is one of the optional annotation
-- and recording stuff that does not exist (yet), return a "204 No Content"
-- instead of a 404 so that the browser does not need to flag the 404.
serveDirectory :: FilePath -> AppActionM ()
serveDirectory directory = do
  tvar <- asks serverState
  path <- requestPathString
  setHeader "Cache-Control" "no-store"
  addPage
  file $ directory </> path

serveSupport :: ActionContext -> AppActionM ()
serveSupport context =
  if context ^. devRun
    then do
      path <- requestPathString
      let meta = context ^. globalMeta
      sources <- liftIO $ deckerResources meta
      serveResource sources ("support" </> path)
      setHeader "Cache-Control" "no-store"
    else do
      serveDirectory supportDir

firstJustM :: [IO (Maybe a)] -> IO (Maybe a)
firstJustM = foldM (\b a -> do if isNothing b then a else return b) Nothing

serveResource :: Resources -> FilePath -> AppActionM ()
serveResource (Resources decker pack) path = do
  resource <- liftIO $ firstJustM [readResource path pack, readResource path decker]
  case resource of
    Nothing -> status (Status 404 "Resource not found")
    Just content -> do
      setHeader "Cache-Control" "no-store"
      setHeader "Content-Type" $ decodeUtf8 $ defaultMimeLookup (toText path)
      raw $ toLazy content

-- Accepts a request and adds the connection to the client list. Then reads the
-- connection forever. Removes the client from the list on disconnect.
reloader :: TVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO -- Use a random number as client id.
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    handleAll (\_ -> return ()) $
      forever (receiveData connection :: IO Text)
