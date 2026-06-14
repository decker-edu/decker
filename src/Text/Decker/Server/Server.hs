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

import Control.Concurrent.STM (modifyTVar, newTQueueIO, readTQueue, writeTQueue)
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Data.ByteString.Builder (byteString)
import Data.List (isSuffixOf)
import Data.Maybe
import Network.HTTP.Types
-- import Network.Mime
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Relude
import System.Directory
import System.Directory qualified as Dir
-- import System.FilePath.Posix
import System.FilePath
import System.Random
import Text.Decker.Internal.Common
import Text.Decker.Project.ActionContext
import Text.Decker.Resource.Resource
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import Text.Printf
import Web.Scotty.Trans as Scotty
import Text.Decker.Internal.Helper (uniqueTransientFileName)
import Network.Wai (modifyResponse, mapResponseHeaders)

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

-- addPage :: AppActionM ()
-- addPage = do
--   tvar <- asks serverState
--   page <- requestPathString
--   atomically $ modifyTVar tvar (add page)
--   where
--     add page (ServerState clients pages) =
--       ServerState
--         clients
--         ( if ".html" `isSuffixOf` page
--             then Set.insert page pages
--             else pages
--         )

reloadClients :: TVar ServerState -> IO ()
reloadClients tvar = do
  state <- readTVarIO tvar
  atomically $ forM_ (state ^. clients) $ \(_, q) -> writeTQueue q "reload!"

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
  (Resources deckerSource packSource) <- deckerResources meta
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
  -- Per-process boot id. Sent to clients via SSE event ids; if a client
  -- reconnects with a Last-Event-ID that does not match, the server has been
  -- restarted and we tell the browser to reload.
  bootId <- (show :: Int -> Text) <$> randomIO
  scottyOptsT opts (useState server) $ do
    -- TODO this middleware business is not the right way to do this.
    -- middleware is ecvaluated BEFORE any routes are resolved. so, if
    -- a path can be served statically, it will not be routed. this is
    -- the opposite of what wie want here. we want to serve static files
    -- as a fallback.
    middleware $ modifyResponse (mapResponseHeaders (("Cache-Control", "no-store") :))
    when (context ^. devRun) $ do
      -- first tries the resource pack
      resourceMiddleware "support" packSource
      -- then the decker default resources
      resourceMiddleware "support" deckerSource
    middleware $ staticPolicy (noDots >-> addBase publicDir)
    middleware $ staticPolicy (noDots >-> addBase privateDir)

    Scotty.get "/reload" $ sseReload bootId state
    Scotty.get "/" $ redirect "index.html"
    Scotty.options (regex "^/(.*)$") $ headDirectory publicDir
    -- when (context ^. devRun) $
    -- Scotty.get (regex "^/support/(.*)$") $ serveSupport context
    Scotty.get (regex "^/recordings/(.*)$") listRecordings
    Scotty.put (regex "^/replace/(.*)$") $ uploadRecording False
    Scotty.put (regex "^/append/(.*)$") $ uploadRecording True
    Scotty.put (regex "^/(.*)$") $ uploadResource uploadable

useState state x = runReaderT x state

resourceMiddleware prefix source =
  case source of
    (LocalDir base) -> middleware $ staticPolicy (noDots >-> hasPrefix prefix >-> addBase base)
    _ -> middleware $ nullMiddleware

nullMiddleware app = app

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
  atomically $ forM_ (state ^. clients) $ \(_, q) -> writeTQueue q "ping!"

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
-- serveDirectory :: FilePath -> AppActionM ()
-- serveDirectory directory = do
--   tvar <- asks serverState
--   path <- requestPathString
--   setHeader "Cache-Control" "no-store"
--   addPage
--   file $ directory </> path

-- serveSupport :: ActionContext -> AppActionM ()
-- serveSupport context = do
--   path <- requestPathString
--   putStrLn $ "server support: " <> path
--   let meta = context ^. globalMeta
--   sources <- liftIO $ deckerResources meta
--   serveResource sources ("support" </> path)
--   setHeader "Cache-Control" "no-store"

-- firstJustM :: [IO (Maybe a)] -> IO (Maybe a)
-- firstJustM = foldM (\b a -> do if isNothing b then a else return b) Nothing

-- serveResource :: Resources -> FilePath -> AppActionM ()
-- serveResource (Resources decker pack) path = do
--   resource <- liftIO $ firstJustM [readResource path pack, readResource path decker]
--   liftIO $ putStrLn $ "serving resource: " <> show pack <> ": " <> path
--   case resource of
--     Nothing -> status (Status 404 "Resource not found")
--     Just content -> do
--       setHeader "Content-Type" $ decodeUtf8 $ defaultMimeLookup (toText path)
--       setHeader "Cache-Control" "no-store, no-cache, must-revalidate, max-age=0"
--       setHeader "Pragma:" "no-cache"
--       setHeader "Expires:" "0"
--       raw $ toLazy content

-- | Server-Sent Events endpoint. Registers the client, then streams events
-- pushed to the client's queue as SSE messages until the connection is closed.
-- Browsers using EventSource will auto-reconnect on disconnect. Every event is
-- tagged with the server's boot id; if a reconnecting client sends a
-- Last-Event-ID that does not match, the server has been restarted while the
-- page was open and we push a reload immediately.
sseReload :: Text -> TVar ServerState -> AppActionM ()
sseReload bootId state = do
  Scotty.setHeader "Content-Type" "text/event-stream"
  Scotty.setHeader "Cache-Control" "no-store"
  Scotty.setHeader "Connection" "keep-alive"
  -- Disable proxy buffering (e.g. nginx) so events are flushed immediately.
  Scotty.setHeader "X-Accel-Buffering" "no"
  lastId <- fmap toStrict <$> Scotty.header "Last-Event-ID"
  cid <- liftIO randomIO
  queue <- liftIO newTQueueIO
  liftIO $ addClient state (cid, queue)
  -- A reconnecting client whose Last-Event-ID does not match the current
  -- boot id was talking to a previous server process. Tell it to reload.
  case lastId of
    Just lid | lid /= bootId -> liftIO $ atomically $ writeTQueue queue "reload!"
    _ -> pure ()
  Scotty.stream $ \write flush -> do
    let send bs = write (byteString bs) >> flush
        event msg = "id: " <> bootId <> "\ndata: " <> msg <> "\n\n"
    flip finally (removeClient state cid) $
      handleAll (\_ -> return ()) $ do
        -- Priming event with id sets the browser's lastEventId so reconnects
        -- carry it back to us as Last-Event-ID.
        send (encodeUtf8 (event "hello"))
        forever $ do
          msg <- atomically $ readTQueue queue
          send (encodeUtf8 (event msg))
