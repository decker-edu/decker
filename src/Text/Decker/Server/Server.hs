{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Server.Server
  ( -- startHttpServer,
    -- stopHttpServer,
    -- startServerForeground,
    reloadClients,
    runHttpServer,
    aPort,
    aBind
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Data.FileEmbed
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Network.WebSockets
import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath.Posix
import System.IO.Streams (connect)
import System.IO.Streams.File (withFileAsOutput)
import System.Random
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Decker.Project.ActionContext
import Text.Decker.Resource.Resource
import Text.Decker.Server.Types

-- Logging and port configuration for the server.
serverConfig :: Int -> String -> IO (Config Snap a)
serverConfig port bind = do
  let accessLog = transientDir </> "server-access.log"
  let errorLog = transientDir </> "server-error.log"
  createDirectoryIfMissing True transientDir
  return
    ( setVerbose True $
        setBind (fromString bind) $
          setPort port $
            setAccessLog (ConfigFileLog accessLog) $
              setErrorLog (ConfigFileLog errorLog) defaultConfig ::
        Config Snap a
    )

-- initState :: IO (MVar ServerState)
-- initState = newMVar ([], Set.fromList ["index.html"])

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
        ( clients,
          if ".html" `isSuffixOf` page
            then Set.insert page pages
            else pages
        )

reloadClients :: MVar ServerState -> IO ()
reloadClients server = withMVar server (mapM_ reload . fst)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("reload!" :: Text.Text)

sslCert = $(embedFile "tls/decker-ssl.crt")

sslKey = $(embedFile "tls/decker-ssl.key")

installSSLCert :: IO ()
installSSLCert = do
  BS.writeFile (transientDir </> "decker-ssl.crt") sslCert
  BS.writeFile (transientDir </> "decker-ssl.key") sslKey

aPort :: Flags -> Bool
aPort (PortFlag _) = True
aPort _ = False

aBind :: Flags -> Bool
aBind (BindFlag _) = True
aBind _ = False

-- Runs the server. Never returns.
runHttpServer :: ActionContext -> IO ()
runHttpServer context = do
  let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort (context ^. extra)
  let BindFlag bind = fromMaybe (BindFlag "localhost") $ find aBind (context ^. extra)
  installSSLCert
  let state = context ^. server
  let routes =
        route
          [ ("/reload", runWebSocketsSnap $ reloader state),
            ("/reload.html", serveFile $ "test" </> "reload.html"),
            (fromString supportPath, serveSupport context state),
            ("/", method PUT $ uploadResource ["-annot.json", "-times.json", "-recording.mp4", "-recording.webm"]),
            ("/", method GET $ serveDirectoryNoCaching state publicDir),
            ("/", method HEAD $ headDirectory publicDir),
            ("/upload", method POST $ uploadFiles ["-annot.json", "-times.json", "-recording.mp4", "-recording.webm"])
          ]
  startUpdater state
  config <- serverConfig port bind
  simpleHttpServe config routes

tenSeconds = 10 * 10 ^ 6

-- |  Sends a ping message to all connected browsers.
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
-- only if the request path ends on one of the suffixes and the local directory
-- already exists. Do this atomically.
uploadResource :: MonadSnap m => [String] -> m ()
uploadResource suffixes = do
  destination <- toString <$> getsRequest rqPathInfo
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && any (`isSuffixOf` destination) suffixes
    then do
      let tmp = transientDir </> takeFileName destination
      runRequestBody (withFileAsOutput tmp . connect)
      liftIO $ renameFile tmp destination
    else modifyResponse $ setResponseStatus 500 "Illegal path suffix"

fileUploadPolicy = setMaximumFileSize (10 ^ 9) defaultFileUploadPolicy

-- | Expects a multi-part file upload.
uploadFiles :: MonadSnap m => [String] -> m ()
uploadFiles suffixes = do
  withTemporaryStore transientDir "upload-" $ \store -> do
    (inputs, files) <-
      handleFormUploads defaultUploadPolicy fileUploadPolicy (const store)
    liftIO $
      forM_ files $
        \(FormFile path tmp) ->
          catch
            ( do
                let destination = dropDrive $ toString path
                exists <- doesDirectoryExist (takeDirectory destination)
                if exists && any (`isSuffixOf` destination) suffixes
                  then do
                    renameFile tmp destination
                    putStrLn $ "# upload received: " <> destination
                  else throwM $ InternalException "Illegal upload path suffix"
            )
            ( \e@(SomeException se) -> do
                putStrLn $ "# upload FAILED: " <> show se
                throwM e
            )
  return ()

headDirectory :: MonadSnap m => FilePath -> m ()
headDirectory directory = do
  path <- getSafePath
  exists <- liftIO $ doesFileExist (directory </> path)
  if
      | not exists && path `endsOn` ["-annot.json", "-times.json", "-recording.mp4", "-recording.vtt"] ->
        finishWith $ setResponseCode 204 emptyResponse
      | not exists -> finishWith $ setResponseCode 404 emptyResponse
      | otherwise -> finishWith $ setResponseCode 200 emptyResponse

-- serveDirectoryWith config directory
-- where
--   config = defaultDirectoryConfig {preServeHook = \_ -> modifyResponse nukeBody}
--   nukeBody res = res {rspBody = Stream return}

-- | Serves all files in the directory. If it is one of the optional annotation
-- and recording stuff that does ot exist (yet), return a "204 No Content"
-- instead of a 404 so that the browser does not need to flag the 404.
serveDirectoryNoCaching :: MonadSnap m => MVar ServerState -> FilePath -> m ()
serveDirectoryNoCaching state directory = do
  path <- getSafePath
  exists <- liftIO $ doesFileExist (directory </> path)
  if not exists && path `endsOn` ["-annot.json", "-times.json", "-recording.mp4", "-recording.vtt"]
    then finishWith $ setResponseCode 204 emptyResponse
    else do
      serveDirectory directory
      modifyResponse $ addHeader "Cache-Control" "no-store"
      liftIO $ addPage state path

endsOn string = any (`isSuffixOf` string)

serveSupport :: (MonadSnap m) => ActionContext -> MVar ServerState -> m ()
serveSupport context state =
  if context ^. devRun
    then do
      path <- getSafePath
      let meta = context ^. globalMeta
      sources <- liftIO $ deckerResources meta
      serveResource sources ("support" </> path)
      modifyResponse $ addHeader "Cache-Control" "no-store"
    else do
      serveDirectoryNoCaching state supportDir

firstJustM :: [IO (Maybe a)] -> IO (Maybe a)
firstJustM = foldM (\b a -> do if isNothing b then a else return b) Nothing

serveResource :: (MonadSnap m) => Resources -> FilePath -> m ()
serveResource (Resources decker pack) path = do
  resource <- liftIO $ firstJustM [readResource path pack, readResource path decker]
  case resource of
    Nothing -> modifyResponse $ setResponseStatus 404 "Resource not found"
    Just content -> do
      modifyResponse $ setHeader "Cache-Control" "no-store"
      modifyResponse $ setContentType (fileType defaultMimeTypes (takeFileName path))
      writeBS content

-- -- | Starts the decker server. Never returns.
-- startServerForeground :: ActionContext -> Int -> String -> IO ()
-- startServerForeground context port bind = do
--   state <- initState
--   runHttpServer context state port bind

-- -- | Starts a server in a new thread and returns the thread id.
-- startHttpServer :: ActionContext -> Int -> String -> IO Server
-- startHttpServer context port bind = do
--   state <- initState
--   channel <- atomically newTChan
--   threadId <- forkIO $ do
--     catchAll
--       (runHttpServer context state port bind)
--       ( \(SomeException e) -> do
--           putStrLn $ "HTTP could not be started on port " <> show port
--           atomically $ writeTChan (context ^. actionChan) (PortInUse port)
--       )
--   return $ Server threadId state

-- -- | Kills the server.
-- stopHttpServer :: Server -> IO ()
-- stopHttpServer = killThread . _threadId

-- Accepts a request and adds the connection to the client list. Then reads the
-- connection forever. Removes the client from the list on disconnect.
reloader :: MVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO -- Use a random number as client id.
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    forever (receiveData connection :: IO Text.Text)
