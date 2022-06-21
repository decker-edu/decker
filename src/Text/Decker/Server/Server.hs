{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Server
  ( reloadClients,
    runHttpServer,
    aPort,
    aBind,
  )
where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.FileEmbed
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Network.WebSockets
import Relude
import System.Directory
import System.FilePath.Posix
import System.IO.Streams (connect, withFileAsOutput)
import System.Random
import Text.Decker.Internal.Common
import Text.Decker.Project.ActionContext
import Text.Decker.Resource.Resource
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import Web.Scotty.Trans

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

aPort :: Flags -> Bool
aPort (PortFlag _) = True
aPort _ = False

aBind :: Flags -> Bool
aBind (BindFlag _) = True
aBind _ = False

uploadable = ["-annot.json", "-times.json", "-transcript.json", "-recording.vtt"]

-- Runs the server. Never returns.
runHttpServer :: ActionContext -> IO ()
runHttpServer context = do
  let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort (context ^. extra)
  let BindFlag bind = fromMaybe (BindFlag "localhost") $ find aBind (context ^. extra)
  let state = context ^. server
  let chan = context ^. actionChan
  let server = Server state chan
  scotty port $ do
    head "/" $ headDirectory publicDir
    put "/" $ uploadResource uploadable
    middleware $ staticPolicy (noDots >-> addBase publicDir)

--       route
--         [ ("/reload", runWebSocketsSnap $ reloader state),
--           ("/reload.html", serveFile $ "test" </> "reload.html"),
--           (fromString supportPath, serveSupport context state),
--           ("/", method PUT $ uploadResource uploadable),
--           ("/", method GET $ serveDirectoryNoCaching state publicDir),
--           ("/", method HEAD $ headDirectory publicDir),
--           ("/recordings", method GET listRecordings),
--           ("/replace", method PUT $ uploadRecording context False),
--           ("/append", method PUT $ uploadRecording context True)
--         ]
-- startUpdater state
-- config <- serverConfig port bind
-- simpleHttpServe config routes

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
uploadResource :: [String] -> AppActionM ()
uploadResource suffixes = do
  destination <- requestPathString
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && any (`isSuffixOf` destination) suffixes
    then do
      tmp <- liftIO $ uniqueTransientFileName destination
      runRequestBody (withFileAsOutput tmp . connect)
      liftIO $ renameFile tmp destination
    else modifyResponse $ setResponseStatus 500 "Illegal path suffix"

headDirectory :: FilePath -> AppActionM ()
headDirectory directory = do
  path <- requestPathString
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
serveDirectoryNoCaching :: MVar ServerState -> FilePath -> AppActionM ()
serveDirectoryNoCaching state directory = do
  serveDirectory directory
  modifyResponse $ addHeader "Cache-Control" "no-store"
  path <- getSafePath
  liftIO $ addPage state path

serveSupport :: ActionContext -> MVar ServerState -> AppActionM ()
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

serveResource :: Resources -> FilePath -> AppActionM ()
serveResource (Resources decker pack) path = do
  resource <- liftIO $ firstJustM [readResource path pack, readResource path decker]
  case resource of
    Nothing -> modifyResponse $ setResponseStatus 404 "Resource not found"
    Just content -> do
      modifyResponse $ setHeader "Cache-Control" "no-store"
      modifyResponse $ setContentType (fileType allMimeTypes (takeFileName path))
      writeBS content

-- Accepts a request and adds the connection to the client list. Then reads the
-- connection forever. Removes the client from the list on disconnect.
reloader :: MVar ServerState -> PendingConnection -> IO ()
reloader state pending = do
  connection <- acceptRequest pending
  cid <- randomIO -- Use a random number as client id.
  flip finally (removeClient state cid) $ do
    addClient state (cid, connection)
    forever (receiveData connection :: IO Text.Text)
