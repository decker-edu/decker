{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Decker.Server.Server
  ( startHttpServer,
    stopHttpServer,
    reloadClients,
    Server,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Data.FileEmbed
import Data.List
import qualified Data.Set as Set
import qualified Data.Text as Text
import Network.WebSockets
import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server
import Snap.Internal.Core (ResponseBody (Stream), rspBody)
import Snap.Util.FileServe
import System.Directory
import System.FilePath.Posix
import System.IO.Streams (connect)
import System.IO.Streams.File (withFileAsOutput)
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
    ( setVerbose True $
        -- setBind "localhost" $
          setPort port $
            --setSSLBind "localhost" $
            --setSSLPort (port + 13) $
            --setSSLCert (transientDir </> "decker-ssl.crt") $
            --setSSLKey (transientDir </> "decker-ssl.key") $
            --setSSLChainCert False $
            setAccessLog (ConfigFileLog accessLog) $
              setErrorLog (ConfigFileLog errorLog) defaultConfig ::
        Config Snap a
    )

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
        ( clients,
          if ".html" `isSuffixOf` page
            then Set.insert page pages
            else pages
        )

reloadAll :: MVar ServerState -> IO ()
reloadAll state = withMVar state (mapM_ reload . fst)
  where
    reload :: Client -> IO ()
    reload (_, conn) = sendTextData conn ("reload!" :: Text.Text)

sslCert = $(embedFile "tls/decker-ssl.crt")

sslKey = $(embedFile "tls/decker-ssl.key")

installSSLCert :: IO ()
installSSLCert = do
  BS.writeFile (transientDir </> "decker-ssl.crt") sslCert
  BS.writeFile (transientDir </> "decker-ssl.key") sslKey

-- Runs the server. Never returns.
runHttpServer :: MVar ServerState -> Int -> IO ()
runHttpServer state port = do
  installSSLCert
  devRun <- isDevelopmentRun
  let supportRoot =
        if devRun
          then devSupportDir
          else supportDir
  config <- serverConfig port
  let routes =
        route
          [ ("/reload", runWebSocketsSnap $ reloader state),
            ("/reload.html", serveFile $ "test" </> "reload.html"),
            (fromString supportPath, serveDirectoryNoCaching state supportRoot),
            ("/", method PUT $ uploadResource ["-annot.json", "-times.json", "-recording.mp4", "-recording.webm"]),
            ("/", method GET $ serveDirectoryNoCaching state publicDir),
            ("/", method HEAD $ headDirectory publicDir)
          ]
  startUpdater state
  catch
    (simpleHttpServe config routes)
    ( \(SomeException e) -> do
        putStrLn $ "HTTP server not started on port " <> show port
        putStrLn $ "  " <> show e
    )

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
-- already exists.
uploadResource :: MonadSnap m => [String] -> m ()
uploadResource suffixes = do
  destination <- toString <$> getsRequest rqPathInfo
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && any (`isSuffixOf` destination) suffixes
    then runRequestBody (withFileAsOutput destination . connect)
    else modifyResponse $ setResponseStatus 500 "Illegal path suffix"

headDirectory :: MonadSnap m => FilePath -> m ()
headDirectory directory = do
  serveDirectoryWith config directory
  where
    config = defaultDirectoryConfig {preServeHook = \_ -> modifyResponse nukeBody}
    nukeBody res = res {rspBody = Stream return}

serveDirectoryNoCaching :: MonadSnap m => MVar ServerState -> FilePath -> m ()
serveDirectoryNoCaching state directory = do
  serveDirectory directory
  modifyResponse $ addHeader "Cache-Control" "no-cache,no-store,must-revalidate"
  modifyResponse $ addHeader "Pragma" "no-cache"
  modifyResponse $ addHeader "Expires" "0"
  path <- getsRequest rqPathInfo
  liftIO $ addPage state (toString path)

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
