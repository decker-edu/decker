{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Shake
  ( runDecker
  , getRelativeSupportDir
  , watchChangesAndRepeat
  , openBrowser
  , startHttpServer
  , stopHttpServer
  , runHttpServer
  , withShakeLock
  , calcSource
  , projectDirsA
  , metaA
  , targetsA
  , decksA
  , decksPdfA
  , pagesA
  , pagesPdfA
  , handoutsA
  , handoutsPdfA
  , allHtmlA
  , allPdfA
  , projectA
  , publicA
  , cacheA
  , supportA
  , appDataA
  , loggingA
  , publicResourceA
  ) where

import Common
import Exception
import Glob
import Meta
import Project
import Server

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson.Lens
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.List
import Data.Maybe
import Data.Text.Lens
import Data.Typeable
import Data.Yaml as Yaml
import Development.Shake
import Development.Shake as Shake
  ( Action
  , Resource
  , Rules
  , ShakeOptions(..)
  , getShakeOptions
  , liftIO
  , newResourceIO
  , shakeArgs
  , shakeOptions
  , withResource
  )
import qualified System.FSNotify as Notify
import System.FilePath
import System.Info
import System.Process

instance Show (IORef a) where
  show _ = "IORef"

data MutableActionState = MutableActionState
  { _server :: IORef (Maybe Server)
  , _watch :: IORef Bool
  , _publicResource :: Shake.Resource
  } deriving (Show)

makeLenses ''MutableActionState

data ActionContext = ActionContext
  { _dirs :: ProjectDirs
  , _targetList :: Targets
  , _meta :: Yaml.Value
  , _state :: MutableActionState
  } deriving (Typeable, Show)

makeLenses ''ActionContext

initMutableActionState = do
  server <- newIORef Nothing
  watch <- newIORef False
  public <- newResourceIO "public" 1
  return $ MutableActionState server watch public

runDecker :: Rules () -> IO ()
runDecker rules = do
  state <- initMutableActionState
  catch (repeatIfTrue $ runShakeOnce state rules) (putError "Terminated: ")
  cleanup state

runShakeOnce :: MutableActionState -> Rules () -> IO Bool
runShakeOnce state rules = do
  context <- initContext state
  options <- deckerShakeOptions context
  catch (shakeArgs options rules) (putError "Error: ")
  server <- readIORef (state ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (state ^. watch)
  when keepWatching $ do
    let exclude = excludeDirs (context ^. meta)
    inDirs <- fastGlobDirs exclude (context ^. dirs . project)
    waitForChange inDirs
  return keepWatching

targetDirs context =
  unique $ map takeDirectory (context ^. targetList . sources)

alwaysExclude = ["public", ".shake", ".git", ".vscode"]

excludeDirs meta =
  let metaExclude =
        meta ^.. key "exclude-directories" . values . _String . unpacked
   in alwaysExclude ++ metaExclude

initContext state = do
  dirs <- projectDirectories
  meta <- readMetaData $ dirs ^. project
  targets <- scanTargets (excludeDirs meta) sourceSuffixes dirs
  return $ ActionContext dirs targets meta state

cleanup state = do
  srvr <- readIORef $ state ^. server
  forM_ srvr stopHttpServer

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  ref <- _watch . _state <$> actionContext
  liftIO $ writeIORef ref True

putError :: String -> SomeException -> IO ()
putError prefix (SomeException e) = putStrLn $ prefix ++ show e

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  cores <- getNumCapabilities
  return $
    shakeOptions
      { shakeFiles = ".shake"
      , shakeColor = True
      , shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty
      , shakeThreads = cores
      -- , shakeLiveFiles = ["shakeLiveFiles.txt"]
      , shakeAbbreviations =
          [ (ctx ^. dirs . project ++ "/", "")
          , (ctx ^. dirs . public ++ "/", "")
          ]
      }

actionContextKey :: TypeRep
actionContextKey = typeOf (undefined :: ActionContext)

actionContext :: Action ActionContext
actionContext = do
  options <- getShakeOptions
  let extra = shakeExtra options
  let dyn =
        fromMaybe
          (error "Error looking up action context")
          (HashMap.lookup actionContextKey extra)
  return $ fromMaybe (error "Error upcasting action context") (fromDynamic dyn)

waitForChange :: [FilePath] -> IO ()
waitForChange inDirs =
  Notify.withManager
    (\manager -> do
       done <- newEmptyMVar
       forM_
         inDirs
         (\dir ->
            Notify.watchDir manager dir (const True) (\e -> putMVar done ()))
       takeMVar done)

getRelativeSupportDir :: FilePath -> Action FilePath
getRelativeSupportDir from = do
  pub <- _public . _dirs <$> actionContext
  let sup = pub </> ("support" ++ "-" ++ deckerVersion)
  return $ makeRelativeTo from sup

publicResourceA = _publicResource . _state <$> actionContext

projectDirsA :: Action ProjectDirs
projectDirsA = _dirs <$> actionContext

metaA :: Action Yaml.Value
metaA = _meta <$> actionContext

projectA :: Action FilePath
projectA = _project <$> projectDirsA

publicA :: Action FilePath
publicA = _public <$> projectDirsA

cacheA :: Action FilePath
cacheA = _cache <$> projectDirsA

supportA :: Action FilePath
supportA = _support <$> projectDirsA

appDataA :: Action FilePath
appDataA = _appData <$> projectDirsA

loggingA :: Action FilePath
loggingA = _logging <$> projectDirsA

targetsA :: Action Targets
targetsA = _targetList <$> actionContext

metaDataA = _meta <$> actionContext

decksA :: Action [FilePath]
decksA = _decks <$> targetsA

decksPdfA :: Action [FilePath]
decksPdfA = _decksPdf <$> targetsA

pagesA :: Action [FilePath]
pagesA = _pages <$> targetsA

pagesPdfA :: Action [FilePath]
pagesPdfA = _pagesPdf <$> targetsA

handoutsA :: Action [FilePath]
handoutsA = _handouts <$> targetsA

handoutsPdfA :: Action [FilePath]
handoutsPdfA = _handoutsPdf <$> targetsA

mapTargets :: [(Targets -> [FilePath])] -> Action [FilePath]
mapTargets fs = do
  ts <- targetsA
  return $ concatMap ($ ts) fs

allHtmlA :: Action [FilePath]
allHtmlA = mapTargets [_decks, _pages, _handouts]

allPdfA :: Action [FilePath]
allPdfA = mapTargets [_decksPdf, _pagesPdf, _handoutsPdf]

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  r <- (_publicResource . _state) <$> actionContext
  withResource r 1 perform

-- Runs the built-in server on the given directory, if it is not already
-- running.
runHttpServer :: Int -> ProjectDirs -> Maybe String -> Action ()
runHttpServer port dirs url = do
  ref <- (_server . _state) <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- liftIO $ startHttpServer dirs port
      liftIO $ writeIORef ref $ Just httpServer
      forM_ url openBrowser

openBrowser :: String -> Action ()
openBrowser url =
  if | any (`isInfixOf` os) ["linux", "bsd"] ->
       liftIO $ callProcess "xdg-open" [url]
     | "darwin" `isInfixOf` os -> liftIO $ callProcess "open" [url]
     | otherwise ->
       putNormal $ "Unable to open browser on this platform for url: " ++ url

reloadBrowsers :: Action ()
reloadBrowsers = do
  ref <- (_server . _state) <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just serv -> liftIO $ reloadClients serv
    Nothing -> return ()

calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target = do
  dirs <- projectDirsA
  let src =
        (replaceSuffix targetSuffix srcSuffix . combine (dirs ^. project) .
         makeRelative (dirs ^. public))
          target
  need [src]
  return src
