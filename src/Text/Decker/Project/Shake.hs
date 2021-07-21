{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.Shake
  ( runDecker,
    calcSource,
    calcSource',
    currentlyServedPages,
    relativeSupportDir,
    isDevRun,
    openBrowser,
    publicResource,
    publicResourceA,
    putCurrentDocument,
    runHttpServer,
    startHttpServer,
    stopHttpServer,
    watchChangesAndRepeat,
    withShakeLock,
    writeSupportFilesToPublic,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception
-- import Data.List

-- import qualified Data.Text as Text

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.Char
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.Extra as List
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake hiding (doesDirectoryExist, putError)
import Relude hiding (state)
import qualified System.Console.GetOpt as GetOpt
import System.Directory as Dir
import qualified System.FSNotify as Notify
import System.FilePath.Posix
import System.Info
import System.Process
import Text.Decker.Internal.Common
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Project.ActionContext
import Text.Decker.Project.Project
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Resource.Template
import Text.Decker.Server.Server
import Text.Pandoc hiding (lookupMeta)

initMutableActionState = do
  devRun <- isDevelopmentRun
  external <- checkExternalPrograms
  server <- newIORef Nothing
  watch <- newIORef False
  public <- newResourceIO "public" 1
  return $ MutableActionState devRun external server watch public

runDecker :: Rules () -> IO ()
runDecker rules = do
  state <- initMutableActionState
  catchAll (repeatIfTrue $ runShakeOnce state rules) (putError "Terminated: ")
  cleanup state

data Flags
  = MetaValueFlag
      String
      String
  | FormatFlag
  | CleanFlag
  | CleanerFlag
  | WatchFlag
  | ServerFlag
  | PortFlag Int
  | BindFlag String
  deriving (Eq, Show)

deckerFlags :: [GetOpt.OptDescr (Either String Flags)]
deckerFlags =
  [ GetOpt.Option
      ['m']
      ["meta"]
      (GetOpt.ReqArg parseMetaValueArg "META")
      "Set a meta value like this: 'name=value'. Overrides values from decker.yaml.",
    GetOpt.Option
      ['w']
      ["watch"]
      (GetOpt.NoArg $ Right WatchFlag)
      "Watch changes to source files and rebuild current target if necessary",
    GetOpt.Option
      ['s']
      ["server"]
      (GetOpt.NoArg $ Right ServerFlag)
      "Serve the public dir via HTTP (implies --watch)",
    GetOpt.Option
      ['p']
      ["port"]
      (GetOpt.ReqArg parsePortArg "PORT")
      "The HTTP server listens on the given port.",
    GetOpt.Option
      ['b']
      ["bind"]
      (GetOpt.ReqArg (Right . BindFlag) "BIND")
      "Bind the HTTP server to given address."
  ]

parsePortArg :: String -> Either String Flags
parsePortArg arg =
  case readMaybe arg of
    Just port | port > 2 ^ 10 && port <= 2 ^ 16 -> Right $ PortFlag port
    _ -> Left "Cannot parse argument. Must be an integer from (2^10, 2^16]."

parseMetaValueArg :: String -> Either String Flags
parseMetaValueArg arg =
  case List.splitOn "=" arg of
    [meta, value]
      | isMetaName meta -> Right $ MetaValueFlag meta value
    _ -> Left "Cannot parse argument. Must be 'name=value'."

isMetaName :: String -> Bool
isMetaName str = all check $ List.splitOn "." str
  where
    check s = length s > 1 && isAlpha (List.head s) && all (\c -> isAlphaNum c || isSymbol c || isPunctuation c) (List.tail s)

handleArguments :: MutableActionState -> Rules () -> [Flags] -> [String] -> IO (Maybe (Rules ()))
handleArguments state rules flags targets = do
  extractMeta flags
  if
      | "clean" `elem` targets -> do
        runClean False
        return Nothing
      | "cleaner" `elem` targets -> do
        runClean True
        return Nothing
      | "example" `elem` targets -> do
        writeExampleProject
        return Nothing
      | otherwise -> do
        let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort flags
        let BindFlag bind = fromMaybe (BindFlag "localhost") $ find aBind flags
        when (WatchFlag `elem` flags) $ do
          watchChangesAndRepeatIO state
        when (ServerFlag `elem` flags) $ do
          watchChangesAndRepeatIO state
          runHttpServerIO state port bind
        buildRules targets rules

-- | Saves the meta flags to a well known file. Will be later read and cached by
-- shake.
extractMeta :: [Flags] -> IO ()
extractMeta flags = do
  let metaFlags = HashMap.fromList $ map (\(MetaValueFlag k v) -> (k, v)) $ filter aMetaValue flags
  -- let metaFlags =
  --       foldl' (\meta (MetaValueFlag k v) -> setMetaValue (Text.pack k) v meta) nullMeta $
  --         filter aMetaValue flags
  let json = decodeUtf8 $ encode metaFlags
  writeFileChanged metaArgsFile json

buildRules :: [FilePath] -> Rules () -> IO (Maybe (Rules ()))
buildRules targets rules =
  if null targets
    then return $ Just rules
    else return $
      Just $ do
        want (filter (`notElem` ["clean", "cleaner", "example"]) targets)
        withoutActions rules

aPort :: Flags -> Bool
aPort (PortFlag _) = True
aPort _ = False

aBind :: Flags -> Bool
aBind (BindFlag _) = True
aBind _ = False

aMetaValue (MetaValueFlag _ _) = True
aMetaValue _ = False

runShakeOnce :: MutableActionState -> Rules () -> IO Bool
runShakeOnce state rules = do
  context <- initContext state
  options <- deckerShakeOptions context
  catchAll
    (shakeArgsWith options deckerFlags (handleArguments state rules))
    (putError "ERROR:\n")
  server <- readIORef (state ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (state ^. watch)
  when keepWatching $ do
    meta <- readMetaDataFile globalMetaFileName
    exclude <- mapM canonicalizePath (excludeDirs meta)
    waitForChange projectDir exclude
  return keepWatching

initContext :: MutableActionState -> IO ActionContext
initContext state = do
  createDirectoryIfMissing True transientDir
  return $ ActionContext state

cleanup state = do
  srvr <- readIORef $ state ^. server
  forM_ srvr stopHttpServer

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  state <- _state <$> actionContext
  liftIO $ watchChangesAndRepeatIO state

watchChangesAndRepeatIO :: MutableActionState -> IO ()
watchChangesAndRepeatIO state = do
  let ref = _watch state
  liftIO $ writeIORef ref True

putError :: Text -> SomeException -> IO ()
putError prefix (SomeException e) =
  print $ prefix <> unlines (lastN 2 $ lines $ show e)

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  cores <- getNumCapabilities
  return $
    shakeOptions
      { shakeFiles = transientDir,
        shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty,
        shakeThreads = cores,
        -- , shakeStaunch = True
        shakeColor = True,
        shakeChange = ChangeModtime
        -- , shakeChange = ChangeModtimeAndDigest
        -- shakeLint = Just LintBasic,
        -- , shakeLint = Just LintFSATrace
        -- shakeReport = [".decker/shake-report.html"]
      }

waitForChange :: FilePath -> [FilePath] -> IO ()
waitForChange inDir exclude =
  Notify.withManager
    ( \manager -> do
        done <- newEmptyMVar
        Notify.watchTree manager inDir filter (\e -> putMVar done ())
        takeMVar done
    )
  where
    filter event = not $ any (`isPrefixOf` Notify.eventPath event) exclude

isDevRun :: Action Bool
isDevRun = do
  context <- actionContext
  return (context ^. state . devRun)

relativeSupportDir :: FilePath -> FilePath
relativeSupportDir from = makeRelativeTo from supportDir

writeSupportFilesToPublic :: Meta -> Action ()
writeSupportFilesToPublic meta = do
  templateSource <- liftIO $ calcTemplateSource meta
  correct <- correctSupportInstalled templateSource
  if correct
    then putNormal "# support files up to date"
    else do
      putNormal $ "# copy support files from: " <> show templateSource
      removeSupport
      extractSupport templateSource

supportId templateSource = show (templateSource, deckerGitCommitId)

extractSupport :: TemplateSource -> Action ()
extractSupport templateSource = do
  context <- actionContext
  liftIO $
    handleAll (\_ -> return ()) $ do
      copySupportFiles templateSource Copy supportDir
      writeFile (supportDir </> ".origin") $ supportId templateSource

correctSupportInstalled :: TemplateSource -> Action Bool
correctSupportInstalled templateSource = do
  context <- actionContext
  liftIO $
    handleAll (\_ -> return False) $ do
      installed <- readFile (supportDir </> ".origin")
      return (installed == supportId templateSource)

removeSupport :: Action ()
removeSupport = do
  context <- actionContext
  liftIO $ handleAll (\_ -> return ()) $ removeDirectoryRecursive supportDir

publicResourceA = _publicResource . _state <$> actionContext

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  r <- _publicResource . _state <$> actionContext
  withResource r 1 perform

runHttpServer :: Int -> Maybe String -> Action ()
runHttpServer port url = do
  state <- _state <$> actionContext
  let ref = _server state
  server <- liftIO $ readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- liftIO $ startHttpServer port "localhost"
      liftIO $ writeIORef ref $ Just httpServer
      forM_ url openBrowser

-- |  Runs the built-in server on the given directory, if it is not already
--  running.
runHttpServerIO :: MutableActionState -> Int -> String -> IO ()
runHttpServerIO state port bind = do
  let ref = _server state
  server <- readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- startHttpServer port bind
      writeIORef ref $ Just httpServer

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  ref <- _server . _state <$> actionContext
  server <- liftIO $ readIORef ref
  case server of
    Just (_, state) -> do
      (_, pages) <- liftIO $ readMVar state
      return $ Set.toList pages
    Nothing -> return []

openBrowser :: String -> Action ()
openBrowser url =
  if
      | any (`List.isInfixOf` os) ["linux", "bsd"] ->
        liftIO $ callProcess "xdg-open" [url]
      | "darwin" `List.isInfixOf` os -> liftIO $ callProcess "open" [url]
      | otherwise ->
        putNormal $ "Unable to open browser on this platform for url: " ++ url

calcSource :: String -> String -> FilePath -> Action FilePath
calcSource targetSuffix srcSuffix target = do
  let src =
        (replaceSuffix targetSuffix srcSuffix . makeRelative publicDir) target
  need [src]
  return src

-- |  calcSource without the call to need and without the suffix replacement
calcSource' :: FilePath -> Action FilePath
calcSource' target = do
  return $ makeRelative publicDir target

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = putNormal $ "# pandoc (for " ++ out ++ ")"

-- | Functionality for "decker clean" command Removes public and .decker
-- directory. Located outside of Shake due to unlinking differences and
-- parallel processes on Windows which prevented files (.shake.lock) from being
-- deleted on Windows.
runClean :: Bool -> IO ()
runClean totally = do
  warnVersion
  putStrLn $ "# Removing " ++ publicDir
  tryRemoveDirectory publicDir
  when totally $
    do
      putStrLn $ "# Removing " ++ transientDir
      tryRemoveDirectory transientDir
