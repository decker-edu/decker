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
    putCurrentDocument,
    watchChangesAndRepeat,
    withShakeLock,
    runHttpServerIO,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception
-- import Data.List

-- import qualified Data.Text as Text

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Data.Aeson hiding (Error)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
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
import System.Environment
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
import Text.Decker.Resource.Resource
import Text.Decker.Server.Server
import Text.Pandoc hiding (Verbosity)
import Text.Pretty.Simple

runDecker :: [Flags] -> Rules () -> IO ()
runDecker extra rules = do
  context <- initContext extra
  catchAll (repeatIfTrue $ runShakeOnce context rules) (putError "Terminated: ")
  cleanup context

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
      ['e']
      ["stderr"]
      (GetOpt.NoArg $ Right ErrorFlag)
      "Output errors to stderr",
    GetOpt.Option
      ['o']
      ["open"]
      (GetOpt.NoArg $ Right OpenFlag)
      "Open the webbrowser.",
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

handleArguments :: ActionContext -> Rules () -> [Flags] -> [String] -> IO (Maybe (Rules ()))
handleArguments context rules flags targets = do
  let allFlags = flags <> context ^. extra
  extractMeta allFlags
  let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort allFlags
  let BindFlag bind = fromMaybe (BindFlag "localhost") $ find aBind allFlags
  if
      | "serve" `elem` targets -> do
        startServerForeground context port bind
        return Nothing
      | "test" `elem` targets -> do
        meta <- readMetaDataFile globalMetaFileName
        publicSupportFiles meta >>= pPrint
        deckerResources meta >>= pPrint
        return Nothing
      | "clean" `elem` targets -> do
        runClean True
        return Nothing
      | "example" `elem` targets -> do
        writeExampleProject
        return Nothing
      | otherwise -> do
        when (WatchFlag `elem` allFlags) $ do
          watchChangesAndRepeatIO context
        when (ServerFlag `elem` allFlags) $ do
          watchChangesAndRepeatIO context
          runHttpServerIO context port bind
        when (OpenFlag `elem` allFlags) $ do
          openBrowser $ "http://localhost:" <> show port <> "/index.html"
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

runShakeOnce :: ActionContext -> Rules () -> IO Bool
runShakeOnce context rules = do
  -- reread the meta data on every run and update the action context
  meta <- readMetaDataFile globalMetaFileName
  meta' <- atomicModifyIORef' (context ^. globalMeta) $ const (meta, meta)
  options <- deckerShakeOptions context
  catchAll
    (shakeArgsWith options deckerFlags (handleArguments context rules))
    (const $ return ())
  server <- readIORef (context ^. server)
  forM_ server reloadClients
  keepWatching <- readIORef (context ^. watch)
  when keepWatching $
    do
      exclude <- mapM canonicalizePath $ excludeDirs meta'
      waitForChange projectDir exclude
  return keepWatching

initContext :: [Flags] -> IO ActionContext
initContext extra = do
  createDirectoryIfMissing True transientDir
  devRun <- isDevelopmentRun
  external <- checkExternalPrograms
  server <- newIORef Nothing
  watch <- newIORef False
  public <- newResourceIO "public" 1
  ref <- newIORef nullMeta
  return $ ActionContext extra devRun external server watch public ref

cleanup context = do
  srvr <- readIORef $ context ^. server
  forM_ srvr stopHttpServer

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  context <- actionContext
  liftIO $ watchChangesAndRepeatIO context

watchChangesAndRepeatIO :: ActionContext -> IO ()
watchChangesAndRepeatIO context = do
  let ref = context ^. watch
  liftIO $ writeIORef ref True

putError :: Text -> SomeException -> IO ()
putError prefix (SomeException e) =
  print $ prefix <> show e -- unlines (lastN 2 $ lines $ show e)

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

stderrMode = do
  args <- getArgs
  return $ "-e" `elem` args || "--stderr" `elem` args

-- | Outputs errors to stderr if the `-e` flag is provided. Otherwise everything
-- goes to stdout.
outputMessage :: Bool -> Verbosity -> String -> IO ()
outputMessage mode verbosity text = do
  let stream = if mode && verbosity == Error then stderr else stdout
  BS.hPutStrLn stream $ UTF8.fromString text

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  cores <- getNumCapabilities
  toStderr <- stderrMode
  return $
    shakeOptions
      { shakeFiles = transientDir,
        shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty,
        shakeThreads = cores,
        shakeColor = not toStderr,
        shakeOutput = outputMessage toStderr,
        shakeChange = ChangeModtimeAndDigest,
        shakeStaunch = toStderr
        -- shakeLint = Just LintBasic,
        -- shakeLint = Just LintFSATrace,
        -- shakeReport = [".decker/shake-report.html"],
        -- shakeChange = ChangeModtime,
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

relativeSupportDir :: FilePath -> FilePath
relativeSupportDir from = makeRelativeTo from supportDir

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  context <- actionContext
  let resource = context ^. publicResource
  withResource resource 1 perform

-- |  Runs the built-in server on the given directory, if it is not already
--  running.
runHttpServerIO :: ActionContext -> Int -> String -> IO ()
runHttpServerIO context port bind = do
  let ref = context ^. server
  server <- readIORef ref
  case server of
    Just _ -> return ()
    Nothing -> do
      httpServer <- startHttpServer context port bind
      writeIORef ref $ Just httpServer

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  context <- actionContext
  let ref = context ^. server
  server <- liftIO $ readIORef ref
  case server of
    Just (_, state) -> do
      (_, pages) <- liftIO $ readMVar state
      return $ Set.toList pages
    Nothing -> return []

openBrowser :: String -> IO ()
openBrowser url =
  if
      | any (`List.isInfixOf` os) ["linux", "bsd"] ->
        liftIO $ callProcess "xdg-open" [url]
      | "darwin" `List.isInfixOf` os -> liftIO $ callProcess "open" [url]
      | otherwise ->
        putStrLn $ "Unable to open browser on this platform for url: " ++ url

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
putCurrentDocument out = putInfo $ "# pandoc (for " ++ out ++ ")"

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
