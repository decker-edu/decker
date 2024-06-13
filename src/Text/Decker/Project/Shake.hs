{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Project.Shake
  ( runDecker,
    runDeckerArgs,
    calcSource,
    calcSource',
    currentlyServedPages,
    relativeSupportDir,
    putCurrentDocument,
    watchChangesAndRepeat,
    withShakeLock,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (newTChan, readTChan, writeTChan)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (try)
import Data.Aeson hiding (Error)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char
import Data.Dynamic
import Data.HashMap.Strict qualified as HashMap
import Data.List.Extra qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Time
import Development.Shake hiding (doesDirectoryExist, putError)
import GHC.IO.Exception (ExitCode (ExitFailure))
import NeatInterpolation
import Relude hiding (state)
import System.Console.GetOpt qualified as GetOpt
import System.Directory as Dir
import System.FSNotify qualified as Notify
import System.FilePath.Glob
import System.FilePath.Posix
import System.Info
import System.Process hiding (runCommand)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Crrrunch
import Text.Decker.Internal.External
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.Transcribe
import Text.Decker.Project.ActionContext
import Text.Decker.Project.Glob (fastGlobDirs)
import Text.Decker.Project.Project
import Text.Decker.Project.Version
import Text.Decker.Resource.Resource
import Text.Decker.Server.Server
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import Text.Pandoc hiding (Verbosity)

runDecker :: Rules () -> IO ()
runDecker rules = do
  args <- getArgs
  runDeckerArgs args rules

runDeckerArgs :: [String] -> Rules () -> IO ()
runDeckerArgs args theRules = do
  -- Parse the extra options first so that we can use them in the commands
  -- without running shake. Errors are ignored and will be reported by shake
  -- later.
  let (results, targets, _) = GetOpt.getOpt GetOpt.Permute deckerFlags args
  let flags = rights results
  let rules =
        if null targets
          then theRules
          else want targets >> withoutActions theRules
  meta <- readMetaDataFile globalMetaFileName
  context <- initContext flags meta
  let commands = ["clean", "purge", "example", "serve", "crunch", "crrrunch", "transcribe", "transcrrribe", "pdf", "version", "check"]
  case targets of
    [command] | command `elem` commands -> runCommand context command rules
    _ -> runTargets context targets rules

runTargets :: ActionContext -> [FilePath] -> Rules () -> IO ()
runTargets context targets rules = do
  let flags = context ^. extra
  extractMetaIntoFile flags
  -- channel <- atomically newTChan

  when (OpenFlag `elem` flags) $ do
    let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort flags
    openBrowser $ "http://localhost:" <> show port <> "/index.html"

  -- always rescan the targets file in case files where added or removed
  let meta = context ^. globalMeta
  targets <- targetsFile
  scanTargetsToFile meta targets

  -- Always run at least once
  runShake context rules

  if
    | ServerFlag `elem` flags -> do
        forkServer context
        watchAndRunForever
    | WatchFlag `elem` flags -> do
        watchAndRunForever
    | otherwise -> return ()
  where
    watchAndRunForever = do
      Notify.withManager $ \manager -> do
        startWatcher manager context
        runShakeForever Nothing context rules

runShake :: ActionContext -> Rules () -> IO ()
runShake context rules = do
  options <- deckerShakeOptions context
  shakeArgsWith options deckerFlags (\_ _ -> return $ Just rules)

_runShakeSlyly :: ActionContext -> Rules () -> IO ()
_runShakeSlyly context rules = do
  -- always rescan the targets file in case files where added or removed
  let meta = context ^. globalMeta
  targets <- targetsFile
  scanTargetsToFile meta targets
  let flags = context ^. extra
  extractMetaIntoFile flags
  options <- deckerShakeOptions context
  transient <- transientDir
  shakeArgsWith (options {shakeFiles = transient </> "crunch"}) deckerFlags (\_ _ -> return $ Just rules)

runShakeForever :: Maybe ActionMsg -> ActionContext -> Rules () -> IO b
runShakeForever last context rules = do
  let flags = context ^. extra
  dod <- debouncedMessage last
  case dod of
    FileChanged time path -> do
      unless (NoRebuildFlag `elem` flags)
        $ catchAll
          (runShake context rules)
          (\(SomeException _) -> return ())
      putStrLn $ "# Server: Reload because of: " <> path
      reloadClients (context ^. server)
    UploadComplete operation -> do
      let transcode = PoserFlag `elem` (context ^. extra)
      applyVideoOperation transcode operation
      reloadClients (context ^. server)
    ServerExit port -> do
      putStrLn $ "# Server: " <> show port
      exitWith (ExitFailure 7)
  runShakeForever (Just dod) context rules
  where
    debouncedMessage (Just (FileChanged lastTime path)) = do
      msg <- atomically $ readTChan (context ^. actionChan)
      case msg of
        FileChanged time info | diffUTCTime time lastTime < 0.5 -> debouncedMessage (Just msg)
        _ -> return msg
    debouncedMessage _ = atomically $ readTChan (context ^. actionChan)

handleUploads :: ActionContext -> IO ()
handleUploads context = do
  forever $ do
    dod <- atomically $ readTChan (context ^. actionChan)
    case dod of
      UploadComplete operation -> do
        let transcode = PoserFlag `elem` (context ^. extra)
        applyVideoOperation transcode operation
      _ -> return ()

applyVideoOperation transcode op@(Replace tmp destination) = do
  replaceVideoUpload transcode tmp destination
applyVideoOperation transcode op@(Append tmp destination) = do
  appendVideoUpload transcode tmp destination

startWatcher :: Notify.WatchManager -> ActionContext -> IO ()
startWatcher manager context = do
  let meta = context ^. globalMeta
  let excludeGlob' = excludeGlob meta
  -- mapM_ print excludeGlob'
  -- excludeDirs' <- mapM canonicalizePath $ excludeDirs meta
  let excludeDirs' = excludeDirs meta
  inDir <- makeAbsolute projectDir
  options <- deckerShakeOptions context
  void
    $ Notify.watchTree manager inDir (filter inDir excludeDirs' excludeGlob')
    $ \event ->
      atomically $ writeTChan (context ^. actionChan) (FileChanged (Notify.eventTime event) (show event))
  where
    filter base dirs globs event =
      let path = makeRelative base (Notify.eventPath event)
       in not (any (`isPrefixOf` path) dirs || any (`match` path) globs)

forkServer :: ActionContext -> IO ThreadId
forkServer context = do
  forkIO $ do
    catchAll
      (runHttpServer context)
      ( \(SomeException e) -> do
          atomically $ writeTChan (context ^. actionChan) (ServerExit $ show e)
      )

runCommand :: (Eq a, IsString a) => ActionContext -> a -> Rules () -> IO b
runCommand context command rules = do
  case command of
    "clean" -> runClean False
    "purge" -> runClean True
    "example" -> writeExampleProject (context ^. globalMeta)
    "serve" -> do
      forkServer context
      handleUploads context
    "crunch" -> crunchAllRecordings context
    "crrrunch" -> crunchAllRecordings context
    "transcribe" -> transcribeRecordings context
    "transcrrribe" -> transcribeAllRecordings context
    "version" -> putDeckerVersion
    "check" -> forceCheckExternalPrograms
    "pdf" -> do
      putStrLn (toString pdfMsg)
      id <- forkServer context
      -- let rules' = want ["build-pdf"] >> withoutActions rules
      -- runShake context rules'
      runShake context rules
      killThread id
    _ -> error "Unknown command. Should not happen."
  exitSuccess

-- crunchRecordings :: ActionContext -> IO ()
-- crunchRecordings context = runShakeSlyly context crunchRules

transcribeRecordings :: ActionContext -> IO ()
transcribeRecordings context = do
  let baseDir = lookupMetaOrElse "/usr/local/share/whisper.cpp" "whisper.base-dir" (context ^. globalMeta)
  exists <- Dir.doesFileExist $ baseDir </> "main"
  if exists
    then transcribeAllRecordings context
    -- then runShakeSlyly context transcriptionRules
    else putStrLn "Install https://github.com/ggerganov/whisper.cpp to generate transcriptions."

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
      "Watch changes to source files and rebuild current target if necessary.",
    GetOpt.Option
      ['n']
      ["no-rebuild"]
      (GetOpt.NoArg $ Right NoRebuildFlag)
      "Do not rebuild everything, just reload the clients.",
    GetOpt.Option
      ['S']
      ["server"]
      (GetOpt.NoArg $ Right ServerFlag)
      "Serve the public dir via HTTP (implies --watch).",
    GetOpt.Option
      ['e']
      ["stderr"]
      (GetOpt.NoArg $ Right ErrorFlag)
      "Output errors to stderr.",
    GetOpt.Option
      ['t']
      ["single-thread"]
      (GetOpt.NoArg $ Right ThreadFlag)
      "Run single threaded.",
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
      "Bind the HTTP server to given address.",
    GetOpt.Option
      ['P']
      ["poser"]
      (GetOpt.NoArg $ Right PoserFlag)
      "Transcode recordings immediately to MP4."
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

addMetaFlags :: [Flags] -> Meta -> Meta
addMetaFlags flags meta =
  foldl'
    ( \meta (MetaValueFlag key value) ->
        readMetaValue (toText key) (toText value) meta
    )
    meta
    (filter aMetaValue flags)

-- | Saves the meta flags to a well known file. Will be later read and cached by
-- shake.
extractMetaIntoFile :: [Flags] -> IO ()
extractMetaIntoFile flags = do
  let metaFlags = HashMap.fromList $ map (\(MetaValueFlag k v) -> (k, v)) $ filter aMetaValue flags
  let json = decodeUtf8 $ encode metaFlags
  argsFile <- metaArgsFile
  writeFileChanged argsFile json

aMetaValue (MetaValueFlag _ _) = True
aMetaValue _ = False

initContext :: [Flags] -> Meta -> IO ActionContext
initContext extra meta = do
  transient <- transientDir
  createDirectoryIfMissing True transient
  devRun <- isDevelopmentRun
  external <- checkExternalPrograms
  server <- newTVarIO (ServerState [] Set.empty)
  watch <- newIORef False
  public <- newResourceIO "public" 1
  chan <- atomically newTChan
  when devRun $ putStrLn "This is a DEVELOPMENT RUN"
  return $ ActionContext extra devRun external server watch chan public (addMetaFlags extra meta)

watchChangesAndRepeat :: Action ()
watchChangesAndRepeat = do
  context <- actionContext
  liftIO $ watchChangesAndRepeatIO context

watchChangesAndRepeatIO :: ActionContext -> IO ()
watchChangesAndRepeatIO context = do
  let ref = context ^. watch
  liftIO $ writeIORef ref True

-- | Outputs errors to stderr if the `-e` flag is provided. Otherwise everything
-- goes to stdout.
outputMessage :: Bool -> Verbosity -> String -> IO ()
outputMessage mode verbosity text = do
  let stream = if mode && verbosity == Error then stderr else stdout
  BS.hPutStrLn stream $ UTF8.fromString text

deckerShakeOptions :: ActionContext -> IO ShakeOptions
deckerShakeOptions ctx = do
  let single = ThreadFlag `elem` (ctx ^. extra)
  let toStderr = ErrorFlag `elem` (ctx ^. extra)
  transient <- transientDir
  return
    $ shakeOptions
      { shakeFiles = transient,
        shakeExtra = HashMap.insert actionContextKey (toDyn ctx) HashMap.empty,
        shakeThreads = if single then 1 else 0,
        shakeColor = not toStderr,
        shakeStaunch = toStderr,
        shakeOutput = outputMessage toStderr,
        shakeChange = ChangeModtime
        -- shakeLint = Just LintFSATrace,
        -- shakeReport = [".decker/shake-report.html"],
        -- shakeChange = ChangeModtimeAndDigest
      }

relativeSupportDir :: FilePath -> FilePath
relativeSupportDir from = makeRelativeTo from supportDir

withShakeLock :: Action a -> Action a
withShakeLock perform = do
  context <- actionContext
  let resource = context ^. publicResource
  withResource resource 1 perform

-- | Returns a list of all pages currently served, if any.
currentlyServedPages :: Action [FilePath]
currentlyServedPages = do
  context <- actionContext
  (ServerState _ pages) <- liftIO $ readTVarIO (context ^. server)
  return $ toList pages

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
  let src = (replaceSuffix targetSuffix srcSuffix . makeRelative publicDir) target
  need [src]
  return src

-- |  calcSource without the call to need and without the suffix replacement
calcSource' :: FilePath -> Action FilePath
calcSource' target = do
  return $ makeRelative publicDir target

putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = putInfo $ "# pandoc (for " ++ out ++ ")"

-- | Functionality for "decker purge" command. Removes public and .decker
-- directory. Located outside of Shake due to unlinking differences and
-- parallel processes on Windows which prevented files (.shake.lock) from being
-- deleted on Windows.
runClean :: Bool -> IO ()
runClean totally = do
  warnVersion
  putStrLn $ "# Removing " <> publicDir
  tryRemoveDirectory publicDir
  putStrLn $ "# Removing " <> privateDir
  tryRemoveDirectory privateDir
  when totally
    $ do
      transient <- transientDir
      putStrLn $ "# Removing " ++ transient
      tryRemoveDirectory transient
      dirs <- fastGlobDirs alwaysExclude (== renderedCodeDir) "."
      forM_
        dirs
        ( \dir -> do
            putStrLn $ "# Removing " ++ dir
            tryRemoveDirectory dir
        )

pdfMsg =
  [text|
    # 
    # To use 'decker pdf' Google Chrome has to be installed.
    # 
    # Windows: Currently 'decker pdf' does not work on Windows.
    #   Please add 'print: true' or 'menu: true' to your slide deck and use
    #   the print button on the title slide.
    #
    # MacOS: Follow the Google Chrome installer instructions.
    #   'Google Chrome.app' has to be located in either of these locations
    #
    #   - '/Applications/Google Chrome.app' 
    #   - '/Users/<username>/Applications/Google Chrome.app'
    #
    # Linux: 'chrome' has to be on $$PATH.
    # 
  |]
