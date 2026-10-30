{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

module Text.Decker.Project.Shake
  ( runDecker,
    runDeckerArgs,
    deckerFlags,
    calcSource,
    calcSource',
    currentlyServedPages,
    relativeSupportDir,
    putCurrentDocument,
    watchChangesAndRepeat,
    withShakeLock,
    runClean,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (newTChan, readTChan, writeTChan)
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Catch hiding (try)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Char
import Data.Dynamic
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict qualified as Map
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
-- import System.FilePath.Posix
import System.FilePath
import System.Info
import System.Process hiding (runCommand)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Crrrunch
import Text.Decker.Internal.Diagnostics
import Text.Decker.Internal.External (checkExternal)
import Text.Decker.Internal.Helper
import Text.Decker.Internal.Meta
import Text.Decker.Internal.MetaExtra (readDeckerMetaIO, readMetaDataIO)
import Text.Decker.Resource.Template (readTemplateMetaIO)
import Text.Decker.Internal.Transcribe
import Text.Decker.Project.ActionContext
import Text.Decker.Project.AgentDocs (AgentDocsOpts (..), defaultAgentDocsOpts, runAgentDocs, runAgentGuide)
import Text.Decker.Project.Info (printProjectInfo)
import Text.Decker.Project.Glob (fastGlobDirs)
import Text.Decker.Project.Project
import Text.Decker.Project.Version
import Text.Decker.Chatty.Upload (runChatty)
import Text.Decker.Reader.Markdown (formatStdin)
import Text.Decker.Resource.Resource
import Text.Decker.Server.Server
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import Text.Pandoc (Meta)
import Text.Pandoc.Definition (nullMeta)

runDecker :: Rules () -> IO ()
runDecker rules = do
  devRun <- isDevelopmentRun
  -- stderr, so machine-readable stdout (info --json, agent-guide) stays clean
  when devRun $ BS.hPutStrLn stderr "DEVELOPMENT RUN"
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
  meta <- fromRight nullMeta <$> readMetaDataFile deckerMetaFile
  context <- initContext flags meta
  let commands = ["clean", "purge", "example", "serve", "crunch", "transcribe", "pdf", "version", "check", "format", "chatty", "exam-builder", "agent-docs"]
  case targets of
    [command] | command `elem` commands -> runCommand context command rules
    -- These print machine-readable output on stdout and run without Shake.
    ("agent-guide" : topics) -> do
      meta <- commandMeta flags
      runAgentGuide meta (AllFlag `elem` flags) topics
    ["info"] | JsonFlag `elem` flags -> commandMeta flags >>= printProjectInfo
    otherwise -> do
      warnVersion
      runTargets context targets rules

runTargets :: ActionContext -> [FilePath] -> Rules () -> IO ()
runTargets context targets rules = do
  let flags = context ^. extra
  extractMetaIntoFile flags
  -- channel <- atomically newTChan

  when (OpenFlag `elem` flags) $ do
    let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort flags
    openBrowser $ "http://localhost:" <> show port <> "/index.html"

  -- Always run at least once
  sink <- runShakeCollecting context rules
  failing <- finishDiagnostics context sink
  when (failing > 0 && isStrict context && not (watching flags)) $ do
    BS.hPutStrLn stderr $ "# strict: " <> show failing <> " diagnostic(s), failing the build"
    exitWith (ExitFailure 1)

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
    watching flags = ServerFlag `elem` flags || WatchFlag `elem` flags

-- | Strict mode fails a build that leaves warnings. Set with @--strict@ or the
-- meta value @strict: true@.
isStrict :: ActionContext -> Bool
isStrict context =
  StrictFlag `elem` (context ^. extra)
    || lookupMetaOrElse False "strict" (context ^. globalMeta)

-- | Reports again the diagnostics of the targets that Shake found up to date,
-- prints a summary and returns the number of failing (non-hint) diagnostics of
-- all targets of the run.
finishDiagnostics :: ActionContext -> DiagnosticSink -> IO Int
finishDiagnostics context sink = do
  live <- liveFile
  exists <- Dir.doesFileExist live
  liveTargets <- if exists then lines . decodeUtf8 <$> readFileBS live else return []
  replayed <- replayDiagnostics sink (map toString liveTargets)
  fresh <- concat . toList <$> sinkDiagnostics sink
  let (hints, failing) = counts (fresh <> replayed)
  when (hints + failing > 0)
    $ putStrLn
    $ "# diagnostics: "
    <> show failing
    <> " warning(s), "
    <> show hints
    <> " hint(s)"
    <> if isStrict context then "" else " (use --strict to fail the build)"
  return failing

runShake :: ActionContext -> Rules () -> IO ()
runShake context rules = void $ runShakeCollecting context rules

-- | Runs Shake once and returns the sink holding the diagnostics reported by
-- the targets that were rebuilt.
runShakeCollecting :: ActionContext -> Rules () -> IO DiagnosticSink
runShakeCollecting context rules = do
  -- always rescan the targets file in case files where added or removed
  let meta = context ^. globalMeta
  targets <- targetsFile
  scanTargetsToFile meta targets

  sink <- newDiagnosticSink (DiagnosticsJsonFlag `elem` (context ^. extra))
  options <- deckerShakeOptions context sink
  shakeArgsWith options deckerFlags (\_ _ -> return $ Just rules)
  return sink

_runShakeSlyly :: ActionContext -> Rules () -> IO ()
_runShakeSlyly context rules = do
  -- always rescan the targets file in case files where added or removed
  let meta = context ^. globalMeta
  targets <- targetsFile
  scanTargetsToFile meta targets

  let flags = context ^. extra
  extractMetaIntoFile flags
  sink <- newDiagnosticSink False
  options <- deckerShakeOptions context sink
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
      -- putStrLn $ "# Server: Reload because of: " <> path
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

-- | The loop of @decker serve@. Exits like 'runShakeForever' if the server
-- fails, e.g. because the port is in use.
handleUploads :: ActionContext -> IO ()
handleUploads context = do
  forever $ do
    dod <- atomically $ readTChan (context ^. actionChan)
    case dod of
      UploadComplete operation -> do
        let transcode = PoserFlag `elem` (context ^. extra)
        applyVideoOperation transcode operation
      ServerExit reason -> do
        BS.hPutStrLn stderr $ UTF8.fromString $ "# Server: " <> reason
        exitWith (ExitFailure 7)
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
  meta <- readDeckerMetaIO deckerMetaFile
  case command of
    "check" -> checkExternal meta
    "clean" -> runClean False
    "purge" -> runClean True
    "example" -> writeExampleProject meta
    "serve" -> do
      forkServer context
      handleUploads context
    "exam-builder" -> do
      extractMetaIntoFile (context ^. extra)
      -- Pre-render the question catalog to JSON before serving.
      runShake context rules
      let PortFlag port = fromMaybe (PortFlag 8888) $ find aPort (context ^. extra)
      openBrowser $ "http://localhost:" <> show port <> "/support/exam-builder.html"
      forkServer context
      -- Watch source files so edits to *-quest.yaml rebuild questions.json and
      -- reload the browser, exactly like the 'serve' command.
      Notify.withManager $ \manager -> do
        startWatcher manager context
        runShakeForever Nothing context rules
    "crunch" -> crunchAllRecordings context
    "transcribe" -> transcribeAllRecordings meta
    "version" -> putDeckerVersion
    "agent-docs" -> runAgentDocs meta (agentDocsOptsFromFlags (context ^. extra))
    "pdf" -> do
      putStrLn (toString pdfMsg)
      id <- forkServer context
      runShake context rules
      killThread id
    "format" -> formatStdin
    "chatty" -> do
      extractMetaIntoFile (context ^. extra)
      runShake context rules
      runChatty (PruneFilesFlag `elem` (context ^. extra))
    _ -> error "Unknown command. Should not happen."
  exitSuccess

-- | Build 'AgentDocsOpts' for the @agent-docs@ command from the parsed flags.
agentDocsOptsFromFlags :: [Flags] -> AgentDocsOpts
agentDocsOptsFromFlags flags =
  defaultAgentDocsOpts
    { adoSkill = NoSkillFlag `notElem` flags,
      adoAgentsMd = NoAgentsMdFlag `notElem` flags
    }

-- | The global meta data for commands that run without Shake: @decker.yaml@,
-- the @-m@ flags of this invocation and the resource pack defaults. Unlike
-- 'readDeckerMetaIO' it ignores the meta flags saved by an earlier run.
commandMeta :: [Flags] -> IO Meta
commandMeta flags = do
  deckerMeta <- readMetaDataIO nullMeta deckerMetaFile
  let base = addMetaFlags flags deckerMeta
  defaults <- readTemplateMetaIO base
  return $ mergePandocMeta base defaults

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
      "Bind the HTTP server to given address (default: 127.0.0.1).",
    GetOpt.Option
      ['P']
      ["poser"]
      (GetOpt.NoArg $ Right PoserFlag)
      "Transcode recordings immediately to MP4.",
    GetOpt.Option
      ['l']
      ["lecture"]
      (GetOpt.NoArg $ Right LectureFlag)
      "Enable lecture publishing.",
    GetOpt.Option
      ['d']
      ["project-dir"]
      (GetOpt.ReqArg (Right . ProjectDirFlag) "DIR")
      "Change to DIR before locating the project root. Useful for building a project that lives elsewhere.",
    GetOpt.Option
      []
      ["resource-base"]
      (GetOpt.ReqArg (Right . MetaValueFlag "resource-base") "URI")
      "Replace the base resource pack that `resource-pack` shadows (e.g. a local dir, a .zip, or exe:PATH). For testing modifications to the base pack. Same as '-m resource-base=URI'.",
    GetOpt.Option
      []
      ["prune-files"]
      (GetOpt.NoArg $ Right PruneFilesFlag)
      "With `chatty`: delete OpenAI file objects not attached to the vector store.",
    GetOpt.Option
      []
      ["strict"]
      (GetOpt.NoArg $ Right StrictFlag)
      "Fail the build if any content diagnostic (e.g. an unresolved citation) is reported. Same as 'strict: true' in decker.yaml.",
    GetOpt.Option
      []
      ["diagnostics-json"]
      (GetOpt.NoArg $ Right DiagnosticsJsonFlag)
      "Also print each diagnostic as a JSON object on stderr, on a line prefixed with 'decker-diagnostic: '.",
    GetOpt.Option
      []
      ["no-skill"]
      (GetOpt.NoArg $ Right NoSkillFlag)
      "With `agent-docs`: do not write .claude/skills/decker/SKILL.md.",
    GetOpt.Option
      []
      ["no-agents-md"]
      (GetOpt.NoArg $ Right NoAgentsMdFlag)
      "With `agent-docs`: do not write the decker section of AGENTS.md.",
    GetOpt.Option
      []
      ["all"]
      (GetOpt.NoArg $ Right AllFlag)
      "With `agent-guide`: print all topics.",
    GetOpt.Option
      []
      ["json"]
      (GetOpt.NoArg $ Right JsonFlag)
      "With `info`: print project facts as JSON on stdout."
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
    check [] = False
    check (s:sx) = isAlpha s && all (\c -> isAlphaNum c || isSymbol c || isPunctuation c) sx

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
  let metaFlags' =
        if LectureFlag `elem` flags
          then Map.insert "lecture.publish" "yes" metaFlags
          else metaFlags
  let meta = HashMap.foldrWithKey (\key val m -> addMetaKeyValue (toText key) (toText val) m) nullMeta metaFlags'
  argsFile <- metaArgsFile
  writeMetaDataFile argsFile meta

aMetaValue (MetaValueFlag _ _) = True
aMetaValue _ = False

initContext :: [Flags] -> Meta -> IO ActionContext
initContext extra meta = do
  transient <- transientDir
  createDirectoryIfMissing True transient
  devRun <- isDevelopmentRun
  server <- newTVarIO (ServerState [] Set.empty)
  watch <- newIORef False
  public <- newResourceIO "public" 1
  chan <- atomically newTChan
  return $ ActionContext extra devRun server watch chan public (addMetaFlags extra meta)

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
outputMessage mode verbosity text =
  -- The live list is only written for 'finishDiagnostics'; don't announce it.
  unless ("Writing live list to " `List.isInfixOf` text) $ do
    let stream = if mode && verbosity == Error then stderr else stdout
    BS.hPutStrLn stream $ UTF8.fromString text

deckerShakeOptions :: ActionContext -> DiagnosticSink -> IO ShakeOptions
deckerShakeOptions ctx sink = do
  let single = ThreadFlag `elem` (ctx ^. extra)
  let toStderr = ErrorFlag `elem` (ctx ^. extra)
  transient <- transientDir
  live <- liveFile
  return
    $ shakeOptions
      { shakeFiles = transient,
        shakeExtra =
          HashMap.fromList
            [ (actionContextKey, toDyn ctx),
              (diagnosticSinkKey, toDyn sink)
            ],
        -- The live files tell 'finishDiagnostics' which up-to-date targets
        -- belong to the run.
        shakeLiveFiles = [live],
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
  putStrLn $ "# Removing " <> publicDir
  tryRemoveDirectory publicDir
  putStrLn $ "# Removing " <> privateDir
  tryRemoveDirectory privateDir
  putStrLn $ "# Removing " <> "chatty"
  tryRemoveDirectory "chatty"
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
