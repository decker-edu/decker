{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Content diagnostics: problems in the sources that do not fail the build but
-- produce wrong output (an unresolved citation, an unexpanded fragment
-- template, a misspelled question field).
--
-- Each diagnostic has a stable kebab-case code. It is printed in a human form
-- (@[WARNING] x-deck.md:12: ... [code]@) and, with @--diagnostics-json@, also as
-- one JSON object per line on stderr, prefixed with 'diagnosticMarker'.
--
-- Diagnostics are collected per build target. The target is taken from the
-- meta value @decker.target@, which the target rules set. After a target is
-- built, its diagnostics are persisted to the transient directory, so they can
-- be reported again when a later run finds the target up to date (see
-- 'replayDiagnostics'). With @--strict@ (or @strict: true@) a build that leaves
-- any warning fails.
module Text.Decker.Internal.Diagnostics
  ( Severity (..),
    Diagnostic (..),
    DiagnosticSink,
    newDiagnosticSink,
    diagnosticSinkKey,
    diagnosticMarker,
    hint,
    warning,
    report,
    reportFor,
    reportAll,
    withDiagnostics,
    sinkDiagnostics,
    replayDiagnostics,
    counts,
    formatDiagnostic,
    runPandocCollecting,
    pandocDiagnostic,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isUpper, toLower)
import Data.Data (toConstr)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Typeable (TypeRep, typeRep)
import Development.Shake (Action, getShakeExtra, putWarn)
import Relude
import System.Directory qualified as Dir
import System.FilePath (takeDirectory, (<.>), (</>))
import System.FilePath.Posix qualified as Posix
import Text.Decker.Internal.Common (transientDir)
import Text.Decker.Internal.Meta (lookupMeta)
import Text.Pandoc (LogMessage, Meta, PandocError, PandocIO, Verbosity (..), getLog, runIO, setVerbosity)
import Text.Pandoc.Logging (messageVerbosity, showLogMessage)
import Text.Pandoc.Logging qualified as Log

-- | Hints are printed but never fail a strict build. They are used for
-- heuristics with false positives.
data Severity = DiagHint | DiagWarning | DiagError
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Severity where
  toJSON DiagHint = "hint"
  toJSON DiagWarning = "warning"
  toJSON DiagError = "error"

instance FromJSON Severity where
  parseJSON = Aeson.withText "Severity" $ \case
    "hint" -> pure DiagHint
    "warning" -> pure DiagWarning
    "error" -> pure DiagError
    other -> fail $ "unknown severity: " <> toString other

data Diagnostic = Diagnostic
  { diagSeverity :: Severity,
    -- | Stable kebab-case identifier, e.g. @citation-not-found@.
    diagCode :: Text,
    -- | The source file the problem is in.
    diagSource :: FilePath,
    diagLine :: Maybe Int,
    diagMessage :: Text,
    -- | The build target that was being built, if any.
    diagTarget :: Maybe FilePath
  }
  deriving (Eq, Show, Generic)

instance ToJSON Diagnostic where
  toJSON d =
    Aeson.object
      [ "severity" .= diagSeverity d,
        "code" .= diagCode d,
        "source" .= diagSource d,
        "line" .= diagLine d,
        "message" .= diagMessage d,
        "target" .= diagTarget d
      ]

instance FromJSON Diagnostic where
  parseJSON = Aeson.withObject "Diagnostic" $ \o ->
    Diagnostic
      <$> o .: "severity"
      <*> o .: "code"
      <*> o .: "source"
      <*> o .:? "line"
      <*> o .: "message"
      <*> o .:? "target"

-- | A hint (never fails a strict build) without a target.
hint :: Text -> FilePath -> Maybe Int -> Text -> Diagnostic
hint code source line message = Diagnostic DiagHint code source line message Nothing

-- | A warning without a target. The target is filled in by 'report'.
warning :: Text -> FilePath -> Maybe Int -> Text -> Diagnostic
warning code source line message = Diagnostic DiagWarning code source line message Nothing

-- | Collects the diagnostics of one Shake run, keyed by target.
data DiagnosticSink = DiagnosticSink
  { sinkJson :: Bool,
    sinkStore :: TVar (Map FilePath [Diagnostic])
  }

-- | Creates a sink for one Shake run. With @json@ every diagnostic is also
-- printed as a marker line on stderr.
newDiagnosticSink :: Bool -> IO DiagnosticSink
newDiagnosticSink json = DiagnosticSink json <$> newTVarIO Map.empty

-- | The key of the sink in 'shakeExtra'.
diagnosticSinkKey :: TypeRep
diagnosticSinkKey = typeRep (Proxy :: Proxy DiagnosticSink)

-- | Prefix of the machine-readable diagnostic lines on stderr.
diagnosticMarker :: ByteString
diagnosticMarker = "decker-diagnostic: "

-- | Reports a diagnostic for the target named by the meta value
-- @decker.target@.
report :: Meta -> Diagnostic -> Action ()
report meta = reportFor (lookupMeta "decker.target" meta)

reportAll :: Meta -> [Diagnostic] -> Action ()
reportAll meta = mapM_ (report meta)

-- | Reports a diagnostic for an explicit target. Identical diagnostics for the
-- same target are reported only once.
--
-- Diagnostics without a target are dropped. They come from auxiliary reads of
-- a source (e.g. collecting the index info of a deck), and the same problem is
-- reported when the source's own target is built.
reportFor :: Maybe FilePath -> Diagnostic -> Action ()
reportFor Nothing _ = return ()
reportFor (Just target) diagnostic = do
  let diag = diagnostic {diagTarget = Just target, diagSource = Posix.normalise (diagSource diagnostic)}
  sink <- getShakeExtra
  new <- case sink of
    Nothing -> return True
    Just sink -> liftIO $ atomically $ do
      store <- readTVar (sinkStore sink)
      let known = Map.findWithDefault [] target store
      if diag `elem` known
        then return False
        else do
          writeTVar (sinkStore sink) (Map.insert target (known <> [diag]) store)
          return True
  when new $ do
    putWarn $ toString $ formatDiagnostic diag
    when (maybe False sinkJson sink) $ liftIO $ printJson diag

printJson :: Diagnostic -> IO ()
printJson diag =
  BS.hPutStrLn stderr $ diagnosticMarker <> LBS.toStrict (Aeson.encode diag)

-- | The human-readable form, e.g.
-- @[WARNING] unit/07-deck.md:12: citation Foo not found [citation-not-found]@.
formatDiagnostic :: Diagnostic -> Text
formatDiagnostic d =
  label (diagSeverity d)
    <> " "
    <> toText (diagSource d)
    <> maybe "" ((":" <>) . show) (diagLine d)
    <> ": "
    <> diagMessage d
    <> " ["
    <> diagCode d
    <> "]"
  where
    label DiagHint = "[HINT]"
    label DiagWarning = "[WARNING]"
    label DiagError = "[ERROR]"

-- | Where the diagnostics of a target are persisted between runs.
persistedPath :: FilePath -> IO FilePath
persistedPath target = do
  transient <- transientDir
  return $ transient </> "diagnostics" </> target <.> "json"

-- | Runs the rule body that builds @target@ and persists the diagnostics it
-- reported, replacing those of an earlier build.
withDiagnostics :: FilePath -> Action a -> Action a
withDiagnostics target action = do
  path <- liftIO $ persistedPath target
  liftIO $ removeIfExists path
  result <- action
  sink <- getShakeExtra
  forM_ sink $ \sink -> liftIO $ do
    diags <- Map.findWithDefault [] target <$> readTVarIO (sinkStore sink)
    unless (null diags) $ do
      Dir.createDirectoryIfMissing True (takeDirectory path)
      LBS.writeFile path (Aeson.encode diags)
  return result

removeIfExists :: FilePath -> IO ()
removeIfExists path = whenM (Dir.doesFileExist path) $ Dir.removeFile path

-- | All diagnostics collected in the sink.
sinkDiagnostics :: DiagnosticSink -> IO (Map FilePath [Diagnostic])
sinkDiagnostics = readTVarIO . sinkStore

-- | Prints the persisted diagnostics of the given live targets that were not
-- rebuilt in this run (Shake found them up to date) and returns them.
replayDiagnostics :: DiagnosticSink -> [FilePath] -> IO [Diagnostic]
replayDiagnostics sink live = do
  rebuilt <- sinkDiagnostics sink
  let skipped = filter (`Map.notMember` rebuilt) live
  replayed <- concat <$> mapM readPersisted skipped
  forM_ replayed $ \diag -> do
    BS.putStrLn $ encodeUtf8 $ formatDiagnostic diag
    when (sinkJson sink) $ printJson diag
  return replayed
  where
    readPersisted target = do
      path <- persistedPath target
      exists <- Dir.doesFileExist path
      if exists
        then fromMaybe [] . Aeson.decodeStrict <$> readFileBS path
        else return []

-- | The number of hints and the number of failing diagnostics (warnings and
-- errors) in a list.
counts :: [Diagnostic] -> (Int, Int)
counts diags =
  let failing = length $ filter ((> DiagHint) . diagSeverity) diags
   in (length diags - failing, failing)

-- | Runs a pandoc action with pandoc's own log output silenced and returns its
-- warnings as diagnostics for @source@. Errors are returned as usual.
runPandocCollecting :: FilePath -> PandocIO a -> IO (Either PandocError (a, [Diagnostic]))
runPandocCollecting source action = runIO $ do
  setVerbosity ERROR
  result <- action
  messages <- getLog
  return (result, mapMaybe (pandocDiagnostic source) messages)

-- | Converts a pandoc log message at level WARNING or ERROR into a diagnostic.
-- The code is derived from the message constructor (@CiteprocWarning@ becomes
-- @pandoc-citeproc-warning@), except for unresolved citations, which get their
-- own code @citation-not-found@.
pandocDiagnostic :: FilePath -> LogMessage -> Maybe Diagnostic
pandocDiagnostic source message =
  case messageVerbosity message of
    INFO -> Nothing
    level ->
      Just
        Diagnostic
          { diagSeverity = if level == ERROR then DiagError else DiagWarning,
            diagCode = code,
            diagSource = source,
            diagLine = line,
            diagMessage = showLogMessage message,
            diagTarget = Nothing
          }
  where
    json = case toJSON message of
      Object o -> o
      _ -> KeyMap.empty
    line = case KeyMap.lookup "line" json of
      Just (Number n) -> Just (round n)
      _ -> Nothing
    code = case message of
      Log.CiteprocWarning text
        | "citation " `Text.isPrefixOf` text && " not found" `Text.isSuffixOf` text ->
            "citation-not-found"
      _ -> "pandoc-" <> kebab (show (toConstr message))

-- | @CiteprocWarning@ becomes @citeproc-warning@.
kebab :: String -> Text
kebab = toText . go
  where
    go (c : cs) = toLower c : concatMap (\x -> if isUpper x then ['-', toLower x] else [x]) cs
    go [] = []
