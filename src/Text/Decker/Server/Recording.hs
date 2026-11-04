{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Recording sessions on the dev server. The browser streams a recording in
-- chunks to a part file next to the deck source, together with snapshots of
-- the slide timings and the speech recognition. Finishing the session moves
-- the parts into place in one step, so a failed or abandoned recording never
-- destroys the previous one, and video and timings are swapped together.
--
-- For the deck @foo-deck.md@ a running session uses:
--
-- * @foo-recording.webm.part@: the video bytes received so far,
-- * @foo-recording.part.json@: the sidecar (mode, start time, time offset),
-- * @foo-times.json.part@: the latest timing snapshot,
-- * @foo-recording.vtt.part@ and @foo-transcript.json.part@: the latest
--   speech recognition snapshots.
--
-- The session exists while the sidecar exists. The size of the video part is
-- the only other state, so the protocol survives a server restart.
module Text.Decker.Server.Recording
  ( ChunkDecision (..),
    decideChunk,
    RecordingMode (..),
    Sidecar (..),
    SessionInfo (..),
    SessionFiles (..),
    sessionFiles,
    ChunkResult (..),
    Sessions,
    newSessions,
    withSessionLock,
    sessionInfo,
    startSession,
    appendChunk,
    putSnapshot,
    finishSession,
    discardSession,
    validDeckBase,
    recordingRoutes,
    transcodeRecording,
  )
where

import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM (writeTChan)
import Control.Exception (IOException, onException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Scientific (toRealFloat)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Types
import Relude
import System.Directory
import System.FilePath
import System.IO (SeekMode (..), hSeek, hSetFileSize, withBinaryFile)
import System.Random (randomIO)
import Text.Decker.Internal.Common (privateDir, publicDir)
import Text.Decker.Server.Types
import Text.Decker.Server.Video
import UnliftIO.IO.File (ensureFileDurable)
import Web.Scotty.Trans as Scotty

-- | What to do with a chunk that arrives for byte offset @offset@ with
-- @length@ bytes, when the part file has @size@ bytes.
data ChunkDecision
  = -- | The chunk continues the file.
    AppendChunk
  | -- | The chunk is already stored completely (a retry).
    AlreadyStored
  | -- | The chunk starts beyond the end, or overlaps it partly.
    Conflict
  deriving (Eq, Show)

-- | Byte offsets instead of sequence numbers: a retry can never append the
-- same bytes twice, and nothing but the file size has to survive a restart.
decideChunk :: Integer -> Integer -> Integer -> ChunkDecision
decideChunk size offset len
  | offset == size = AppendChunk
  | offset >= 0 && offset < size && offset + len <= size = AlreadyStored
  | otherwise = Conflict

-- | Replace deletes the existing recording when the session is finished.
-- Append adds a fragment.
data RecordingMode = ReplaceRecording | AppendRecording
  deriving (Eq, Show)

instance FromJSON RecordingMode where
  parseJSON = withText "mode" $ \case
    "replace" -> return ReplaceRecording
    "append" -> return AppendRecording
    other -> fail $ "unknown mode: " <> toString other

instance ToJSON RecordingMode where
  toJSON ReplaceRecording = "replace"
  toJSON AppendRecording = "append"

-- | The sidecar of a session. The offset is the position in the recording
-- (in seconds) at which this session starts. It is 0 for replace and the end
-- of the existing recording for append.
data Sidecar = Sidecar
  { scMode :: RecordingMode,
    scStartedAt :: UTCTime,
    scOffset :: Double
  }
  deriving (Eq, Show)

instance FromJSON Sidecar where
  parseJSON = withObject "Sidecar" $ \o ->
    Sidecar <$> o .: "mode" <*> o .: "startedAt" <*> o .:? "offset" .!= 0

instance ToJSON Sidecar where
  toJSON (Sidecar mode startedAt offset) =
    object ["mode" .= mode, "startedAt" .= startedAt, "offset" .= offset]

-- | What the server knows about a running session. The duration (seconds) is
-- taken from the latest timing snapshot, if there is one.
data SessionInfo = SessionInfo
  { siMode :: RecordingMode,
    siSize :: Integer,
    siDuration :: Maybe Double,
    siStartedAt :: UTCTime
  }
  deriving (Eq, Show)

instance ToJSON SessionInfo where
  toJSON (SessionInfo mode size duration startedAt) =
    object ["mode" .= mode, "size" .= size, "duration" .= duration, "startedAt" .= startedAt]

data SessionFiles = SessionFiles
  { videoPart :: FilePath,
    sidecarFile :: FilePath,
    timesPart :: FilePath,
    captionsPart :: FilePath,
    transcriptPart :: FilePath
  }
  deriving (Eq, Show)

-- | The session files of the deck with the base name @base@ (the deck source
-- path without @-deck.md@).
sessionFiles :: FilePath -> SessionFiles
sessionFiles base =
  SessionFiles
    { videoPart = base <> "-recording.webm.part",
      sidecarFile = base <> "-recording.part.json",
      timesPart = base <> "-times.json.part",
      captionsPart = base <> "-recording.vtt.part",
      transcriptPart = base <> "-transcript.json.part"
    }

-- | The snapshot parts with the companions they become on finish.
snapshotTargets :: FilePath -> [(FilePath, FilePath)]
snapshotTargets base =
  let files = sessionFiles base
   in [ (timesPart files, base <> "-times.json"),
        (captionsPart files, base <> "-recording.vtt"),
        (transcriptPart files, base <> "-transcript.json")
      ]

allParts :: SessionFiles -> [FilePath]
allParts files =
  [videoPart files, timesPart files, captionsPart files, transcriptPart files, sidecarFile files]

-- | One lock per deck. Scotty handles requests concurrently, but the requests
-- of one session must not interleave.
newtype Sessions = Sessions (MVar (Map FilePath (MVar ())))

newSessions :: IO Sessions
newSessions = Sessions <$> MVar.newMVar mempty

withSessionLock :: Sessions -> FilePath -> IO a -> IO a
withSessionLock (Sessions locks) base action = do
  let key = normalise base
  lock <- MVar.modifyMVar locks $ \m ->
    case Map.lookup key m of
      Just lock -> return (m, lock)
      Nothing -> do
        lock <- MVar.newMVar ()
        return (Map.insert key lock m, lock)
  MVar.withMVar lock (const action)

-- | Reads the sidecar. One that cannot be read or parsed counts as an append
-- session, so that finishing it never deletes the existing recording.
readSidecar :: SessionFiles -> IO Sidecar
readSidecar files = do
  let path = sidecarFile files
  contents <- try (LBS.readFile path) :: IO (Either IOException LByteString)
  case eitherDecode <$> contents of
    Right (Right sidecar) -> return sidecar
    _ -> do
      time <- getModificationTime path
      return $ Sidecar AppendRecording time 0

fileSizeOrZero :: FilePath -> IO Integer
fileSizeOrZero path = do
  exists <- doesFileExist path
  if exists then getFileSize path else return 0

removeIfExists :: FilePath -> IO ()
removeIfExists path = whenM (doesFileExist path) $ removeFile path

-- | The running session of the deck, if there is one.
sessionInfo :: FilePath -> IO (Maybe SessionInfo)
sessionInfo base = do
  let files = sessionFiles base
  exists <- doesFileExist (sidecarFile files)
  if not exists
    then return Nothing
    else do
      sidecar <- readSidecar files
      size <- fileSizeOrZero (videoPart files)
      duration <- snapshotDuration (scOffset sidecar) (timesPart files)
      return $ Just $ SessionInfo (scMode sidecar) size duration (scStartedAt sidecar)

-- | The end of the last interval of the timing snapshot, minus the offset of
-- the session. The times are seconds, stored as strings or numbers.
snapshotDuration :: Double -> FilePath -> IO (Maybe Double)
snapshotDuration offset path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      contents <- LBS.readFile path
      return $ do
        intervals <- rightToMaybe (eitherDecode contents) :: Maybe [Aeson.Value]
        Aeson.Object interval <- viaNonEmpty last intervals
        end <- KeyMap.lookup (Key.fromString "timeOut") interval >>= seconds
        return $ max 0 (end - offset)
  where
    seconds (Aeson.Number n) = Just (toRealFloat n)
    seconds (Aeson.String s) = readMaybe (toString s)
    seconds _ = Nothing

-- | Starts a session, unless one is running already. Parts left over without
-- a sidecar are remains of an interrupted start or finish and are removed.
startSession :: FilePath -> RecordingMode -> Double -> IO (Either SessionInfo ())
startSession base mode offset = do
  let files = sessionFiles base
  running <- sessionInfo base
  case running of
    Just info -> return (Left info)
    Nothing -> do
      mapM_ removeIfExists (allParts files)
      LBS.writeFile (videoPart files) ""
      ensureFileDurable (videoPart files)
      now <- getCurrentTime
      writeDurableAtomic (sidecarFile files) (encode $ Sidecar mode now offset)
      return (Right ())

data ChunkResult
  = -- | The chunk is stored. The part file now has this size.
    Stored Integer
  | -- | The offset does not fit. The part file has this size.
    Mismatch Integer
  | NoSession
  deriving (Eq, Show)

-- | Writes the file atomically and durably: to a hidden part file next to it,
-- which is synced and then renamed. The file gets the default permissions.
-- (unliftio's durable writers create files with 0600 or worse, which would
-- carry over to @public/@ and to the published files. Only its
-- 'ensureFileDurable', which syncs an existing file and its directory, is
-- used.)
writeDurableAtomic :: FilePath -> LByteString -> IO ()
writeDurableAtomic path contents = do
  n <- randomIO :: IO Word
  let (dir, name) = splitFileName path
      tmp = dir </> ("." <> name <> "." <> show n <> ".part")
  (LBS.writeFile tmp contents >> ensureFileDurable tmp) `onException` removeIfExists tmp
  renameFile tmp path
  ensureFileDurable path

-- | Appends the chunk at the offset, see 'decideChunk'. The whole chunk is in
-- memory before anything is written, so a dropped connection never leaves a
-- partial chunk. A failed write truncates the file to its old size. The data
-- is on disk when this returns.
appendChunk :: FilePath -> Integer -> LByteString -> IO ChunkResult
appendChunk base offset chunk = do
  let files = sessionFiles base
  running <- doesFileExist (sidecarFile files)
  if not running
    then return NoSession
    else do
      size <- fileSizeOrZero (videoPart files)
      let len = toInteger (LBS.length chunk)
      case decideChunk size offset len of
        AppendChunk -> do
          withBinaryFile (videoPart files) ReadWriteMode $ \h -> do
            hSeek h AbsoluteSeek size
            LBS.hPut h chunk `onException` hSetFileSize h size
          ensureFileDurable (videoPart files)
          return $ Stored (size + len)
        AlreadyStored -> return $ Stored size
        Conflict -> return $ Mismatch size

-- | Replaces a snapshot part atomically. False if there is no session.
putSnapshot :: FilePath -> FilePath -> LByteString -> IO Bool
putSnapshot base part contents = do
  running <- doesFileExist (sidecarFile (sessionFiles base))
  when running $ writeDurableAtomic part contents
  return running

-- | Moves the parts of the session into place and ends it. Returns the new
-- video fragment, if there is one.
--
-- * Replace: the existing fragments and the old MP4 are deleted, the video
--   part becomes @-recording.webm@.
-- * Append: the video part becomes the next numbered fragment.
--
-- The snapshots replace their companions. An empty video part discards the
-- whole session, so that stopping right after the start does not replace a
-- good recording with nothing. If the video part is missing, an earlier
-- finish was interrupted after moving it, and only the rest is moved.
finishSession :: FilePath -> IO (Maybe FilePath)
finishSession base = do
  let files = sessionFiles base
      webm = base <> "-recording.webm"
  sidecar <- readSidecar files
  hasVideo <- doesFileExist (videoPart files)
  size <- fileSizeOrZero (videoPart files)
  if hasVideo && size == 0
    then discardSession base >> return Nothing
    else do
      fragment <-
        if hasVideo
          then Just <$> moveVideo (scMode sidecar) (videoPart files) webm
          else return Nothing
      forM_ (snapshotTargets base) $ \(part, final) ->
        whenM (doesFileExist part) $ renameFile part final
      removeIfExists (sidecarFile files)
      return fragment

moveVideo :: RecordingMode -> FilePath -> FilePath -> IO FilePath
moveVideo ReplaceRecording part webm = do
  existingVideos webm >>= mapM_ removeFile
  removeIfExists (replaceExtension webm ".mp4")
  renameFile part webm
  return webm
moveVideo AppendRecording part webm = do
  existing <- existingVideos webm
  case existing of
    [] -> do
      renameFile part webm
      return webm
    [single] -> do
      let name0 = setSequenceNumber 0 webm
          name1 = setSequenceNumber 1 webm
      renameFile single name0
      renameFile part name1
      return name1
    multiple -> do
      let name = setSequenceNumber (getHighestSequenceNumber multiple + 1) webm
      renameFile part name
      return name

-- | Deletes all parts of the session. False if there was none.
discardSession :: FilePath -> IO Bool
discardSession base = do
  let parts = allParts (sessionFiles base)
  existed <- or <$> mapM doesFileExist parts
  mapM_ removeIfExists parts
  return existed

-- | Converts the fragments of the recording to @-recording.mp4@. Runs in the
-- action loop, serialized with builds.
transcodeRecording :: FilePath -> IO ()
transcodeRecording webm = do
  let mp4 = replaceExtension webm ".mp4"
  fragments <- existingVideos webm
  case fragments of
    [] -> return ()
    [single] -> convertVideoMp4 single mp4
    multiple -> concatVideoMp4 multiple mp4

-- | The deck base name from a request path, if it names a deck: relative, no
-- @..@, outside the build output, and @<base>-deck.md@ exists.
validDeckBase :: FilePath -> IO (Maybe FilePath)
validDeckBase raw = do
  let base = normalise raw
      parts = splitDirectories base
      inOutput = any (\dir -> [dir] `isPrefixOf` parts) [publicDir, privateDir]
  if null raw || isAbsolute base || ".." `elem` parts || inOutput
    then return Nothing
    else do
      exists <- doesFileExist (base <> "-deck.md")
      return $ if exists then Just base else Nothing

-- | The routes of the recording protocol. @<deck>@ is the deck path without
-- @-deck.html@. With @transcode@, a finished recording is converted to MP4 in
-- the action loop.
recordingRoutes :: Sessions -> Bool -> AppScottyM ()
recordingRoutes sessions transcode = do
  Scotty.post (regex "^/recording/(.+)/start$") $ withDeck $ \base -> do
    raw <- Scotty.body
    case eitherDecode raw of
      Left err -> badRequest err
      Right (StartRequest mode offset) -> do
        result <- liftIO $ locked base $ startSession base mode offset
        case result of
          Left info -> status status409 >> json info
          Right () -> json $ object ["size" .= (0 :: Integer)]
  Scotty.put (regex "^/recording/(.+)/chunk$") $ withDeck $ \base -> do
    offset <- queryParamMaybe "offset"
    case offset of
      Nothing -> badRequest "missing offset"
      Just offset -> do
        chunk <- Scotty.body
        result <- liftIO $ locked base $ appendChunk base offset chunk
        case result of
          Stored size -> json $ object ["size" .= size]
          Mismatch size -> status status409 >> json (object ["size" .= size])
          NoSession -> noSession
  snapshot "times" timesPart
  snapshot "captions" captionsPart
  snapshot "transcript" transcriptPart
  Scotty.post (regex "^/recording/(.+)/finish$") $ withDeck $ \base -> do
    raw <- Scotty.body
    case eitherDecode raw of
      Left err -> badRequest err
      Right (FinishRequest size) -> do
        result <- liftIO $ locked base $ do
          running <- sessionInfo base
          case running of
            Nothing -> return Nothing
            Just info
              | siSize info /= size -> return $ Just (Left (siSize info))
              | otherwise -> Just . Right <$> finishSession base
        case result of
          Nothing -> noSession
          Just (Left actual) -> status status409 >> json (object ["size" .= actual])
          Just (Right fragment) -> do
            let converting = transcode && isJust fragment
            when converting $ do
              chan <- lift $ asks channel
              atomically $ writeTChan chan (TranscodeRecording (base <> "-recording.webm"))
            json $ object ["fragment" .= fragment, "transcoding" .= converting]
  Scotty.delete (regex "^/recording/(.+)$") $ withDeck $ \base -> do
    existed <- liftIO $ locked base $ discardSession base
    if existed then json (object ["discarded" .= True]) else noSession
  where
    locked = withSessionLock sessions
    snapshot name part =
      Scotty.put (regex $ "^/recording/(.+)/" <> name <> "$") $ withDeck $ \base -> do
        contents <- Scotty.body
        running <- liftIO $ locked base $ putSnapshot base (part (sessionFiles base)) contents
        if running then json (object ["size" .= LBS.length contents]) else noSession
    badRequest :: String -> AppActionM ()
    badRequest err = status status400 >> text ("ERROR: " <> toLText err)
    noSession = status status404 >> text "ERROR: no recording session"
    withDeck action = do
      raw <- captureParam "1"
      deck <- liftIO $ validDeckBase raw
      maybe (status status404 >> text "ERROR: no such deck") action deck

-- | The mode, and the position (seconds) at which the session starts.
data StartRequest = StartRequest RecordingMode Double

instance FromJSON StartRequest where
  parseJSON = withObject "StartRequest" $ \o ->
    StartRequest <$> o .: "mode" <*> o .:? "offset" .!= 0

newtype FinishRequest = FinishRequest Integer

instance FromJSON FinishRequest where
  parseJSON = withObject "FinishRequest" $ \o -> FinishRequest <$> o .: "size"
