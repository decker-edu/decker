{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Video where

import Control.Concurrent.STM (writeTChan)
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.List qualified as List
import Data.Maybe
import Network.HTTP.Types
import Relude
import System.Directory
import System.FilePath.Glob
import System.FilePath.Posix
import System.Process
import Text.Decker.Filter.Util (randomId)
import Text.Decker.Internal.Common
import Text.Decker.Server.Types
import Text.Regex.TDFA hiding (empty)
import Web.Scotty.Trans

-- | Returns a JSON list of all existing WEBM video fragments for a recording
listRecordings :: AppActionM ()
listRecordings = do
  webm <- param "1"
  webms <- liftIO $ existingVideos webm
  json webms

-- | Accepts the upload of a recording fragment. Either appends to existing
-- recordings or replaces them.
uploadRecording :: Bool -> AppActionM ()
uploadRecording append = do
  destination <- param "1"
  chan <- reader channel
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && "-recording.webm" `List.isSuffixOf` destination
    then do
      tmp <- liftIO $ uniqueTransientFileName destination
      reader <- bodyReader
      liftIO $ writeBody tmp reader
      let operation = if append then Append tmp destination else Replace tmp destination
      atomically $ writeTChan chan (UploadComplete operation)
    else do
      text "ERROR: directory does not exist or file (suffix) is not uploadable"
      status status406

-- | Returns the list of video fragments under the same name.
existingVideos :: FilePath -> IO [FilePath]
existingVideos webm = do
  let [dir, file, ext] = map ($ webm) [takeDirectory, takeFileName . dropExtension, takeExtension]
  sort <$> globDir1 (compile $ file <> "*" <> ext) dir

-- Unique transient tmp filename
uniqueTransientFileName :: FilePath -> IO FilePath
uniqueTransientFileName base = do
  transient <- transientDir
  id <- toString <$> randomId
  return
    $ transient
    </> dropExtension (takeFileName base)
      <> "-"
      <> id
      <.> takeExtension base

writeBody :: FilePath -> IO ByteString -> IO ()
writeBody path reader = do
  chunk <- reader
  unless (BS.null chunk) $ do
    BS.appendFile path chunk
    writeBody path reader

-- | Converts a WEBM video file into an MP4 video file on the fast track. The
-- video stream is assumed to be H264 and is not transcoded. The audio is
-- transcoded to AAC in any case.
convertVideoMp4 :: FilePath -> FilePath -> IO ()
convertVideoMp4 webm mp4 = do
  putStrLn $ "# concat (" <> webm <> " -> " <> mp4 <> ")"
  runFfmpeg webm mp4
  where
    runFfmpeg src dst = do
      tmp <- uniqueTransientFileName dst
      let args = ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", tmp]
      putStrLn $ "# calling: ffmpeg " <> List.unwords args
      callProcess "ffmpeg" args
      renameFile tmp dst

-- | Converts a WEBM video file into an MP4 video file on the slow track. The audio is
-- transcoded to AAC.
transcodeVideoMp4 :: FilePath -> FilePath -> IO ()
transcodeVideoMp4 webm mp4 = do
  runFfmpeg webm mp4
  where
    runFfmpeg src dst = do
      tmp <- uniqueTransientFileName dst
      let args = ["-nostdin", "-v", "fatal", "-y", "-i", src] <> slow <> [tmp]
      putStrLn $ "# calling: ffmpeg " <> List.unwords args
      callProcess "ffmpeg" args
      renameFile tmp dst

-- Transcoding parameters
fast = ["-preset", "fast", "-vcodec", "copy"]

slow =
  [ "-pix_fmt",
    "yuv420p",
    "-crf",
    "27",
    "-preset",
    "slow",
    "-tune",
    "stillimage",
    "-ac",
    "1",
    "-movflags",
    "+faststart",
    "-vcodec",
    "libx264",
    "-r",
    "30",
    "-metadata",
    "comment=decker-crunched"
  ]

-- | Append the uploaded video to the list of already uploaded videos under the
--  same name and coverts the resulting concatenation to MP4. The same
--  transcoding as in `convertVideoMp4` is performed. webms is the list of the
--  uploaded WEBM videos, destination ist the original upload name.
--
-- Turns out the 'concat protocol' is not gonna cut it if stream parameters
-- differ even slightly. Must use the 'concat demuxer' which unfortunately
-- must transcode the video stream, which might take a while.
concatVideoMp4 :: [String] -> [FilePath] -> FilePath -> IO ()
concatVideoMp4 ffmpegArgs files mp4 = do
  let sorted = sort files
  listFile <- mkListFile sorted mp4
  putStrLn $ "# concat (" <> intercalate ", " sorted <> " -> " <> mp4 <> ")"
  concatVideoMp4' ffmpegArgs listFile mp4

mkListFile webms mp4 = do
  let listFile = mp4 <.> "list"
  writeFile listFile (List.unlines $ map (\f -> "file '../" <> f <> "'") webms)
  return listFile

concatVideoMp4' :: [String] -> FilePath -> FilePath -> IO ()
concatVideoMp4' ffmpegArgs listFile mp4 = do
  runFfmpeg listFile mp4
  where
    runFfmpeg listFile dst = do
      tmp <- uniqueTransientFileName dst
      let args =
            ["-nostdin", "-v", "warning", "-y", "-f", "concat", "-safe", "0", "-i", listFile]
              <> ffmpegArgs
              <> ["-acodec", "aac", tmp]
      putStrLn $ "# calling: ffmpeg " <> List.unwords args
      callProcess "ffmpeg" args
      renameFile tmp dst

-- | Atomically moves the transcoded upload into place.  All existing parts of
-- previous uploads are removed.
replaceVideoUpload :: Bool -> FilePath -> FilePath -> IO ()
replaceVideoUpload transcode upload webm = do
  webms <- existingVideos webm
  mapM_ removeFile webms
  let mp4 = replaceExtension webm ".mp4"
  when transcode $ convertVideoMp4 upload mp4
  renameFile upload webm

-- | Appends the uploaded WEBM video to potentially already existing fragments.
appendVideoUpload :: Bool -> FilePath -> FilePath -> IO ()
appendVideoUpload transcode upload webm = do
  let mp4 = replaceExtension webm ".mp4"
  existing <- existingVideos webm
  case existing of
    [] -> do
      replaceVideoUpload transcode upload webm
    [single] -> do
      let name0 = setSequenceNumber 0 webm
      let name1 = setSequenceNumber 1 webm
      renameFile single name0
      renameFile upload name1
      when transcode $ concatVideoMp4 fast [name0, name1] mp4
    multiple -> do
      let number = getHighestSequenceNumber multiple
      let name = setSequenceNumber (number + 1) webm
      renameFile upload name
      when transcode $ concatVideoMp4 fast (multiple <> [name]) mp4

-- | Sets the sequence number of a file. The number is appended to the base file
-- name just before the extension.
setSequenceNumber :: Int -> FilePath -> FilePath
setSequenceNumber n path =
  let pattern = "^(.*)-([0-9]+)(\\.[^.]+)$" :: String
      result = path =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [name, _, ext]) -> name <> "-" <> show n <> ext
        (_, _, _, _) -> dropExtension path <> "-" <> show n <> takeExtension path

-- | Returns the sequence number of a file, if there is one.
getSequenceNumber :: FilePath -> Maybe Int
getSequenceNumber name =
  let pattern = "^.*-([0-9]+)\\.[^.]+$" :: String
      result = name =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [num]) -> readMaybe num
        (_, _, _, _) -> Nothing

-- | Returns the highest sequence number of a list of files.
getHighestSequenceNumber :: [FilePath] -> Int
getHighestSequenceNumber files =
  let numbers = map getSequenceNumber files
   in foldl' max 0 (catMaybes numbers)
