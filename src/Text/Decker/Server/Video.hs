{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Video where

import Control.Concurrent.STM (TChan, writeTChan)
import Control.Lens ((^.))
import Control.Monad.Catch
import Control.Monad.State
import qualified Data.List as List
import Data.Maybe
import Data.Aeson
import qualified Data.Set as Set
import Relude
import Snap.Core
import Snap.Util.FileUploads
import System.Directory
import System.FilePath.Glob
import System.FilePath.Posix
import System.IO.Streams (connect, withFileAsOutput)
import System.Process
import Text.Decker.Filter.Local (randomId)
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
  ( DeckerException (InternalException),
  )
import Text.Decker.Project.ActionContext
import Text.Decker.Server.Types
import Text.Regex.TDFA

-- | Expects a multi-part WEBM video file upload. The upload can contain multiple complete videos.
uploadVideo :: MonadSnap m => TChan ActionMsg -> Bool -> m ()
uploadVideo channel append = do
  withTemporaryStore transientDir "upload-" $ \store -> do
    (inputs, files) <-
      handleFormUploads defaultUploadPolicy fileUploadPolicy (const store)
    liftIO $
      forM_ files $
        \(FormFile path tmp) ->
          catch
            ( do
                let destination = dropDrive $ decodeUtf8 path
                putStrLn $ "# upload received: " <> tmp <> " for: " <> destination
                if "-recording.webm" `List.isSuffixOf` destination
                  then do
                    exists <- doesDirectoryExist (takeDirectory destination)
                    if exists
                      then do
                        let operation = if append then Append tmp destination else Replace tmp destination
                        atomically $ writeTChan channel (UploadComplete operation)
                      else throwM $ InternalException $ "Upload directory does not exist: " <> takeDirectory destination
                  else throwM $ InternalException $ "Uploaded file is not a WEBM video: " <> destination
            )
            ( \e@(SomeException se) -> do
                putStrLn $ "# upload FAILED: " <> show se
                throwM e
            )
  return ()

listRecordings :: MonadSnap m =>  m ()
listRecordings = do
    webm <- decodeUtf8 <$> getsRequest rqPathInfo
    webms <- liftIO $ existingVideos webm
    modifyResponse $ setContentType "application/json"
    writeBS $ fromLazy $ encode webms

-- Unique transient tmp filename
uniqueTransientFileName :: FilePath -> IO FilePath
uniqueTransientFileName base = do
  id <- toString <$> randomId
  return $
    transientDir
      </> dropExtension (takeFileName base)
      <> "-"
      <> id
      <.> takeExtension base

withUploadCheck :: TMVar (Set FilePath) -> FilePath -> (Bool -> Snap ()) -> Snap ()
withUploadCheck registry path action = do
  reg <- atomically $ takeTMVar registry
  putStrLn $ "withUploadCheck: " <> show reg
  let uploading = Set.member path reg
  atomically $ putTMVar registry (Set.insert path reg)
  putStrLn $ "withUploadCheck: calling action"
  if uploading
    then action True
    else action False
  putStrLn $ "withUploadCheck: action done."
  atomically $ do
    reg <- takeTMVar registry
    putTMVar registry (Set.delete path reg)

uploadRecording :: ActionContext -> Bool -> Snap ()
uploadRecording context append = do
  let channel = context ^. actionChan
  destination <- decodeUtf8 <$> getsRequest rqPathInfo
  exists <- liftIO $ doesDirectoryExist (takeDirectory destination)
  if exists && "-recording.webm" `List.isSuffixOf` destination
    then do
      tmp <- liftIO $ uniqueTransientFileName destination
      runRequestBody (withFileAsOutput tmp . connect)
      let operation = if append then Append tmp destination else Replace tmp destination
      atomically $ writeTChan channel (UploadComplete operation)
      modifyResponse $ setResponseCode 200
    else modifyResponse $ setResponseStatus 500 "Illegal path or suffix"

-- | Converts a WEBM video file into an MP4 video file on the fast track. The
-- video stream is assumed to be H264 and is  not transcoded. The audio is
-- transcoded to AAC in any case.
convertVideoMp4 :: FilePath -> FilePath -> IO ()
convertVideoMp4 webm mp4 = do
  putStrLn $ "# ffmpeg (" <> webm <> " -> " <> mp4 <> ")"
  runFfmpeg webm mp4
  where
    runFfmpeg src dst = do
      tmp <- uniqueTransientFileName dst
      let args = ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", tmp]
      putStrLn $ "# calling: ffmpeg " <> List.unwords args
      callProcess "ffmpeg" args
      renameFile tmp dst

-- Transcoding parameters
fast = ["-preset", "fast", "-vcodec", "copy"]

slow = ["-pix_fmt", "yuv420p", "-crf", "27", "-preset", "veryslow", "-tune", "stillimage", "-ac", "1", "-movflags", "+faststart", "-vcodec", "libx264", "-r", "30", "-metadata", "comment=decker-crunched"]

-- | Append the uploaded video to the list of already uploaded videos under the
--  same name and coverts the resulting concatenation to MP4. The same
--  transcoding as in `convertVideoMp4` is performed. webms is the list of the
--  uploaded WEBM videos, destination ist the original upload name.
--
-- Turns out the 'concat protocol' is not gonna cut it if stream parameters
-- differ even slightly. Must use the 'concat demuxer' which unfortunately
-- must transcode the video stream, which might take a while.
concatVideoMp4 :: [String] -> [FilePath] -> FilePath -> IO ()
concatVideoMp4 ffmpegArgs webms mp4 = do
  let sorted = sort webms
  listFile <- uniqueTransientFileName (mp4 <.> "list")
  writeFile listFile (List.unlines $ map (\f -> "file '../" <> f <> "'") sorted)
  putStrLn $ "# ffmpeg (" <> intercalate ", " sorted <> " -> " <> mp4 <> ")"
  runFfmpeg listFile mp4
  where
    runFfmpeg listFile dst = do
      tmp <- uniqueTransientFileName dst
      let args = ["-nostdin", "-v", "fatal", "-y", "-f", "concat", "-safe", "0", "-i", listFile] <> ffmpegArgs <> ["-acodec", "aac", tmp]
      putStrLn $ "# calling: ffmpeg " <> List.unwords args
      callProcess "ffmpeg" args
      renameFile tmp dst

-- | Returns the list of video fragments under the same name. It include is True
-- the actually uploaded file is included in the list.
existingVideos :: FilePath -> IO [FilePath]
existingVideos webm = do
  let [dir, file, ext] = map ($ webm) [takeDirectory, takeFileName . dropExtension, takeExtension]
  sort <$> globDir1 (compile $ file <> "*" <> ext) dir

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

-- | Allows upload sizes up to one GB.
fileUploadPolicy :: FileUploadPolicy
fileUploadPolicy = setMaximumFileSize (10 ^ 9) defaultFileUploadPolicy
