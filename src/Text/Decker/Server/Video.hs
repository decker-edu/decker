{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Video where

import Control.Monad.Catch
import Control.Monad.State
import Data.List
import Data.Maybe
import Relude
import Snap.Core
import Snap.Util.FileUploads
import System.Directory
import System.FilePath.Glob
import System.FilePath.Posix
import System.Process
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
  ( DeckerException (InternalException),
  )
import Text.Regex.TDFA

-- | Expects a multi-part WEBM video file upload. The upload can contain multiple complete videos.
uploadVideo :: MonadSnap m => Bool -> m ()
uploadVideo append = do
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
                if "-recording.webm" `isSuffixOf` destination
                  then do
                    exists <- doesDirectoryExist (takeDirectory destination)
                    if exists
                      then do
                        if append
                          then do
                            appendVideoUpload tmp destination
                          else do
                            replaceVideoUpload tmp destination
                      else throwM $ InternalException $ "Upload directory does not exist: " <> takeDirectory destination
                  else throwM $ InternalException $ "Uploaded file is not a WEBM video: " <> destination
            )
            ( \e@(SomeException se) -> do
                putStrLn $ "# upload FAILED: " <> show se
                throwM e
            )
  return ()

-- | Converts a WEBM video file into an MP4 video file on the fast track. The
-- video stream is assumed to be H264 and is  not transcoded. The audio is
-- transcoded to AAC in any case. All existing parts of previous uploads are
-- removed.
convertVideoMp4 :: FilePath -> FilePath -> IO ()
convertVideoMp4 webm mp4 = do
  runFfmpeg webm mp4
  putStrLn $ "# ffmpeg (" <> webm <> " -> " <> mp4 <> ")"
  where
    runFfmpeg src dst = do
      let tmp = transientDir </> takeFileName dst
      callProcess
        "ffmpeg"
        ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", tmp]
      renameFile tmp dst

-- |  Append the uploaded video to the list of already uploaded videos under the
--  same name and coverts the resulting concatenation to MP4. The same
--  transcoding as in `convertVideoMp4` is performed. webms is the list of the
--  uploaded WEBM videos, destination ist the original upload name.
concatVideoMp4 :: [FilePath] -> FilePath -> IO ()
concatVideoMp4 webms mp4 = do
  let sorted = sort webms
  runFfmpeg sorted mp4
  putStrLn $ "# ffmpeg (" <> intercalate ", " sorted <> " -> " <> mp4 <> ")"
  where
    runFfmpeg srcs dst = do
      let tmp = transientDir </> takeFileName dst
      callProcess
        "ffmpeg"
        ["-nostdin", "-v", "fatal", "-y", "-i", "concat:" <> intercalate "|" srcs, "-vcodec", "copy", "-acodec", "aac", tmp]
      renameFile tmp dst

-- |  Returns the list of video fragments under the same name.
existingVideos :: FilePath -> IO [FilePath]
existingVideos webm = do
  let [dir, file, ext] = map ($ webm) [takeDirectory, takeFileName . dropExtension, takeExtension]
  globDir1 (compile $ file <> "*" <> ext) dir

-- | Atomically moves the transcoded upload into place.
replaceVideoUpload :: FilePath -> FilePath -> IO ()
replaceVideoUpload tmp destination = do
  let mp4 = replaceExtension destination ".mp4"
  convertVideoMp4 tmp mp4
  renameFile tmp destination
  existingVideos destination >>= mapM_ removeFile . filter (/= destination)

-- | Appends the uploaded WEBM video to potentially already existing fragments.
appendVideoUpload :: FilePath -> FilePath -> IO ()
appendVideoUpload upload destination = do
  let mp4 = replaceExtension destination ".mp4"
  existing <- existingVideos destination
  print existing
  case existing of
    [] -> do
      replaceVideoUpload upload destination
    [single] -> do
      let name0 = setSequenceNumber 0 destination
      let name1 = setSequenceNumber 1 destination
      renameFile single name0
      renameFile upload name1
      concatVideoMp4 [name0, name1] mp4
    multiple -> do
      let number = getHighestSequenceNumber multiple
      putStrLn $ "nth: " <> destination
      let name = setSequenceNumber (number + 1) destination
      renameFile upload name
      concatVideoMp4 (multiple <> [name]) mp4

-- | Sets the sequence number of a file. The number is appended to the base file
-- name just before the extension extension.
setSequenceNumber :: Int -> FilePath -> FilePath
setSequenceNumber n path =
  let pattern = "^(.*)-([0-9]+)(\\.[^.]+)$" :: String
      result = path =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [name, _, ext]) -> name <> "-" <> show n <> ext
        (_, _, _, _) -> dropExtension path <> "-" <> show n <> takeExtension path

-- |  Returns the sequence number of a file, if there is one.
getSequenceNumber :: FilePath -> Maybe Int
getSequenceNumber name =
  let pattern = "^.*-([0-9]+)\\.[^.]+$" :: String
      result = name =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [num]) -> readMaybe num
        (_, _, _, _) -> Nothing

-- |  Returns the highest sequence number of a list of files.
getHighestSequenceNumber :: [FilePath] -> Int
getHighestSequenceNumber files =
  let numbers = map getSequenceNumber files
   in foldl' max 0 (catMaybes numbers)

-- |  Allows upload sizes up to one GB.
fileUploadPolicy :: FileUploadPolicy
fileUploadPolicy = setMaximumFileSize (10 ^ 9) defaultFileUploadPolicy
