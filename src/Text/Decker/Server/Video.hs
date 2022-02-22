{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Server.Video where

import Control.Monad.Catch
import Control.Monad.State
import Data.List
import Data.Maybe
import Development.Shake (command)
import Development.Shake.FilePath (dropDirectory1)
import Relude
import Snap.Core
import Snap.Util.FileUploads
import System.Directory
import System.FilePath.Glob
import System.FilePath.Posix
import System.Process
import Text.Decker.Internal.Common
import Text.Decker.Internal.Exception
import Text.Regex.TDFA

-- | Expects a multi-part file upload.
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
                exists <- doesDirectoryExist (takeDirectory destination)
                if exists && "-recording.webm" `isSuffixOf` destination
                  then do
                    if append
                      then do
                        appendUpload tmp destination
                        concatVideoMp4 destination
                      else do
                        replaceUpload tmp destination
                        convertVideoMp4 destination
                    putStrLn $ "# upload received: " <> destination
                  else throwM $ InternalException "Illegal upload path suffix"
            )
            ( \e@(SomeException se) -> do
                putStrLn $ "# upload FAILED: " <> show se
                throwM e
            )
  return ()

convertVideoMp4 :: FilePath -> IO ()
convertVideoMp4 webm = do
  let dst = replaceExtension webm ".mp4"
  putStrLn $ "# ffmpeg (" <> webm <> " -> " <> dst <> ")"
  runFfmpeg webm dst
  where
    runFfmpeg src dst = do
      let tmp = transientDir </> takeFileName dst
      callProcess
        "ffmpeg"
        ["-nostdin", "-v", "fatal", "-y", "-i", src, "-vcodec", "copy", "-acodec", "aac", tmp]
      renameFile tmp dst

concatVideoMp4 :: FilePath -> IO ()
concatVideoMp4 webm = do
  let [dir, file, ext] = map ($ webm) [takeDirectory, takeFileName . dropExtension, takeExtension]
  existing <- globDir1 (compile $ file <> "*" <> ext) dir
  let dst = replaceExtension webm ".mp4"
  let sorted = sort existing
  putStrLn $ "# ffmpeg (" <> intercalate ", " sorted <> " -> " <> dst <> ")"
  runFfmpeg sorted dst
  where
    runFfmpeg srcs dst = do
      let tmp = transientDir </> takeFileName dst
      callProcess
        "ffmpeg"
        ["-nostdin", "-v", "fatal", "-y", "-i", "concat:" <> intercalate "|" srcs, "-vcodec", "copy", "-acodec", "aac", tmp]
      renameFile tmp dst

replaceUpload tmp destination = renameFile tmp destination >> return destination

appendUpload :: FilePath -> FilePath -> IO FilePath
appendUpload upload destination = do
  let [dir, file, ext] = map ($ destination) [takeDirectory, takeFileName . dropExtension, takeExtension]
  existing <- globDir1 (compile $ file <> "*" <> ext) dir
  print existing
  case existing of
    [] -> do
      renameFile upload destination
      return destination
    [single] -> do
      renameFile single (setSequenceNumber 0 destination)
      let name = setSequenceNumber 1 destination
      renameFile upload name
      return name
    multiple -> do
      let number = getHighestSequenceNumber multiple
      putStrLn $ "nth: " <> destination
      let name = setSequenceNumber (number + 1) destination
      renameFile upload name
      return name

setSequenceNumber :: Int -> FilePath -> FilePath
setSequenceNumber n path =
  let pattern = "^(.*)-([0-9]+)(\\.[^.]+)$" :: String
      result = path =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [name, _, ext]) -> name <> "-" <> show n <> ext
        (_, _, _, _) -> dropExtension path <> "-" <> show n <> takeExtension path

getSequenceNumber :: FilePath -> Maybe Int
getSequenceNumber name =
  let pattern = "^.*-([0-9]+)\\.[^.]+$" :: String
      result = name =~ pattern :: (String, String, String, [String])
   in case result of
        (_, _, _, [num]) -> readMaybe num
        (_, _, _, _) -> Nothing

getHighestSequenceNumber :: [FilePath] -> Int
getHighestSequenceNumber files =
  let numbers = map getSequenceNumber files
   in foldl' max 0 (catMaybes numbers)

fileUploadPolicy :: FileUploadPolicy
fileUploadPolicy = setMaximumFileSize (10 ^ 9) defaultFileUploadPolicy
