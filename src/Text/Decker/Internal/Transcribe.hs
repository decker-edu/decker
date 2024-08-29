{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unwords" #-}

module Text.Decker.Internal.Transcribe where

import Control.Lens ((^.))
import Control.Monad
import Data.Map.Strict qualified as Map
import Development.Shake
import Development.Shake.FilePath
import Relude
import System.Directory (removeFile)
import System.Directory qualified as Dir
import System.Process (readProcessWithExitCode)
import Text.Decker.Filter.Util (randomId)
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.Crrrunch (needsRebuild)
import Text.Decker.Internal.Helper (replaceSuffix)
import Text.Decker.Internal.Meta (lookupMetaOrElse, readMetaDataFile)
import Text.Decker.Project.ActionContext
import Text.Decker.Project.Project
import Text.Pandoc (Meta)
import Text.Pandoc.Builder (nullMeta)

-- | Rules for transcribiung videos. Mp4 videos are transcribed using
-- whisper.ccp if they have not yet been transcribed.
transcriptionRules :: Rules ()
transcriptionRules = do
  meta <- liftIO $ fromRight nullMeta <$> readMetaDataFile deckerMetaFile

  gpu <- newResource "GPU" 1
  (_, getDeps, _) <- prepCaches
  want ["vtts"]
  phony "vtts" $ do
    -- language of all the recordings
    let lang = lookupMetaOrElse "de" "whisper.lang" meta
    targets <- getDeps
    -- Need vtts transcriptions for each deck that has a MP4 video transcoded (chrunched).
    forM_ (Map.keys $ targets ^. decks) $ \deck -> do
      let source = makeRelative publicDir deck
      let vtten = replaceSuffix "-deck.html" "-recording-en.vtt" deck
      let vtt = replaceSuffix "-deck.html" ("-recording-" <> lang <> ".vtt") deck
      let mp4 = replaceSuffix "-deck.html" "-recording.mp4" source
      exists <- liftIO $ Dir.doesFileExist mp4
      -- translation to EN is only needed for non-EN recordings
      when exists $ need $ if lang == "en" then [vtten] else [vtten, vtt]
  -- these rules are tried in order
  alternatives $ do
    -- copies the all transcriptions to public
    publicDir <//> "*-recording-*.vtt" %> \out -> do
      let src = makeRelative publicDir out
      need [src]
      copyFileChanged src out
    -- transcribes to EN, translation is used for non-EN languages
    "**/*-recording-en.vtt" %> \out -> do
      let mp4 = replaceSuffix "-recording-en.vtt" "-recording.mp4" out
      need [mp4]
      let lang :: String = lookupMetaOrElse "de" "whisper.lang" meta
      -- avoid context switches on the GPU
      withResource gpu 1 $ do
        liftIO $ transcribe meta mp4 out lang (lang /= "en")
    -- transcribes to recorded language without translation.
    "**/*-recording-*.vtt" %> \out -> do
      let lang = lookupMetaOrElse "de" "whisper.lang" meta
      let mp4 = replaceSuffix ("-recording-" <> lang <> ".vtt") "-recording.mp4" out
      need [mp4]
      -- avoid context switches on the GPU
      withResource gpu 1 $ do
        liftIO $ transcribe meta mp4 out lang False

-- Replaces the Shake dependency nightmare with straight forward modtime checking.
transcribeAllRecordings :: ActionContext -> IO ()
transcribeAllRecordings context = do
  -- language of all the recordings
  let meta = context ^. globalMeta
  let lang = lookupMetaOrElse "de" "whisper.lang" meta
  targets <- scanTargets meta
  -- Need vtts transcriptions for each deck that has a MP4 video transcoded (chrunched).
  forM_ (Map.keys $ targets ^. decks) $ \deck -> do
    let source = makeRelative publicDir deck
    let vtten = replaceSuffix "-deck.html" "-recording-en.vtt" source
    let vtt = replaceSuffix "-deck.html" ("-recording-" <> lang <> ".vtt") source
    let mp4 = replaceSuffix "-deck.html" "-recording.mp4" source
    exists <- liftIO $ Dir.doesFileExist mp4
    -- translation to EN is only needed for non-EN recordings
    when exists $ do
      if lang == "en"
        then do
          transcribe meta mp4 vtten lang False
        else do
          transcribe meta mp4 vtten lang True
          transcribe meta mp4 vtt lang False

transcribe :: Meta -> FilePath -> String -> String -> Bool -> IO ()
transcribe meta mp4 vtt lang translate = do
  whenM (needsRebuild vtt [mp4]) $ do
    let baseDir = lookupMetaOrElse "/usr/local/share/whisper.cpp" "whisper.base-dir" meta
    let model = baseDir </> lookupMetaOrElse "models/ggml-large.bin" "whisper.model" meta
    id9 <- toString <$> liftIO randomId
    transient <- transientDir
    let wav = transient </> takeFileName mp4 <> "-" <> id9 <.> "wav"
    putStrLn $ "# whisper (for " <> vtt <> ")"

    let ffmpegOptions = lookupMetaOrElse ["-af", "speechnorm"] "whisper.options.ffmpeg" meta
    let ffmpegArgs = ["-y", "-i", mp4, "-acodec", "pcm_s16le", "-ac", "1", "-ar", "16000"] <> ffmpegOptions <> [wav]
    -- putStrLn $ "ffmpeg " <> intercalate " " ffmpegArgs
    call "ffmpeg" ffmpegArgs

    let selector = toText $ if translate then "translate" else lang
    let options = lookupMetaOrElse ["-bs", "5", "-mc", "0"] ("whisper.options." <> selector) meta
    let translateOption = ["--translate" | translate]
    let whisperArgs = ["--file", wav, "-m", model, "--language", lang] <> translateOption <> options <> ["--output-vtt", "--output-file", dropExtension vtt]

    let whisper = baseDir </> "main"
    -- putStrLn $ whisper <> intercalate " " whisperArgs
    call whisper whisperArgs

    -- putStrLn $ "rm " <> wav
    liftIO $ removeFile wav

call cmd args = do
  (code, out, err) <- readProcessWithExitCode cmd args ""
  return ()
  -- putStrLn err
  -- putStrLn out

