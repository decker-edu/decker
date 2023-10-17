{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Transcribe where

import Control.Lens ((^.))
import Control.Monad
import qualified Data.Map.Strict as Map
import Development.Shake
import Development.Shake.FilePath
import Relude
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper (replaceSuffix)
import Text.Decker.Project.Project
import qualified System.Directory as Dir
import Codec.Picture.Metadata (lookup)
import Text.Decker.Internal.Meta (lookupMetaOrElse)
import System.Process (callCommand)

-- | Rules for transcribiung videos. Mp4 videos are transcribed using
-- whisper.ccp if they have not yet been transcribed.
transcriptionRules :: Rules ()
transcriptionRules = do
  (getGlobalMeta, getDeps, getTemplate) <- prepCaches
  want ["vtts"]
  phony "vtts" $ do
    targets <- getDeps
    -- Need vtts transcriptions for each deck that has a MP4 video transcoded (chrunched).
    forM_ (Map.keys $ targets ^. decks) $ \deck -> do
      let source = makeRelative publicDir deck
      let vtten = replaceSuffix "-deck.html" "-recording-en.vtt" deck
      let vttde = replaceSuffix "-deck.html" "-recording-de.vtt" deck
      let mp4 = replaceSuffix "-deck.html" "-recording.mp4" source
      exists <- liftIO $ Dir.doesFileExist mp4
      when exists $ need [vtten, vttde]
  alternatives $ do
    publicDir <//> "*-recording-*.vtt" %> \out -> do
      let src = makeRelative publicDir out
      need [src]
      copyFileChanged src out
    "**/*-recording-en.vtt" %> \out -> do
      meta <- getGlobalMeta
      let mp4 = replaceSuffix "-recording-en.vtt" "-recording.mp4" out
      transcribe meta mp4 out "en"
    "**/*-recording-de.vtt" %> \out -> do
      meta <- getGlobalMeta
      let mp4 = replaceSuffix "-recording-de.vtt" "-recording.mp4" out
      transcribe meta mp4 out "de"

transcribe meta mp4 vtt lang = do 
      let baseDir ::String = lookupMetaOrElse "/usr/local/share/whisper.cpp" "whisper.base-dir" meta
      need [mp4]
      putNormal $ "# whisper (for " <> vtt <> ")"
      whisper baseDir lang mp4 vtt

whisperPath :: FilePath
whisperPath = transientDir </> "whisper.sh"

whisper :: [Char] -> [Char] -> [Char] -> [Char] -> Action ()
whisper baseDir lang mp4 vtt = do
  let command = intercalate " " $ [whisperPath, baseDir, lang, mp4, vtt]
  putNormal $ "# " ++ command
  liftIO $ callCommand command 
