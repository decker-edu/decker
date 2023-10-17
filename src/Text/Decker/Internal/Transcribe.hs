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
import Text.Decker.Internal.External (whisper)

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
      let vtt = replaceSuffix "-deck.html" "-recording.vtt" deck
      let mp4 = replaceSuffix "-deck.html" "-recording.mp4" source
      exists <- liftIO $ Dir.doesFileExist mp4
      when exists $ need [vtt]
  alternatives $ do
    publicDir <//> "*-recording.vtt" %> \out -> do
      let src = makeRelative publicDir out
      need [src]
      copyFileChanged src out
    "**/*-recording.vtt" %> \out -> do
      let mp4 = replaceSuffix "-recording.vtt" "-recording.mp4" out
      need [mp4]
      putNormal $ "# whisper (for " <> mp4 <> ")"
      meta <- getGlobalMeta
      let model ::String = lookupMetaOrElse "/usr/local/share/whisper.cpp/models/ggml-large.bin" "whisper.model" meta
      writeFile out model
      -- whisper ["-m", model, "-l", "auto"] Nothing
      putNormal "# TODO really call it"

