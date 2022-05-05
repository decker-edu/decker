{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Crunch where

import Control.Lens ((^.), (^?))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.List as List
import Development.Shake
import Development.Shake.FilePath
import Relude
import System.Exit
import System.Process
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper (replaceSuffix)
import Text.Decker.Project.Project
import Text.Decker.Server.Video

-- | Rules for transcoding videos. Mp4 videos are recreated with higher
-- compression parameters if any of the recording fragments changed. Also, if
-- they have not yet been transcoded.
crunchRules :: Rules ()
crunchRules = do
  (getGlobalMeta, getTargets, getTemplate) <- prepCaches
  want ["mp4s"]
  phony "mp4s" $ do
    alwaysRerun
    targets <- getTargets
    forM_ (targets ^. decks) $ \deck -> do
      let source = makeRelative publicDir deck
      webms <- liftIO $ existingVideos (replaceSuffix "-deck.html" "-recording.webm" source)
      unless (null webms) $ do
        let publicMp4 = replaceSuffix "-deck.html" "-recording.mp4" deck
        need [publicMp4]
  alternatives $ do
    publicDir <//> "*-recording.mp4" %> \out -> do
      let src = makeRelative publicDir out
      putNormal $ "# copy recording (for " <> out <> ")"
      copyFile' src out
    "**/*-recording.mp4" %> \out -> do
      let list = (out <.> "list")
      need [list]
      liftIO $ putStrLn $ "# ffmpeg ( for " <> out <> ")"
      liftIO $ concatVideoMp4' slow list out
    "**/*-recording.mp4.list" %> \out -> do
      alwaysRerun
      let src = replaceSuffix ".mp4.list" ".webm" out
      webms <- liftIO $ existingVideos src
      let sorted = sort webms
      writeFileChanged out (List.unlines $ map (\f -> "file '" <> takeFileName f <> "'") sorted)
      need webms

-- | Reads the 'comment' meta data field from the video container. Return True
-- if the value is 'decker-crunched', False otherwise.
wasCrunched :: FilePath -> IO Bool
wasCrunched mp4 = do
  (code, stdout, stderr) <- readProcessWithExitCode "ffprobe" (["-print_format", "json", "-show_format"] <> [mp4]) ""
  case code of
    ExitFailure _ -> return False
    ExitSuccess -> do
      let json :: Maybe Value = decode $ encodeUtf8 stdout
      case json of
        Nothing -> return False
        Just json -> do
          let comment = json ^? (key "format" . key "tags" . key "comment" . _String)
          return (comment == Just "decker-crunched")
