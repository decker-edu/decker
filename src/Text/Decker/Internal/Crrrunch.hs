{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Crrrunch where

import Control.Lens ((^.), (^?))
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Development.Shake
import Development.Shake.FilePath
import Relude
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.Exit
import System.FilePath.Glob (compile, globDir1)
import System.Process
import Text.Decker.Internal.Caches
import Text.Decker.Internal.Common
import Text.Decker.Internal.Helper (dropSuffix, replaceSuffix)
import Text.Decker.Project.ActionContext (ActionContext, globalMeta)
import Text.Decker.Project.Project
import Text.Decker.Server.Video

-- Checks if one of the sources is newer than the target file.
needsRebuild :: FilePath -> [FilePath] -> IO Bool
needsRebuild target sources = do
  putStrLn "\nchecking for newer video snippets"
  texists <- System.Directory.doesFileExist target
  if not texists
    then do
      putStrLn $ target <> " does not exist"
      return True
    else do
      tmod <- getModificationTime target
      putStrLn "sources:"
      needs <- forM sources $ \source -> do
        sexists <- System.Directory.doesFileExist source
        if not sexists
          then return False
          else do
            smod <- getModificationTime source
            putStrLn $ source <> ", " <> show smod
            return (tmod < smod)
      putStrLn "target:"
      putStrLn $ target <> ", " <> show tmod       
      return $ or needs

-- Replaces the Shake dependency nightmare with straight forward modtime checking.
crunchAllRecordings :: ActionContext -> IO ()
crunchAllRecordings context = do
  targets <- scanTargets (context ^. globalMeta)
  forM_ (Map.keys $ targets ^. decks) $ \deck -> do
    let recording = makeRelative publicDir $ replaceSuffix "-deck.html" "-recording.mp4" deck
    let source = makeRelative publicDir deck
    let pattern = dropSuffix "-deck.html" source <> "*.webm"
    webms <- globDir1 (compile pattern) "."
    let mp4s = map (`replaceExtension` ".mp4") webms
    unless (null webms) $ do
      forM_ webms $ \webm -> do
        let mp4 = replaceExtension webm ".mp4"
        whenM (needsRebuild mp4 [webm])
          $ do
            putStrLn $ "# crrrunch (transcode " <> webm <> " -> " <> mp4 <> ")"
            transcodeVideoMp4 webm mp4
      when (length mp4s > 1) $
        whenM (needsRebuild recording mp4s)
          $ do
            let list = recording <.> "list"
            writeFileChanged list (List.unlines $ map (\f -> "file '" <> takeFileName f <> "'") $ sort mp4s)
            putStrLn "# crrrunch (combine fast:"
            mapM_ (putStrLn . ("  " <>)) (sort mp4s)
            putStrLn "# )"
            concatVideoMp4' fast list recording
            removeFile list

-- | Rules for transcoding videos. Mp4 videos are recreated with higher
-- compression parameters if any of the recording fragments changed. Also, if
-- they have not yet been transcoded.
crunchRulesO :: Rules ()
crunchRulesO = do
  (getGlobalMeta, getDeps, getTemplate) <- prepCaches
  want ["mp4s"]
  phony "mp4s" $ do
    targets <- getDeps
    -- Need MP4 videos for each deck that has at least one WEBM fragment recorded.
    forM_ (Map.keys $ targets ^. decks) $ \deck -> do
      let source = makeRelative publicDir deck
      let pattern = dropSuffix "-deck.html" source <> "*.webm"
      webms <- getDirectoryFiles "" [pattern]
      unless (null webms) $ do
        need [replaceSuffix "-deck.html" "-recording.mp4" deck]
  alternatives $ do
    -- copy the crunched MP4 to public
    publicDir <//> "*-recording.mp4" %> \out -> do
      let src = makeRelative publicDir out
      need [src]
      putNormal $ "# copy recording (for " <> out <> ")"
      copyFileChanged src out
    -- crunch the WEBMs in the list if the list changed
    "**/*-recording.mp4" %> \out -> do
      let list = out <.> "list"
      need [list]
      putNormal $ "# ffmpeg (for " <> out <> ")"
      liftIO $ concatVideoMp4' slow list out
    -- compile the lost of WEBMs
    "**/*-recording.mp4.list" %> \out -> do
      alwaysRerun
      let pattern = dropSuffix ".mp4.list" out <> "*.webm"
      webms <- getDirectoryFiles "" [pattern]
      putNormal $ "# collect WEBMs (for " <> out <> ")"
      -- only write the list if it would change
      writeFileChanged out (List.unlines $ map (\f -> "file '" <> takeFileName f <> "'") $ sort webms)

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
