{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Crrrunch where

import Control.Lens ((^.))
import Control.Monad
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Development.Shake
import Development.Shake.FilePath
import Relude
import System.Directory (doesFileExist, getModificationTime, removeFile)
import System.FilePath.Glob (compile, globDir1)
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
            concatVideoMp4' list recording
            removeFile list

