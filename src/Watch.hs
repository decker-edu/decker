{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Watch
  ( waitForTwitchPassive
  ) where

import Control.Concurrent.MVar

-- | A non-polling file watcher based on fsnotify
import Data.List
import System.FSNotify
import System.FilePath
import System.FilePath.Glob

-- | Wait for something to happen on one of the matching files 
-- in one of the supplied directories.
waitForTwitch :: [FilePath] -> [Pattern] -> IO FilePath
waitForTwitch directories patterns = do
  done <- newEmptyMVar
  mgr <- startManager
  stops <- watchIt mgr done
  filepath <- takeMVar done
  sequence_ stops
  stopManager mgr
  return filepath
        -- Match a filepath against the supplied patterns
  where
    isWatchedFile event = any (`match` eventPath event) patterns
        -- Stop the watch manager and notify the main thread
    stopWatching mgr done event = putMVar done (eventPath event)
        -- Watch everything within the supplied dirs
    watchInDir mgr done dir =
      watchTree mgr dir isWatchedFile (stopWatching mgr done)
    watchIt mgr done = mapM (watchInDir mgr done) directories

twitchPatterns =
  map compile ["**/*.md", "**/*.yaml", "**/*.png", "**/*.gif", "**/*.jpg", "**/*.mp4"]

waitForTwitchPassive files = do
  let dirs = nub (map takeDirectory files)
  waitForTwitch dirs twitchPatterns
