{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Watch
  ( waitForTwitchPassive
  ) where

-- | A non-polling file watcher based on fsnotify
import Control.Concurrent.MVar
import Data.List
import qualified Data.Set as Set
import Filter
import Render
import System.FSNotify
import System.FilePath

-- | Wait for something to happen on one of the matching files in one of the
-- supplied directories. TODO: Get rid of the twitchExtensions. Watch
-- everything, except the public dir.
waitForTwitch :: [FilePath] -> IO FilePath
waitForTwitch directories = do
  done <- newEmptyMVar
  mgr <- startManager
  stops <- watchIt mgr done
  filepath <- takeMVar done
  sequence_ stops
  stopManager mgr
  return filepath
    -- Match a filepath against the supplied patterns
  where
    isWatchedFile event =
      (takeExtension . eventPath) event `elem` twitchExtensions
    -- Stop the watch manager and notify the main thread
    stopWatching _ done event = putMVar done (eventPath event)
    -- Watch everything within the supplied dirs
    watchInDir mgr done dir =
      watchTree mgr dir isWatchedFile (stopWatching mgr done)
    watchIt mgr done = mapM (watchInDir mgr done) directories

commonExtensions :: [String]
commonExtensions = [".scss", ".css", ".md", ".yaml", ".png", ".gif", ".jpg", ".svg"]

twitchExtensions :: [String]
twitchExtensions =
  commonExtensions ++
  iframeExtensions ++
  audioExtensions ++ videoExtensions ++ renderedCodeExtensions

waitForTwitchPassive :: [FilePath] -> IO FilePath
waitForTwitchPassive files = do
  let dirs = unique (map takeDirectory files)
  waitForTwitch dirs

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList
