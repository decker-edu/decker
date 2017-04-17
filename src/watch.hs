module Watch (waitForTwitchPassive) where
    
-- | A non-polling file watcher based on fsnotify

import Data.List
import System.FilePath
import System.FilePath.Glob 
import Control.Concurrent.MVar
import System.FSNotify

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
    where
        -- Match a filepath against the supplied patterns
        isWatchedFile event =
            any ((flip match) (eventPath event)) patterns
        -- Stop the watch manager and notify the main thread
        stopWatching mgr done event = do
            putMVar done (eventPath event)
        -- Watch everything within the supplied dirs
        watchInDir mgr done dir = watchTree mgr dir isWatchedFile (stopWatching mgr done)
        watchIt mgr done = do
            mapM (watchInDir mgr done) directories
            

twitchPatterns = map compile ["**/*.md", "**/*.yaml", "**/*.png", "**/*.jpg"]

waitForTwitchPassive files = do
    let dirs = nub (map takeDirectory files)
    waitForTwitch dirs twitchPatterns






