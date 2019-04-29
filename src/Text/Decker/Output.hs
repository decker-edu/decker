-- | This module gathers all functions that output something during runtime
module Text.Decker.Output
  ( getResourceString
  ) where

import Text.Decker.Project.Project

import Development.Shake
import System.FilePath

getResourceString :: FilePath -> IO String
getResourceString path = do
  dataDir <- deckerResourceDir
  readFile (dataDir </> path)
