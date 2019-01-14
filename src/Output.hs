-- | This module gathers all functions that output something during runtime
module Output
  ( getResourceString
  , putCurrentDocument
  ) where

import Project
import Shake

import Development.Shake
import System.FilePath

-- CLEANUP: From Resources
getResourceString :: FilePath -> IO String
getResourceString path = do
  dataDir <- deckerResourceDir
  readFile (dataDir </> path)

-- CLEANUP: From Utilities
putCurrentDocument :: FilePath -> Action ()
putCurrentDocument out = do
  public <- publicA
  let rel = makeRelative public out
  putNormal $ "# pandoc (for " ++ rel ++ ")"
