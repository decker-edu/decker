module Text.Decker.Project.Git
  ( gitOriginUrl
  , gitBranch
  , gitRevision
  , gitRevisionTag
  ) where

import Control.Exception
import qualified Data.Set as Set
import System.Process
import Data.Maybe

-- | Runs a git command an returns the output
git :: [String] -> IO (Maybe String)
git args = do
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  case code of
    ExitSuccess -> return $ Just stdout
    ExitFailure _ -> return Nothing

-- | Returns the Url for the remote 'origin' if that exists 
gitOriginUrl :: IO (Maybe String)
gitOriginUrl = git ["remote", "get-url", "origin"]

-- | Returns the name of the branch the current repo is on 
gitBranch :: IO (Maybe String)
gitBranch = git ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Returns the hexadecimal hash of the current revision, if that can be
-- determined 
gitRevision :: IO (Maybe String)
gitRevision = git ["rev-parse", "--verify", "HEAD"]

-- | Returns the first tag that is pinned to the current revision
gitRevisionTag :: IO (Maybe String)
gitRevisionTag = listToMaybe . lines <$> git ["name-rev", "--tags", "--name-only", "HEAD"]
