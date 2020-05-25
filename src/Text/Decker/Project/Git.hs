module Text.Decker.Project.Git
  ( gitOriginUrl
  , gitBranch
  , gitRevision
  , gitRevisionTag
  ) where

import Data.Maybe
import qualified Data.Text as Text
import System.Exit
import System.Process

-- | Runs a git command an returns the output
git :: [String] -> IO (Maybe Text.Text)
git args = do
  (code, stdout, stderr) <- readProcessWithExitCode "git" args ""
  case code of
    ExitSuccess -> return $ Just $ Text.pack stdout
    ExitFailure _ -> return Nothing

-- | Returns the Url for the remote 'origin' if that exists 
gitOriginUrl :: IO (Maybe Text.Text)
gitOriginUrl = git ["remote", "get-url", "origin"]

-- | Returns the name of the branch the current repo is on 
gitBranch :: IO (Maybe Text.Text)
gitBranch = git ["rev-parse", "--abbrev-ref", "HEAD"]

-- | Returns the hexadecimal hash of the current revision, if that can be
-- determined 
gitRevision :: IO (Maybe Text.Text)
gitRevision = git ["rev-parse", "--verify", "HEAD"]

-- | Returns the first tag that is pinned to the current revision
gitRevisionTag :: IO (Maybe Text.Text)
gitRevisionTag = do
  tags <- git ["name-rev", "--tags", "--name-only", "HEAD"]
  return $ tags >>= (listToMaybe . Text.lines)
