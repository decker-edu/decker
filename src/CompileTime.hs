module CompileTime
  ( lookupGitBranch
  , lookupGitCommitId
  , lookupGitTag
  ) where

import Control.Monad
import Data.Maybe
import Data.String.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Exit
import System.Process

lookupGitBranch :: Q Exp
lookupGitBranch =
  (stringE . strip . fromMaybe "" <=< runIO . git)
    ["rev-parse", "--abbrev-ref", "HEAD"]

lookupGitCommitId :: Q Exp
lookupGitCommitId =
  (stringE . strip . fromMaybe "" <=< runIO . git)
    ["rev-parse", "--short", "HEAD"]

lookupGitTag :: Q Exp
lookupGitTag =
  (stringE . strip . fromMaybe "" <=< runIO . git)
    ["tag", "--points-at", "HEAD"]

git :: [String] -> IO (Maybe String)
git args = do
  (exitCode, stdout, _) <- readProcessWithExitCode "git" args ""
  return $
    case exitCode of
      ExitSuccess -> Just stdout
      _ -> Nothing
