module CompileTime
  ( lookupGitBranch
  ) where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Exit
import System.Process
import Control.Monad
import Data.String.Utils

lookupGitBranch :: Q Exp
lookupGitBranch = do
  (stringE . strip. fromJust <=< runIO . git) ["rev-parse", "--abbrev-ref", "HEAD"] 

git :: [String] -> IO (Maybe String)
git args = do
  (exitCode, stdout, _) <- readProcessWithExitCode "git" args ""
  return $
    case exitCode of
      ExitSuccess -> Just stdout
      _ -> Nothing
