module CompileTime
  ( lookupGitBranch
  ) where

import Control.Monad
import Data.Maybe
import Data.String.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Exit
import System.Process

lookupGitBranch :: Q Exp
lookupGitBranch = do
  (stringE . strip . fromMaybe "" <=< runIO . git)
    ["rev-parse", "--abbrev-ref", "HEAD"]

git :: [String] -> IO (Maybe String)
git args = do
  (exitCode, stdout, _) <- readProcessWithExitCode "git" args ""
  return $
    case exitCode of
      ExitSuccess -> Just stdout
      _ -> Nothing
