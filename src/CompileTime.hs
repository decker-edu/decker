module CompileTime
  ( lookupGitBranch
  , lookupGitCommitId
  , lookupGitTag
  ) where

import Control.Monad
import Data.Maybe
import Data.String.Utils
import Git
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Exit
import System.Process

lookupGitBranch :: Q Exp
lookupGitBranch = stringE . strip . fromMaybe "" =<< runIO gitBranch

lookupGitCommitId :: Q Exp
lookupGitCommitId = stringE . strip . fromMaybe "" =<< runIO gitRevision

lookupGitTag :: Q Exp
lookupGitTag = stringE . strip . fromMaybe "" =<< runIO gitRevisionTag
