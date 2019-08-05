module Text.Decker.Internal.CompileTime
  ( lookupGitBranch
  , lookupGitCommitId
  , lookupGitTag
  ) where

import Data.Maybe
import Data.String.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Text.Decker.Project.Git

lookupGitBranch :: Q Exp
lookupGitBranch = stringE . strip . fromMaybe "none" =<< runIO gitBranch

lookupGitCommitId :: Q Exp
lookupGitCommitId = stringE . strip . fromMaybe "none" =<< runIO gitRevision

lookupGitTag :: Q Exp
lookupGitTag = stringE . strip . fromMaybe "none" =<< runIO gitRevisionTag
