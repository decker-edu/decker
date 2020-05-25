module Text.Decker.Internal.CompileTime
  ( lookupGitBranch
  , lookupGitCommitId
  , lookupGitTag
  ) where

import Data.Maybe
import qualified Data.Text as Text
import Language.Haskell.TH
import Text.Decker.Project.Git

lookupGitBranch :: Q Exp
lookupGitBranch = stringE . Text.unpack .Text.strip . fromMaybe "none" =<< runIO gitBranch

lookupGitCommitId :: Q Exp
lookupGitCommitId = stringE . Text.unpack .Text.strip . fromMaybe "none" =<< runIO gitRevision

lookupGitTag :: Q Exp
lookupGitTag = stringE . Text.unpack .Text.strip . fromMaybe "none" =<< runIO gitRevisionTag
