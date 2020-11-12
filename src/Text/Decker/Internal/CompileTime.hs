{-# LANGUAGE OverloadedStrings #-}

module Text.Decker.Internal.CompileTime
    ( lookupGitBranch,
      lookupGitCommitId,
      lookupGitTag,
      lookupBuildDate,
    )
where

import Data.Maybe
-- import Data.String.Utils
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Language.Haskell.TH
import Text.Decker.Project.Git

lookupGitBranch :: Q Exp
lookupGitBranch = stringE . Text.unpack . Text.strip . fromMaybe "none" =<< runIO gitBranch

lookupGitCommitId :: Q Exp
lookupGitCommitId = stringE . Text.unpack . Text.strip . fromMaybe "none" =<< runIO gitRevision

lookupBuildDate :: Q Exp
lookupBuildDate =
    do
        date <- runIO (utctDay <$> getCurrentTime)
        let temp = iso8601Show date
        stringE $ Text.unpack $ Text.strip $ Text.pack temp

lookupGitTag :: Q Exp
lookupGitTag = stringE . Text.unpack . Text.strip . fromMaybe "none" =<< runIO gitRevisionTag
