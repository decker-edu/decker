module Text.Decker.Project.Version
  ( deckerVersion
  , deckerGitBranch
  , deckerGitCommitId
  , deckerGitVersionTag
  , isDevelopmentVersion
  , versionCheck
  ) where

import Text.Decker.Internal.Meta

import Control.Monad
import Data.List.Extra as List
import Data.Maybe
import Data.Version (showVersion, versionBranch)
import Development.Shake
import Paths_decker (version)
import Text.Decker.Internal.CompileTime
import Text.Pandoc hiding (lookupMeta)
import Text.Read (readMaybe)
import Text.Regex.TDFA

-- | The version from the cabal file
deckerVersion :: String
deckerVersion = showVersion version

-- | Determines the git branch at compile time 
deckerGitBranch :: String
deckerGitBranch = $(lookupGitBranch)

-- | Determines the git branch at compile time 
deckerGitCommitId :: String
deckerGitCommitId = $(lookupGitCommitId)

-- | Determines the git tag at compile time 
deckerGitVersionTag :: String
deckerGitVersionTag = $(lookupGitTag)

-- | Regex that matches a version tag
tagRegex = "v([0-9]+)[.]([0-9]+)[.]([0-9]+)" :: String

-- | Returns the tagged version as an array of strings.
deckerGitVersionTag' :: [String]
deckerGitVersionTag' =
  case getAllTextSubmatches $ deckerGitVersionTag =~ tagRegex of
    [] -> []
    m:ms -> ms

isVersionTagMatching :: Bool
isVersionTagMatching =
  versionBranch version == mapMaybe readMaybe deckerGitVersionTag'

-- | Is this a development or a production branch? Release versions are cut from
-- the master branch and carry a version tag (vX.Y.Z) that matches the version
-- entry in `package.yaml`. Everything else is a development version.
isDevelopmentVersion :: Bool
isDevelopmentVersion = not (deckerGitBranch == "master" && isVersionTagMatching)

versionCheck :: Meta -> Action ()
versionCheck meta =
  unless isDevelopmentVersion $ do
    let version = lookupMeta "decker-version" meta
    case version of
      Just version -> check version
      _ ->
        putNormal $
        "  - Document version unspecified. This is decker version " ++
        deckerVersion ++ "."
  where
    check version =
      when (List.trim version /= List.trim deckerVersion) $
      putNormal $
      "  - Document version " ++
      version ++
      ". This is decker version " ++ deckerVersion ++ ". Expect problems."
