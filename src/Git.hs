module Git
  ( gitOriginUrl
  , gitBranch
  , gitRevision
  , gitRevisionTag
  ) where

import Control.Exception
import Data.Git
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Revision
import Data.Maybe
import qualified Data.Set as Set

-- | Returns the Url for the remote 'origin' if that exists 
gitOriginUrl :: IO (Maybe String)
gitOriginUrl =
  handle (\(SomeException e) -> return Nothing) $
  withCurrentRepo (\repo -> configGet repo "remote \"origin\"" "url")

-- | Returns the name of the branch the current repo is on 
gitBranch :: IO (Maybe String)
gitBranch =
  handle (\(SomeException e) -> return Nothing) $
  withCurrentRepo
    (\repo -> do
       head <- headGet repo
       return $
         case head of
           Left ref -> Nothing
           Right (RefName name) -> Just name)

-- | Returns the hexadecimal hash of the current revision, if that can be
-- determined 
gitRevision :: IO (Maybe String)
gitRevision =
  handle (\(SomeException e) -> return Nothing) $
  withCurrentRepo
    (\repo -> do
       head <- headGet repo
       case head of
         Left ref -> return $ Just $ toHexString ref
         Right (RefName name) ->
           fmap (take 7 . toHexString) <$>
           resolveRevision repo (Revision name []))

-- | Returns the first tag that is pinned to the current revision
gitRevisionTag :: IO (Maybe String)
gitRevisionTag =
  handle (\(SomeException e) -> return Nothing) $
  withCurrentRepo
    (\repo -> do
       head <- headGet repo
       commitRef <-
         case head of
           Left ref -> return $ Just ref
           Right name -> resolveRevisionNamed repo name
       tagNames <- Set.toList <$> tagList repo
       tagRefs <- mapM (resolveRevisionNamed repo) tagNames
       return $ lookup commitRef (zip tagRefs (map refNameRaw tagNames)))
  where
    resolveRevisionNamed repo (RefName name) =
      resolveRevision repo (Revision name [])
