{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Helper
  ( dropSuffix
  , replaceSuffix
  , repeatIfTrue
  , whenTrue
  , unique
  , time
  , (<++>)
  , runIOQuietly
  , copyDir
  , copyFileIfNewer
  , fileIsNewer
  , handleLeft
  , handleLeftM
  , isDevelopmentRun
  , warnVersion
  , tryRemoveDirectory
  ) where

import Text.Decker.Internal.Exception
import Text.Decker.Project.Version

import Control.Monad.Catch
import Control.Monad.State
import qualified Data.List as List
import qualified Data.List.Extra as List
import qualified Data.Set as Set
import Relude
import System.CPUTime
import qualified System.Directory as Dir
import System.Environment
import System.FilePath
import System.Directory
import Text.Pandoc
import Text.Printf

runIOQuietly :: PandocIO a -> IO (Either PandocError a)
runIOQuietly act = runIO (setVerbosity ERROR >> act)

-- | Monadic version of list concatenation.
(<++>) :: Monad m => m [a] -> m [a] -> m [a]
(<++>) = liftM2 (++)

repeatIfTrue :: Monad m => m Bool -> m ()
repeatIfTrue action = do
  again <- action
  when again $ repeatIfTrue action

whenTrue :: Monad m => m Bool -> m () -> m ()
whenTrue bool action = do
  true <- bool
  when true action

-- | Removes the last suffix from a filename
dropSuffix :: String -> String -> String
dropSuffix s t = fromMaybe t (List.stripSuffix s t)

replaceSuffix :: String -> String -> String -> String
replaceSuffix srcSuffix targetSuffix filename =
  dropSuffix srcSuffix filename ++ targetSuffix

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

time :: String -> IO a -> IO a
time name action = do
  start <- getCPUTime
  result <- action
  stop <- getCPUTime
  let diff = fromIntegral (stop - start) / (10 ^ 12)
  printf "%s: %0.5f sec\n" name (diff :: Double)
  return result

-- | Copy a directory and its contents recursively
copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  unlessM (Dir.doesDirectoryExist src) $
    throwM (ResourceException "src does not exist or is not a directory")
  dstExists <- Dir.doesDirectoryExist dst
  if dstExists && (List.last (splitPath src) /= List.last (splitPath dst))
    then copyDir src (dst </> List.last (splitPath src))
    else do
      Dir.createDirectoryIfMissing True dst
      contents <- Dir.listDirectory src
      forM_ contents $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDirectory <- Dir.doesDirectoryExist srcPath
        if isDirectory
          then copyDir srcPath dstPath
          else copyFileIfNewer srcPath dstPath

-- | Copies the src to dst if src is newer or dst does not exist. Creates
-- missing directories while doing so.
copyFileIfNewer :: FilePath -> FilePath -> IO ()
copyFileIfNewer src dst =
  whenM (fileIsNewer src dst) $ do
    Dir.createDirectoryIfMissing True (takeDirectory dst)
    Dir.copyFile src dst

fileIsNewer :: FilePath -> FilePath -> IO Bool
fileIsNewer a b = do
  aexists <- Dir.doesFileExist a
  bexists <- Dir.doesFileExist b
  if bexists
    then if aexists
           then do
             at <- Dir.getModificationTime a
             bt <- Dir.getModificationTime b
             return (at > bt)
           else return False
    else return aexists

handleLeft :: ToText a => Either a b -> b
handleLeft (Right x) = x
handleLeft (Left e) = error $ toText e

handleLeftM :: (ToString a, MonadThrow m) => Either a b -> m b
handleLeftM (Right x) = return x
handleLeftM (Left e) = throwM $ InternalException $ toString e

-- | Finds out if the decker executable is located below the current directory.
-- This means most probably that decker was started in the decker development
-- project using `stack run decker`.
isDevelopmentRun :: IO Bool
isDevelopmentRun = do
  cwd <- Dir.getCurrentDirectory
  exePath <- getExecutablePath
  return $ cwd `isPrefixOf` exePath

warnVersion :: IO ()
warnVersion = do
  devRun <- isDevelopmentRun
  when (isDevelopmentVersion && not devRun) $
    printf
      "WARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag

tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory path = do
  exists <- doesDirectoryExist path
  when exists $ removeDirectoryRecursive path

