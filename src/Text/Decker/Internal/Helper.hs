{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Internal.Helper where

-- import Codec.FFmpeg
import Codec.Picture
import Control.Lens
import Control.Monad.Catch
import Control.Monad.State
import Data.Aeson.Lens
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.Scientific
import Data.Set qualified as Set
import Relude
import System.CPUTime
import System.Directory
import System.Directory qualified as Dir
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process
import Text.Decker.Internal.Exception
import Text.Decker.Project.Version
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

-- | Remove a file, but don't worry if it fails
removeFile_ :: FilePath -> IO ()
removeFile_ x = removeFile x `catch` \(SomeException e) -> return ()

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
          else do
            -- symlink safety first!
            removeFile_ dstPath
            Dir.copyFile srcPath dstPath

-- else copyFileIfNewer srcPath dstPath

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
    then
      if aexists
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
  progName <- getProgName
  cwd <- Dir.getCurrentDirectory
  exePath <- getExecutablePath
  return $ progName == "<interactive>" || (".stack-work") `List.isInfixOf` exePath

warnVersion :: IO ()
warnVersion = do
  devRun <- isDevelopmentRun
  when (isDevelopmentVersion && not devRun) $
    printf
      "\nWARNING: You are running a development build of decker (version: %s, branch: %s, commit: %s, tag: %s, build date: %s). Please be sure that you know what you're doing.\n"
      deckerVersion
      deckerGitBranch
      deckerGitCommitId
      deckerGitVersionTag
      deckerBuildDate

tryRemoveDirectory :: FilePath -> IO ()
tryRemoveDirectory path = do
  exists <- System.Directory.doesDirectoryExist path
  when exists $ removeDirectoryRecursive path

-- | Express the second path argument as relative to the first.
-- TODO: Ensure this always works with dirs
-- TODO: Ensure resulting dirs end on /
makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo dir file =
  let (d, f) = removeCommonPrefix (normalise dir, normalise file)
   in normalise $ invertPath d </> f

invertPath :: FilePath -> FilePath
invertPath fp = joinPath $ map (const "..") $ filter ("." /=) $ splitPath fp

removeCommonPrefix :: (FilePath, FilePath) -> (FilePath, FilePath)
removeCommonPrefix =
  mapTuple joinPath . removeCommonPrefix_ . mapTuple splitDirectories
  where
    removeCommonPrefix_ :: ([FilePath], [FilePath]) -> ([FilePath], [FilePath])
    removeCommonPrefix_ (al@(a : as), bl@(b : bs))
      | a == b = removeCommonPrefix_ (as, bs)
      | otherwise = (al, bl)
    removeCommonPrefix_ pathes = pathes

isPrefix :: FilePath -> FilePath -> Bool
isPrefix prefix whole = isPrefix_ (splitPath prefix) (splitPath whole)
  where
    isPrefix_ :: Eq a => [a] -> [a] -> Bool
    isPrefix_ (a : as) (b : bs)
      | a == b = isPrefix_ as bs
      | otherwise = False
    isPrefix_ [] _ = True
    isPrefix_ _ _ = False

mapTuple :: (t1 -> t) -> (t1, t1) -> (t, t)
mapTuple f (a, b) = (f a, f b)

putThrough :: (MonadIO m, Show a) => String -> a -> m a
putThrough info value = do
  liftIO $ putStrLn $ "  " <> info <> ": " <> show value
  return value

imageSize' :: DynamicImage -> (Int, Int)
imageSize' image =
  let w = dynamicMap imageWidth image
      h = dynamicMap imageHeight image
   in (w, h)

imageSize :: FilePath -> IO (Maybe (Int, Int))
imageSize path = do
  result <- readImage path
  case result of
    Left error -> do
      putStrLn $ "WARNING: cannot determine size, assuming default aspect ratio: " <> path
      return Nothing
    Right image -> do
      return $ Just $ imageSize' image

videoSize :: FilePath -> IO (Maybe (Int, Int))
videoSize path =
  handle handleError $ do
    (code, meta, error) <- readProcessWithExitCode "ffprobe" ["-v", "quiet", "-print_format", "json", "-show_streams", "-select_streams", "v:0", path] ""
    case code of
      ExitSuccess -> do
        let width = meta ^? key "streams" . nth 0 . key "width" . _Number
        let height = meta ^? key "streams" . nth 0 . key "height" . _Number
        case (width, height) of
          (Just w, Just h) -> return $ Just (truncate $ toRealFloat w, truncate $ toRealFloat h)
          _ -> return Nothing
      _ -> return Nothing
  where
    handleError (_ :: SomeException) = do
      putStrLn $ "WARNING: cannot run 'ffprobe', assuming default aspect ratio: " <> path
      return Nothing

-- videoSize :: FilePath -> IO (Maybe (Int, Int))
-- videoSize path = do
--   initFFmpeg
--   (getFrame, cleanup) <- imageReader (File path)
--   frame <- fmap ImageRGB8 <$> getFrame
--   case frame of
--     Just frame -> do
--       let w = dynamicMap imageWidth frame
--       let h = dynamicMap imageHeight frame
--       cleanup
--       return $ Just (w, h)
--     Nothing -> do
--       cleanup
--       return Nothing
