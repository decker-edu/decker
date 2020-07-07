{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module Text.Decker.Project.Glob
  ( fastGlobFiles
  , fastGlobDirs
  , globFiles
  , subDirs
  ) where

import Control.Monad
import Data.List
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath.Posix

-- | Glob for files a little more efficiently. 'exclude' contains a list of
-- directories that will be culled from the traversal. Hidden directories are
-- ignored. 'suffixes' is the list of file suffixes that are included in the
-- glob.
fastGlobFiles :: [String] -> [String] -> FilePath -> IO [FilePath]
fastGlobFiles exclude suffixes root = sort . map normalise <$> glob root
  where
    absExclude = map (root </>) exclude
    absListDirectory dir =
      map (dir </>) . filter (not . isPrefixOf ".") <$> listDirectory dir
    glob :: FilePath -> IO [String]
    glob root = do
      dirExists <- doesDirectoryExist root
      fileExists <- doesFileExist root
      if | dirExists -> globDir root
         | fileExists -> globFile root
         | otherwise -> return []
    globFile :: String -> IO [String]
    globFile file =
      if null suffixes || any (`isSuffixOf` file) suffixes
        then return [file]
        else return []
    globDir :: FilePath -> IO [String]
    globDir dir =
      if dir `elem` absExclude
        then return []
        else concat <$> (absListDirectory dir >>= mapM glob)

-- | Glob for directories efficiently. 'exclude' contains a list of directories
-- (relative to 'root') that will be culled from the traversal.
fastGlobDirs :: [String] -> FilePath -> IO [FilePath]
fastGlobDirs exclude root = map normalise <$> glob root
  where
    absExclude = map (root </>) exclude
    absListDirectory dir =
      map (dir </>) . filter (not . isPrefixOf ".") <$> listDirectory dir
    glob dir = do
      dirExists <- doesDirectoryExist dir
      if dirExists && notElem dir absExclude
        then (dir :) <$> (concat <$> (absListDirectory dir >>= mapM glob))
        else return []

-- | Same as `fastGlobFiles` but groups results by file extension.
globFiles :: [String] -> [String] -> FilePath -> IO [(String, [FilePath])]
globFiles exclude suffixes root = do
  scanned <- fastGlobFiles exclude suffixes root
  return $
    foldl
      (\alist suffix -> (suffix, filter (isSuffixOf suffix) scanned) : alist)
      []
      suffixes

-- | Get a list of all sub dirs.
subDirs :: FilePath -> IO [FilePath]
subDirs dir = do
  all <- listDirectory dir
  let nodot = filter (not . isPrefixOf ".") all
  filterM doesDirectoryExist nodot
