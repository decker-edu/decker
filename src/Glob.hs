{-- Author: Henrik Tramberend <henrik@tramberend.de> --}
module Glob
  ( fastGlobFiles
  , fastGlobDirs
  , globFiles
  ) where

import Data.List
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  )
import System.FilePath

-- | Glob for files a little more efficiently. 'exclude' contains a list of
-- directories that will be culled from the traversal. Hidden directories are
-- ignored .'suffixes' is the list of file suffixes that are included in the
-- glob.
fastGlobFiles :: [String] -> [String] -> FilePath -> IO [FilePath]
fastGlobFiles exclude suffixes root = glob root
  where
    absExclude = map (root </>) exclude
    absListDirectory dir =
      map (dir </>) . filter (not . isPrefixOf ".") <$> listDirectory dir
    glob root = do
      dirExists <- doesDirectoryExist root
      fileExists <- doesFileExist root
      if | dirExists -> globDir root
         | fileExists -> globFile root
         | otherwise -> return []
    globFile file =
      if any (`isSuffixOf` file) suffixes
        then return [file]
        else return []
    globDir dir =
      if dir `elem` absExclude
        then return []
        else concat <$> (absListDirectory dir >>= mapM glob)

-- | Glob for directories efficiently. 'exclude' contains a list of directories
-- (relative to 'root') that will be culled from the traversal.
fastGlobDirs :: [String] -> FilePath -> IO [FilePath]
fastGlobDirs exclude root = glob root
  where
    absExclude = map (root </>) exclude
    absListDirectory dir =
      map (dir </>) . filter (not . isPrefixOf ".") <$> listDirectory dir
    glob dir = do
      dirExists <- doesDirectoryExist dir
      if dirExists && notElem dir absExclude
        then (dir :) <$> (concat <$> (absListDirectory dir >>= mapM glob))
        else return []

globFiles :: [String] -> [String] -> FilePath -> IO [(String, [FilePath])]
globFiles exclude suffixes root = do
  scanned <- fastGlobFiles exclude suffixes root
  return $
    foldl
      (\alist suffix -> (suffix, filter (isSuffixOf suffix) scanned) : alist)
      []
      suffixes
