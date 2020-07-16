module Text.Decker.Resource.Zip
  ( extractResourceEntries
  , extractResourceEntry
  , extractResourceEntryList
  , extractEntry
  , extractSubEntries
  , extractEntryList
  ) where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import Data.Map.Strict (filterWithKey, keys)
import qualified System.Directory as Dir
import System.Environment
import qualified System.FilePath as FP
import System.FilePath.Posix

-- | Extracts entries from the embedded resource archive that match the prefix
-- directory into the destination directory. The entry path from the archive is
-- preserverd and non-existent intermediate directories are created. Existing
-- files are overwritten.
extractResourceEntries :: FilePath -> FilePath -> IO ()
extractResourceEntries prefix destinationDirectory = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ do
    subEntries <- filterWithKey (subEntry prefix) <$> getEntries
    forM_ (keys subEntries) saveSubEntry
  where
    subEntry dir sel _ = dir `isPrefixOf` unEntrySelector sel
    saveSubEntry sel = do
      let path = destinationDirectory </> unEntrySelector sel
      let dir = FP.takeDirectory path
      liftIO $ Dir.createDirectoryIfMissing True dir
      saveEntry sel path

extractResourceEntry :: FilePath -> IO BS.ByteString
extractResourceEntry entryName = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ mkEntrySelector entryName >>= getEntry

extractResourceEntryList :: [FilePath] -> IO [(FilePath, BS.ByteString)]
extractResourceEntryList entryNames = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ foldM extractEntry [] entryNames
  where
    extractEntry entryList entryName = do
      bs <- mkEntrySelector entryName >>= getEntry
      return $ (entryName, bs) : entryList

extractSubEntries :: FilePath -> FilePath -> FilePath -> IO ()
extractSubEntries prefix archivePath destinationDirectory =
  withArchive archivePath $ do
    subEntries <- filterWithKey (subEntry prefix) <$> getEntries
    forM_ (keys subEntries) saveSubEntry
  where
    subEntry dir sel _ = dir `isPrefixOf` unEntrySelector sel
    saveSubEntry sel = do
      let path = destinationDirectory </> unEntrySelector sel
      let dir = FP.takeDirectory path
      liftIO $ Dir.createDirectoryIfMissing True dir
      saveEntry sel path

extractEntry :: FilePath -> FilePath -> IO BS.ByteString
extractEntry entryName archivePath = do
  withArchive archivePath $ mkEntrySelector entryName >>= getEntry

extractEntryList :: [FilePath] -> FilePath -> IO [(FilePath, BS.ByteString)]
extractEntryList entryNames archivePath = do
  withArchive archivePath $ foldM extractEntry [] entryNames
  where
    extractEntry entryList entryName = do
      bs <- mkEntrySelector entryName >>= getEntry
      return $ (entryName, bs) : entryList
