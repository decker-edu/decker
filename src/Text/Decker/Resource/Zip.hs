{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.Decker.Resource.Zip
  ( extractResourceEntries,
    extractResourceEntry,
    extractResourceEntryList,
    extractEntry,
    extractSubEntries,
    extractEntryList,
    listEntries,
  )
where

import Codec.Archive.Zip
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Relude
import qualified System.Directory as Dir
import System.Environment
import System.FilePath.Posix
import Text.Decker.Internal.Helper

-- | Extracts entries from the embedded resource archive that match the prefix
-- directory into the destination directory. The entry path from the archive is
-- preserverd and non-existent intermediate directories are created. Existing
-- files are overwritten.
extractResourceEntries :: FilePath -> FilePath -> IO ()
extractResourceEntries prefix destinationDirectory = do
  deckerExecutable <- getExecutablePath
  withArchive deckerExecutable $ do
    subEntries <- Map.filterWithKey (subEntry prefix) <$> getEntries
    forM_ (Map.keys subEntries) saveSubEntry
  where
    subEntry dir sel _ = dir `isPrefixOf` unEntrySelector sel
    saveSubEntry sel = do
      let path = destinationDirectory </> unEntrySelector sel
      let dir = takeDirectory path
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

stripPrefix :: FilePath -> FilePath -> FilePath
stripPrefix prefix path =
  let pr = splitPath prefix
      pa = splitPath path
   in joinPath $ drop (length pr) pa

extractSubEntries :: FilePath -> FilePath -> FilePath -> IO ()
extractSubEntries prefix archivePath destinationDirectory =
  withArchive archivePath $ do
    subEntries <- Map.filterWithKey (subEntry prefix) <$> getEntries
    forM_ (Map.keys subEntries) saveSubEntry
  where
    subEntry dir sel _ = dir `isPrefixOf` unEntrySelector sel
    saveSubEntry sel = do
      let path = destinationDirectory </> stripPrefix prefix (unEntrySelector sel)
      -- putStrLn $ "# Zip: extract: " <> unEntrySelector sel <> " to: " <> path
      let dir = takeDirectory path
      liftIO $ do
        Dir.createDirectoryIfMissing True dir
        removeFile_ path
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

listEntries :: FilePath -> IO [FilePath]
listEntries archivePath = do
  withArchive archivePath $ do
    map (toString . getEntryName) . Map.keys <$> getEntries
