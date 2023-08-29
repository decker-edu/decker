{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

import Codec.Archive.Zip
import Conduit
import Control.Monad.Extra
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.FilePath
import System.FilePath.Glob
import System.IO
import System.IO.Extra

-- main = defaultMain
main = defaultMainWithHooks simpleUserHooks {postCopy = appendResourceArchive}

resourceDir = "./resource"

appendResourceArchive ::
  Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
appendResourceArchive args flags descr info = do
  let binDir = fromPathTemplate $ bindir $ installDirTemplates info
  executable <- makeAbsolute $ binDir </> executableName
  withCurrentDirectory resourceDir $ do
    files <-
      map normalise <$> fastGlobFiles [] [] "."
        >>= filterM doesFileExist
        >>= mapM makeRelativeToCurrentDirectory
    withTempFile
      ( \archive -> do
          createArchive archive $ forM_ files addFile
          putStrLn $
            "Appending resource archive ("
              ++ show (length files)
              ++ " files) to "
              ++ executable
          exeSize <- withBinaryFile executable ReadMode $ \h -> hFileSize h
          fixZip archive exeSize
          runConduitRes $
            sourceFileBS archive .| sinkIOHandle (openFile executable AppendMode)
      )
  where
    addFile path = do
      selector <- mkEntrySelector path
      loadEntry Deflate selector path

-- | This is a plain copy from Glob.hs. We need it here because
-- System.FilePath.Glob.glob does not follow symlinks.
fastGlobFiles :: [String] -> [String] -> FilePath -> IO [FilePath]
fastGlobFiles exclude suffixes root = sort <$> glob root
  where
    absExclude = map (root </>) exclude
    absListDirectory dir =
      map (dir </>) . filter (not . isPrefixOf ".") <$> listDirectory dir
    glob :: FilePath -> IO [String]
    glob root = do
      dirExists <- doesDirectoryExist root
      fileExists <- doesFileExist root
      if
          | dirExists -> globDir root
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

fixZip :: FilePath -> Integer -> IO ()
fixZip zipPath adjustmentSize = do
  withBinaryFile zipPath ReadWriteMode $ \h -> do
    fsize <- hFileSize h
    -- TODO: if fsize is too small, don't do anything or report error
    hSeek h SeekFromEnd (-22)
    findEOCD h
    hSeek h RelativeSeek 10
    numberOfFiles <- readLen h
    hSeek h RelativeSeek 4
    currentOffset <- readNum h
    hSeek h RelativeSeek (-4) -- undo change of last read
    let cdPos = currentOffset + fromIntegral adjustmentSize
    -- update central directory offset in eocd
    writeNum h (fromIntegral cdPos)
    -- update central directory file offsets
    hSeek h AbsoluteSeek (fromIntegral currentOffset)
    fixCDEntryRec h (fromIntegral numberOfFiles)
    return ()
  where
    findEOCD h = do
      sig <- readNum h
      hSeek h RelativeSeek (-4) -- undo read increment
      if sig == 0x06054b50
        then do
          return ()
        else do
          hSeek h RelativeSeek (-1) -- search backwards
          findEOCD h
          return ()
    readNum h = runGet getWord32le <$> B.hGet h 4
    readLen h = runGet getWord16le <$> B.hGet h 2
    writeNum h val = B.hPut h (runPut $ putWord32le val)
    fixCDEntry h = do
      hSeek h RelativeSeek 28
      fileLength <- readLen h
      extraLength <- readLen h
      commentLength <- readLen h
      hSeek h RelativeSeek 8
      currentRelativeOffset <- readNum h
      hSeek h RelativeSeek (-4) -- undo change of last read
      writeNum
        h
        (fromIntegral (currentRelativeOffset + fromIntegral adjustmentSize))
      hSeek
        h
        RelativeSeek
        (fromIntegral $ fileLength + extraLength + commentLength)
      return ()
    fixCDEntryRec :: Handle -> Int -> IO ()
    fixCDEntryRec h iterations =
      if iterations > 0
        then do
          fixCDEntry h
          fixCDEntryRec h (iterations - 1)
          return ()
        else do
          return ()

executableName :: String
#ifdef mingw32_HOST_OS
executableName = "decker.exe"
#else
executableName = "decker"
#endif
