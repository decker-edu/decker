{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Sync chatty/ markdown files to an OpenAI vector store.
--
-- Reads OPENAI_API_KEY from the environment and chatty.vector-store-id
-- from decker.yaml. On first run (no store id), creates a store and prints
-- the id for the user to paste back into decker.yaml.
--
-- Incremental: each attached vector-store file carries the source's relative
-- path and md5 as attributes, so the remote store *is* the manifest. Each run:
--   - uploads new or changed files (deleting the old file first),
--   - detaches and deletes files that vanished locally,
--   - leaves unchanged files alone,
--   - waits for indexing to complete.
module Text.Decker.Chatty.Upload (runChatty) where

import Control.Concurrent (threadDelay)
import Control.Lens hiding ((.=))
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson ((.=))
import Data.Aeson hiding ((.=), Options)
import Data.Aeson.Lens (key, _Array, _Bool, _Integer, _String)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.MD5 (md5)
import Data.List (sort, (\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wreq
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Text.Decker.Internal.Common (deckerMetaFile)
import Text.Decker.Internal.Meta (lookupMetaOrElse)
import Text.Decker.Internal.MetaExtra (readDeckerMetaIO)

chattyDir :: FilePath
chattyDir = "chatty"

-- ---------------------------------------------------------------------------
-- File walking and hashing

findMarkdownFiles :: IO [FilePath]
findMarkdownFiles = do
  exists <- doesDirectoryExist chattyDir
  if exists then sort <$> walk chattyDir else return []
  where
    walk dir = do
      entries <- listDirectory dir
      fmap concat . forM entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        if isDir
          then walk p
          else
            if takeExtension p == ".md"
              then return [makeRelative chattyDir p]
              else return []

hashFile :: FilePath -> IO Text
hashFile rel = do
  bs <- BSL.readFile (chattyDir </> rel)
  return (T.pack (show (md5 bs)))

-- ---------------------------------------------------------------------------
-- OpenAI API

openaiBase :: String
openaiBase = "https://api.openai.com/v1"

authOpts :: BS.ByteString -> Options
authOpts apiKey =
  defaults
    & header "Authorization" .~ ["Bearer " <> apiKey]

jsonOpts :: BS.ByteString -> Options
jsonOpts apiKey =
  authOpts apiKey & header "Content-Type" .~ ["application/json"]

createVectorStore :: BS.ByteString -> Text -> IO Text
createVectorStore apiKey name = do
  r <- postWith (jsonOpts apiKey) (openaiBase <> "/vector_stores") (object ["name" .= name])
  case r ^? responseBody . key "id" . _String of
    Just sid -> return sid
    Nothing -> fail "OpenAI: no id in vector_stores response"

uploadFile :: BS.ByteString -> FilePath -> IO Text
uploadFile apiKey path = do
  r <-
    postWith
      (authOpts apiKey)
      (openaiBase <> "/files")
      [ partString "purpose" "assistants",
        partFileSource "file" path
      ]
  case r ^? responseBody . key "id" . _String of
    Just fid -> return fid
    Nothing -> fail $ "OpenAI: no id in files response for " <> path

deleteFile :: BS.ByteString -> Text -> IO ()
deleteFile apiKey fid = do
  _ <- deleteWith (authOpts apiKey) (openaiBase <> "/files/" <> T.unpack fid)
  return ()

detachFromStore :: BS.ByteString -> Text -> Text -> IO ()
detachFromStore apiKey storeId fid = do
  _ <-
    deleteWith
      (authOpts apiKey)
      (openaiBase <> "/vector_stores/" <> T.unpack storeId <> "/files/" <> T.unpack fid)
  return ()

-- | Attach a file to the store with attributes capturing its source path and md5.
attachToStore :: BS.ByteString -> Text -> Text -> FilePath -> Text -> IO ()
attachToStore apiKey storeId fid path hash = do
  let body =
        object
          [ "file_id" .= fid,
            "attributes"
              .= object
                [ "path" .= T.pack path,
                  "md5" .= hash
                ]
          ]
  _ <-
    postWith
      (jsonOpts apiKey)
      (openaiBase <> "/vector_stores/" <> T.unpack storeId <> "/files")
      body
  return ()

-- | Poll the store until no files are still being indexed.
waitForIndexing :: BS.ByteString -> Text -> IO ()
waitForIndexing apiKey storeId = loop (0 :: Int)
  where
    loop n
      | n > 120 = fail "OpenAI: vector store indexing did not complete within timeout"
      | otherwise = do
          r <- getWith (authOpts apiKey) (openaiBase <> "/vector_stores/" <> T.unpack storeId)
          let inProgress = r ^? responseBody . key "file_counts" . key "in_progress" . _Integer
          let failed = r ^? responseBody . key "file_counts" . key "failed" . _Integer
          case (inProgress, failed) of
            (Just 0, Just 0) -> return ()
            (Just 0, Just f) -> fail $ "OpenAI: " <> show f <> " file(s) failed to index"
            _ -> threadDelay 1000000 >> loop (n + 1)

-- | List all files attached to the store with their attributes.
-- Returns: path → (fileId, md5). Files without a path attribute are skipped.
listStoreFiles :: BS.ByteString -> Text -> IO (Map FilePath (Text, Text))
listStoreFiles apiKey storeId = page Nothing Map.empty
  where
    page cursor acc = do
      let url =
            openaiBase
              <> "/vector_stores/"
              <> T.unpack storeId
              <> "/files?limit=100"
              <> maybe "" (\c -> "&after=" <> T.unpack c) cursor
      r <- getWith (authOpts apiKey) url
      let entries = r ^.. responseBody . key "data" . _Array . traverse
      let acc' = foldr insertEntry acc entries
      let hasMore = r ^? responseBody . key "has_more" . _Bool
      let lastId = r ^? responseBody . key "last_id" . _String
      case (hasMore, lastId) of
        (Just True, Just lid) -> page (Just lid) acc'
        _ -> return acc'

    insertEntry v m =
      let fid = v ^? key "id" . _String
          p = v ^? key "attributes" . key "path" . _String
          h = v ^? key "attributes" . key "md5" . _String
       in case (fid, p, h) of
            (Just f, Just pp, Just hh) -> Map.insert (T.unpack pp) (f, hh) m
            _ -> m

-- ---------------------------------------------------------------------------
-- Entry point

runChatty :: IO ()
runChatty = do
  apiKey <-
    lookupEnv "OPENAI_API_KEY" >>= \case
      Just k | not (null k) -> return (BS.pack k)
      _ -> do
        putStrLn "# OPENAI_API_KEY is not set."
        exitFailure
  meta <- readDeckerMetaIO deckerMetaFile
  let storeId = lookupMetaOrElse ("" :: Text) "chatty.vector-store-id" meta
  let storeName = lookupMetaOrElse ("decker" :: Text) "chatty.vector-store-name" meta

  storeId' <-
    if T.null storeId
      then do
        putStrLn "# No chatty.vector-store-id in decker.yaml — creating a new vector store..."
        sid <- createVectorStore apiKey storeName
        putStrLn ""
        putStrLn $ "# Created vector store: " <> T.unpack sid
        putStrLn "# Add the following to decker.yaml and run `decker chatty` again:"
        putStrLn ""
        putStrLn "chatty:"
        putStrLn $ "  vector-store-id: " <> T.unpack sid
        putStrLn ""
        exitSuccess
      else return storeId

  files <- findMarkdownFiles
  when (null files) $ do
    putStrLn $
      "# No markdown files under "
        <> chattyDir
        <> "/. Enable `chatty.write-markdown: true` in decker.yaml and build first."
    exitFailure

  newHashes <- Map.fromList <$> mapM (\p -> (,) p <$> hashFile p) files
  remote <- listStoreFiles apiKey storeId'

  let unchanged =
        Map.filterWithKey
          (\p h -> fmap snd (Map.lookup p remote) == Just h)
          newHashes
  let upserts =
        Map.filterWithKey
          (\p h -> fmap snd (Map.lookup p remote) /= Just h)
          newHashes
  let removals = Map.keys remote \\ Map.keys newHashes

  forM_ removals $ \p -> do
    let (fid, _) = remote Map.! p
    putStrLn $ "# delete: " <> p
    detachFromStore apiKey storeId' fid
    deleteFile apiKey fid

  attached <- forM (Map.toList upserts) $ \(p, h) -> do
    case Map.lookup p remote of
      Just (oldFid, _) -> do
        putStrLn $ "# replace: " <> p
        detachFromStore apiKey storeId' oldFid
        deleteFile apiKey oldFid
      Nothing -> putStrLn $ "# upload: " <> p
    fid <- uploadFile apiKey (chattyDir </> p)
    attachToStore apiKey storeId' fid p h
    return p

  unless (null attached) $ do
    putStrLn $ "# waiting for indexing of " <> show (length attached) <> " file(s)..."
    waitForIndexing apiKey storeId'

  putStrLn $
    "# done. "
      <> show (Map.size unchanged)
      <> " unchanged, "
      <> show (length attached)
      <> " uploaded, "
      <> show (length removals)
      <> " removed."
