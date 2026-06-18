{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Sync chatty/ markdown files to an OpenAI vector store.
--
-- Reads OPENAI_API_KEY from the environment and chatty.vector-store-id
-- from decker.yaml. On first run (no store id), creates a store and prints
-- the id for the user to paste back into decker.yaml.
--
-- In addition to the generated markdown under chatty/, every file in the
-- directories listed in the chatty.extra meta variable is uploaded as is.
--
-- Incremental: each attached vector-store file carries the source's relative
-- path and md5 as attributes, so the remote store *is* the manifest. Each run:
--   - uploads new or changed files (deleting the old file first),
--   - detaches and deletes files that vanished locally,
--   - leaves unchanged files alone,
--   - waits for indexing to complete.
module Text.Decker.Chatty.Upload (runChatty, syncChattyToStore) where

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
import Text.Pandoc (Meta)

chattyDir :: FilePath
chattyDir = "chatty"

-- ---------------------------------------------------------------------------
-- File walking and hashing

-- | All markdown files generated under 'chattyDir', mapping the store path
-- (the path relative to 'chattyDir', e.g. @lectures\/12-deck.md@) to the local
-- file to upload.
findMarkdownFiles :: IO (Map FilePath FilePath)
findMarkdownFiles = do
  exists <- doesDirectoryExist chattyDir
  if exists
    then Map.fromList . map (\p -> (makeRelative chattyDir p, p)) <$> walk (== ".md") chattyDir
    else return Map.empty

-- | All files under the given `chatty.extra` directory, to be uploaded as is.
-- The store path is the file's project-relative path (which equals the local
-- path), so files keep their identity across runs.
findExtraFiles :: FilePath -> IO (Map FilePath FilePath)
findExtraFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then Map.fromList . map (\p -> (p, p)) <$> walk (const True) dir
    else do
      putStrLn $ "# chatty.extra: directory does not exist, skipping: " <> dir
      return Map.empty

-- | Recursively collect files below `dir` whose extension passes `keep`.
walk :: (String -> Bool) -> FilePath -> IO [FilePath]
walk keep dir = do
  entries <- listDirectory dir
  fmap (sort . concat) . forM entries $ \e -> do
    let p = dir </> e
    isDir <- doesDirectoryExist p
    if isDir
      then walk keep p
      else return [p | keep (takeExtension p)]

-- | The complete set of files to mirror into the vector store: the generated
-- chatty markdown plus everything in the configured `chatty.extra` directories.
-- Generated markdown wins on a path collision.
collectLocalFiles :: Meta -> IO (Map FilePath FilePath)
collectLocalFiles meta = do
  markdown <- findMarkdownFiles
  let extraDirs = map T.unpack (lookupMetaOrElse [] "chatty.extra" meta :: [Text])
  extra <- Map.unions <$> mapM findExtraFiles extraDirs
  return (Map.union markdown extra)

hashLocal :: FilePath -> IO Text
hashLocal path = do
  bs <- BSL.readFile path
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

-- | Look up the OpenAI API key from the environment.
lookupApiKey :: IO (Maybe BS.ByteString)
lookupApiKey =
  lookupEnv "OPENAI_API_KEY" >>= \case
    Just k | not (null k) -> return (Just (BS.pack k))
    _ -> return Nothing

-- | Reconcile the given local files (a map from store path to local file path)
-- against the store: upload new or changed files (deleting the old remote file
-- first), delete files that vanished locally, leave unchanged files alone, then
-- wait for indexing.
reconcileStore :: BS.ByteString -> Text -> Map FilePath FilePath -> IO ()
reconcileStore apiKey storeId files = do
  newHashes <- traverse hashLocal files
  remote <- listStoreFiles apiKey storeId

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
    detachFromStore apiKey storeId fid
    deleteFile apiKey fid

  attached <- forM (Map.toList upserts) $ \(p, h) -> do
    case Map.lookup p remote of
      Just (oldFid, _) -> do
        putStrLn $ "# replace: " <> p
        detachFromStore apiKey storeId oldFid
        deleteFile apiKey oldFid
      Nothing -> putStrLn $ "# upload: " <> p
    fid <- uploadFile apiKey (files Map.! p)
    attachToStore apiKey storeId fid p h
    return p

  unless (null attached) $ do
    putStrLn $ "# waiting for indexing of " <> show (length attached) <> " file(s)..."
    waitForIndexing apiKey storeId

  putStrLn $
    "# done. "
      <> show (Map.size unchanged)
      <> " unchanged, "
      <> show (length attached)
      <> " uploaded, "
      <> show (length removals)
      <> " removed."

runChatty :: IO ()
runChatty = do
  apiKey <-
    lookupApiKey >>= \case
      Just k -> return k
      Nothing -> do
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

  files <- collectLocalFiles meta
  when (Map.null files) $ do
    putStrLn $
      "# Nothing to sync: no markdown under "
        <> chattyDir
        <> "/ and no files in `chatty.extra`. Build first."
    exitFailure

  reconcileStore apiKey storeId' files

-- | Non-exiting sync for `decker publish`. Reconciles the generated markdown
-- under 'chattyDir' plus the files in the configured `chatty.extra` directories
-- against the vector store. Does nothing (apart from a note) when no store id or
-- API key is available, so it can never abort a publish run. The caller is
-- expected to have (re)populated 'chattyDir' with exactly the set of generated
-- files that should be present in the store.
syncChattyToStore :: IO ()
syncChattyToStore = do
  meta <- readDeckerMetaIO deckerMetaFile
  let storeId = lookupMetaOrElse ("" :: Text) "chatty.vector-store-id" meta
  if T.null storeId
    then putStrLn "# chatty.vector-store-id not set — skipping vector store sync."
    else
      lookupApiKey >>= \case
        Nothing -> putStrLn "# OPENAI_API_KEY is not set — skipping vector store sync."
        Just apiKey -> do
          files <- collectLocalFiles meta
          reconcileStore apiKey storeId files
