{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Sync chatty/ markdown files to an OpenAI vector store.
--
-- All OpenAI calls go through the decker-chatty proxy's signed @/sync@ relay
-- (see "Text.Decker.Chatty.Sync"), so decker never needs the OpenAI API key.
-- The relay is found via chatty.server and chatty.prompt in decker.yaml; the
-- signing key is that prompt's deck-config key from chatty-key.json. On first
-- run (no chatty.vector-store-id), creates a store and prints the id for the
-- user to paste back into decker.yaml.
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
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Digest.Pure.MD5 (md5)
import Data.List (partition, sort, (\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types.URI (urlEncode)
import Network.Wreq
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Text.Decker.Chatty.MetaSeal (chattyKeyFile)
import Text.Decker.Chatty.Seal (KeyFileResult (..), lookupKey, readKeyFile)
import Text.Decker.Chatty.Sync (SyncClient (..), signedHeaders)
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
-- OpenAI API, via the proxy's signed sync relay

-- | Resolve the sync relay from @chatty.server@ and @chatty.prompt@, and the
-- signing key from 'chattyKeyFile'.
syncClient :: Meta -> IO (Either String SyncClient)
syncClient meta = do
  let server = T.strip (lookupMetaOrElse "" "chatty.server" meta)
      promptId = T.strip (lookupMetaOrElse "" "chatty.prompt" meta)
  if
    | T.null server -> return (Left "chatty.server is not set in decker.yaml")
    | T.null promptId -> return (Left "chatty.prompt is not set in decker.yaml")
    | otherwise ->
        readKeyFile chattyKeyFile >>= \case
          KeyFileAbsent -> return (Left ("no " <> chattyKeyFile <> " at the project root"))
          KeyFileError err -> return (Left err)
          KeyFileOk kf -> case lookupKey promptId kf of
            Nothing ->
              return (Left ("no key for prompt '" <> T.unpack promptId <> "' in " <> chattyKeyFile))
            Just k ->
              let base = T.unpack (T.dropWhileEnd (== '/') server) <> "/sync"
               in return (Right (SyncClient base promptId k))

-- | Send one signed request to the relay. @path@ is relative to the relay root
-- and includes the query string; @payload@ is an optional content type and
-- body. A 401 always fails with a hint at the likely cause; any other error
-- status fails only when @strict@ is set, so callers can inspect e.g. a 404.
relay ::
  SyncClient ->
  Bool ->
  BS.ByteString ->
  String ->
  Maybe (BS.ByteString, BS.ByteString) ->
  IO (Response BSL.ByteString)
relay client strict method path payload = do
  now <- round <$> getPOSIXTime
  let body = maybe "" snd payload
      auth = signedHeaders client now method path body
      opts =
        defaults
          & checkResponse ?~ (\_ _ -> return ())
          & foldr (\(n, v) o -> o . (header n .~ [v])) id auth
          & maybe id (\(ct, _) -> header "Content-Type" .~ [ct]) payload
      url = scBase client <> path
  r <- case payload of
    Nothing -> customMethodWith (BS.unpack method) opts url
    Just (_, b) -> customPayloadMethodWith (BS.unpack method) opts url b
  let code = r ^. responseStatus . statusCode
      detail = BS.unpack method <> " " <> path <> ": " <> BSL8.unpack (r ^. responseBody)
  when (code == 401) $
    fail $
      "chatty proxy rejected the request for prompt '"
        <> T.unpack (scPromptId client)
        <> "' (does "
        <> chattyKeyFile
        <> " match the proxy's deckConfigKey? are both clocks correct?) "
        <> detail
  when (strict && code >= 400) $
    fail $
      "OpenAI via chatty proxy: status " <> show code <> " for " <> detail
  return r

relayGet :: SyncClient -> String -> IO (Response BSL.ByteString)
relayGet client path = relay client True "GET" path Nothing

relayJson :: SyncClient -> BS.ByteString -> String -> Value -> IO (Response BSL.ByteString)
relayJson client method path v =
  relay client True method path (Just ("application/json", BSL.toStrict (encode v)))

-- | Whether the vector store with the given id still exists. A store deleted
-- via the OpenAI GUI leaves a dangling id in decker.yaml; without this check
-- every subsequent call would crash with a 404.
vectorStoreExists :: SyncClient -> Text -> IO Bool
vectorStoreExists client storeId = do
  r <- relay client False "GET" ("/vector_stores/" <> T.unpack storeId) Nothing
  let code = r ^. responseStatus . statusCode
  when (code >= 400 && code /= 404) $
    fail $
      "OpenAI via chatty proxy: status " <> show code <> " checking vector store " <> T.unpack storeId
  return (code /= 404)

createVectorStore :: SyncClient -> Text -> IO Text
createVectorStore client name = do
  r <- relayJson client "POST" "/vector_stores" (object ["name" .= name])
  case r ^? responseBody . key "id" . _String of
    Just sid -> return sid
    Nothing -> fail "OpenAI: no id in vector_stores response"

-- | Upload a file as raw bytes; the proxy wraps it into OpenAI's multipart
-- @purpose=assistants@ upload under the given file name.
uploadFile :: SyncClient -> FilePath -> IO Text
uploadFile client path = do
  bytes <- BS.readFile path
  let name = urlEncode True (TE.encodeUtf8 (T.pack (takeFileName path)))
  r <-
    relay
      client
      True
      "POST"
      ("/files?filename=" <> BS.unpack name)
      (Just ("application/octet-stream", bytes))
  case r ^? responseBody . key "id" . _String of
    Just fid -> return fid
    Nothing -> fail $ "OpenAI: no id in files response for " <> path

-- | Issue a DELETE that treats a 404 as success. Deletes are idempotent: a file
-- or attachment that is already gone is the desired end state, so a stale id
-- (e.g. a duplicate purged in an earlier interrupted run) must not abort the
-- sync. Other 4xx/5xx responses still raise, with the body for context.
idempotentDelete :: SyncClient -> String -> IO ()
idempotentDelete client path = do
  -- Non-strict relay so a 404 does not throw.
  r <- relay client False "DELETE" path Nothing
  let code = r ^. responseStatus . statusCode
  when (code >= 400 && code /= 404) $
    fail $
      "OpenAI: DELETE " <> path <> " failed with status " <> show code

deleteFile :: SyncClient -> Text -> IO ()
deleteFile client fid =
  idempotentDelete client ("/files/" <> T.unpack fid)

detachFromStore :: SyncClient -> Text -> Text -> IO ()
detachFromStore client storeId fid =
  idempotentDelete
    client
    ("/vector_stores/" <> T.unpack storeId <> "/files/" <> T.unpack fid)

-- | Attach a file to the store with attributes capturing its source path and md5.
attachToStore :: SyncClient -> Text -> Text -> FilePath -> Text -> IO ()
attachToStore client storeId fid path hash = do
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
    relayJson
      client
      "POST"
      ("/vector_stores/" <> T.unpack storeId <> "/files")
      body
  return ()

-- | Poll the store until no files are still being indexed.
waitForIndexing :: SyncClient -> Text -> IO ()
waitForIndexing client storeId = loop (0 :: Int)
  where
    loop n
      | n > 120 = fail "OpenAI: vector store indexing did not complete within timeout"
      | otherwise = do
          r <- relayGet client ("/vector_stores/" <> T.unpack storeId)
          let inProgress = r ^? responseBody . key "file_counts" . key "in_progress" . _Integer
          let failed = r ^? responseBody . key "file_counts" . key "failed" . _Integer
          case (inProgress, failed) of
            (Just 0, Just 0) -> return ()
            (Just 0, Just f) -> fail $ "OpenAI: " <> show f <> " file(s) failed to index"
            _ -> threadDelay 1000000 >> loop (n + 1)

-- | All files attached to the store, grouped by their @path@ attribute.
-- Returns @(path → [(fileId, md5)], orphanFileIds)@. A path may map to more
-- than one file when earlier runs left duplicates behind; 'reconcileStore'
-- prunes those. @orphanFileIds@ are files carrying no @path@ attribute (e.g.
-- uploaded by an older decker or by hand) — these are removed too.
listStoreFiles :: SyncClient -> Text -> IO (Map FilePath [(Text, Text)], [Text])
listStoreFiles client storeId = page Nothing Map.empty []
  where
    page cursor acc orphans = do
      let url =
            "/vector_stores/"
              <> T.unpack storeId
              <> "/files?limit=100"
              <> maybe "" (\c -> "&after=" <> T.unpack c) cursor
      r <- relayGet client url
      let entries = r ^.. responseBody . key "data" . _Array . traverse
      let (acc', orphans') = foldr insertEntry (acc, orphans) entries
      let hasMore = r ^? responseBody . key "has_more" . _Bool
      let lastId = r ^? responseBody . key "last_id" . _String
      case (hasMore, lastId) of
        (Just True, Just lid) -> page (Just lid) acc' orphans'
        _ -> return (acc', orphans')

    insertEntry v (m, orphans) =
      let fid = v ^? key "id" . _String
          p = v ^? key "attributes" . key "path" . _String
          h = v ^? key "attributes" . key "md5" . _String
       in case (fid, p, h) of
            (Just f, Just pp, Just hh) ->
              (Map.insertWith (++) (T.unpack pp) [(f, hh)] m, orphans)
            (Just f, _, _) -> (m, f : orphans)
            _ -> (m, orphans)

-- | List every @purpose=assistants@ file object in the project, as @(id, name)@.
-- These are the files decker uploads; other purposes (fine-tune, batch, …) are
-- left untouched.
listAssistantFiles :: SyncClient -> IO [(Text, Text)]
listAssistantFiles client = page Nothing []
  where
    page cursor acc = do
      let url =
            "/files?purpose=assistants&limit=10000"
              <> maybe "" (\c -> "&after=" <> T.unpack c) cursor
      r <- relayGet client url
      let entries = r ^.. responseBody . key "data" . _Array . traverse
      let acc' = acc ++ mapMaybe entry entries
      let hasMore = r ^? responseBody . key "has_more" . _Bool
      let lastId = r ^? responseBody . key "last_id" . _String
      case (hasMore, lastId) of
        (Just True, Just lid) -> page (Just lid) acc'
        _ -> return acc'

    entry v = do
      fid <- v ^? key "id" . _String
      let name = v ^? key "filename" . _String
      return (fid, fromMaybe fid name)

-- | Delete every @assistants@ file object that is not currently attached to the
-- given vector store. Run after 'reconcileStore' so the store holds exactly the
-- files that should survive; everything else (e.g. files orphaned when a store
-- was deleted in the GUI) is removed. Destructive: this touches all
-- assistant-purpose files in the OpenAI project, not just decker's.
pruneOrphanFiles :: SyncClient -> Text -> IO ()
pruneOrphanFiles client storeId = do
  (remote, orphans) <- listStoreFiles client storeId
  let attached = orphans ++ concatMap (map fst) (Map.elems remote)
  all' <- listAssistantFiles client
  let danglers = [(fid, name) | (fid, name) <- all', fid `notElem` attached]
  if null danglers
    then putStrLn "# prune: no dangling assistant files."
    else do
      forM_ danglers $ \(fid, name) -> do
        putStrLn $ "# prune file: " <> T.unpack name <> " (" <> T.unpack fid <> ")"
        deleteFile client fid
      putStrLn $ "# pruned " <> show (length danglers) <> " file(s)."

-- ---------------------------------------------------------------------------
-- Entry point

-- | Detach a file from the store and delete the underlying file object.
purgeFile :: SyncClient -> Text -> Text -> IO ()
purgeFile client storeId fid = do
  detachFromStore client storeId fid
  deleteFile client fid

-- | Reconcile the given local files (a map from store path to local file path)
-- against the store so that, when finished, every local path is represented by
-- exactly one current file:
--   - upload new or changed files (deleting any old remote files for that path),
--   - delete files whose path vanished locally,
--   - collapse accidental duplicates down to a single current copy,
--   - delete orphan files that carry no path attribute,
--   - leave unchanged files alone,
-- then wait for indexing.
reconcileStore :: SyncClient -> Text -> Map FilePath FilePath -> IO ()
reconcileStore client storeId files = do
  newHashes <- traverse hashLocal files
  (remote, orphans) <- listStoreFiles client storeId

  -- Files with no path attribute can never be matched against a local file;
  -- drop them so the store only holds files this tool manages.
  forM_ orphans $ \fid -> do
    putStrLn "# delete orphan (no path attribute)"
    purgeFile client storeId fid

  -- Paths present remotely but no longer local: remove every copy.
  let removals = Map.keys remote \\ Map.keys newHashes
  forM_ removals $ \p -> do
    putStrLn $ "# delete: " <> p
    forM_ (remote Map.! p) $ \(fid, _) -> purgeFile client storeId fid

  -- Each local path: keep a single matching copy if one exists, otherwise
  -- (re)upload; in both cases purge any other remote copies for that path.
  results <- forM (Map.toList files) $ \(p, localPath) -> do
    let h = newHashes Map.! p
    let copies = Map.findWithDefault [] p remote
    let (matching, stale) = partition ((== h) . snd) copies
    case matching of
      ((_, _) : extras) -> do
        -- Already present and current; drop any duplicate/stale copies.
        let dups = map fst (extras ++ stale)
        unless (null dups) $ putStrLn $ "# dedup: " <> p
        forM_ dups $ purgeFile client storeId
        return Unchanged
      [] -> do
        if null stale
          then putStrLn $ "# upload: " <> p
          else putStrLn $ "# replace: " <> p
        forM_ (map fst stale) $ purgeFile client storeId
        fid <- uploadFile client localPath
        attachToStore client storeId fid p h
        return Uploaded

  let uploaded = length (filter (== Uploaded) results)
  let unchanged = length (filter (== Unchanged) results)

  when (uploaded > 0) $ do
    putStrLn $ "# waiting for indexing of " <> show uploaded <> " file(s)..."
    waitForIndexing client storeId

  putStrLn $
    "# done. "
      <> show unchanged
      <> " unchanged, "
      <> show uploaded
      <> " uploaded, "
      <> show (length removals)
      <> " removed."

-- | Outcome of reconciling a single local file.
data Outcome = Unchanged | Uploaded deriving (Eq)

-- | Sync the local chatty files to the configured vector store. When @prune@ is
-- set (the @--prune-files@ flag), also delete any assistant-purpose OpenAI file
-- objects left dangling, i.e. not attached to the store.
runChatty :: Bool -> IO ()
runChatty prune = do
  meta <- readDeckerMetaIO deckerMetaFile
  client <-
    syncClient meta >>= \case
      Right c -> return c
      Left err -> do
        putStrLn $ "# " <> err <> " — cannot reach the chatty proxy for vector store sync."
        exitFailure
  let storeId = lookupMetaOrElse ("" :: Text) "chatty.vector-store-id" meta
  let storeName = lookupMetaOrElse ("decker" :: Text) "chatty.vector-store-name" meta

  missing <-
    if T.null storeId
      then return True
      else do
        ok <- vectorStoreExists client storeId
        unless ok $
          putStrLn $
            "# chatty.vector-store-id "
              <> T.unpack storeId
              <> " no longer exists (deleted?) — creating a new vector store..."
        return (not ok)

  when missing $ do
    when (T.null storeId) $
      putStrLn "# No chatty.vector-store-id in decker.yaml — creating a new vector store..."
    sid <- createVectorStore client storeName
    putStrLn ""
    putStrLn $ "# Created vector store: " <> T.unpack sid
    putStrLn "# Add the following to decker.yaml and run `decker chatty` again:"
    putStrLn ""
    putStrLn "chatty:"
    putStrLn $ "  vector-store-id: " <> T.unpack sid
    putStrLn ""
    exitSuccess

  files <- collectLocalFiles meta
  when (Map.null files) $ do
    putStrLn $
      "# Nothing to sync: no markdown under "
        <> chattyDir
        <> "/ and no files in `chatty.extra`. Build first."
    exitFailure

  reconcileStore client storeId files
  when prune $ pruneOrphanFiles client storeId

-- | Non-exiting sync for `decker publish`. Reconciles the generated markdown
-- under 'chattyDir' plus the files in the configured `chatty.extra` directories
-- against the vector store. Does nothing (apart from a note) when no store id or
-- no sync relay is configured, so it can never abort a publish run. The caller is
-- expected to have (re)populated 'chattyDir' with exactly the set of generated
-- files that should be present in the store.
syncChattyToStore :: IO ()
syncChattyToStore = do
  meta <- readDeckerMetaIO deckerMetaFile
  let storeId = lookupMetaOrElse ("" :: Text) "chatty.vector-store-id" meta
  if T.null storeId
    then putStrLn "# chatty.vector-store-id not set — skipping vector store sync."
    else
      syncClient meta >>= \case
        Left err -> putStrLn $ "# " <> err <> " — skipping vector store sync."
        Right client -> do
          exists <- vectorStoreExists client storeId
          if not exists
            then
              putStrLn $
                "# chatty.vector-store-id "
                  <> T.unpack storeId
                  <> " no longer exists — skipping vector store sync. "
                  <> "Run `decker chatty` to create a new store and update decker.yaml."
            else do
              files <- collectLocalFiles meta
              reconcileStore client storeId files
