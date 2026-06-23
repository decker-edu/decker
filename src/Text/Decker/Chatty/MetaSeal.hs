{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Build-time redaction seam for chatty sealed config.
--
-- 'sealChattyMeta' is applied to the deck meta at the two choke points where
-- meta is serialized for the client (the @<hash>.json@ file and the inlined
-- HTML template): the top of @writePandocFile@ and the top of @renderIndex@.
-- It seals the author-controlled chatty fields (@instructions@, @model@,
-- @params@, @vector-store-id@) into @chatty.sealed-config@ and **deletes the
-- plaintext** so it never reaches @public/@.
--
-- The deck-config key is read from a git-controlled @chatty-key.json@ at the
-- project root (never via deck meta, never copied to @public/@). If the key
-- file is absent, or no @chatty.prompt@ is set, or there are no @instructions@
-- to protect, the meta passes through unchanged so non-chatty builds are not
-- disturbed.
module Text.Decker.Chatty.MetaSeal
  ( sealChattyMeta,
    sealChattyMetaIO,
    chattyKeyFile,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Development.Shake (Action, putInfo)
import System.Directory (doesFileExist)
import Text.Decker.Chatty.Seal
  ( KeyFile,
    SealInput (..),
    lookupKey,
    readKeyFile,
    sealConfig,
  )
import Text.Decker.Internal.Meta
  ( deleteMetaValue,
    fromPandocMeta',
    getMetaValue,
    lookupMeta,
    lookupMetaOrElse,
    setMetaValue,
  )
import Text.Pandoc (Meta)

-- | The git-controlled deck-config key file, read directly (not via meta) and
-- never copied to @public/@.
chattyKeyFile :: FilePath
chattyKeyFile = "chatty-key.json"

-- | Seal the chatty config into the meta and strip the plaintext, reading the
-- key file from the project root. Emits its notices through Shake's log.
sealChattyMeta :: Meta -> Action Meta
sealChattyMeta meta = do
  mKeyFile <- liftIO (readKeyFile chattyKeyFile)
  (meta', notices) <- liftIO (sealChattyMetaIO mKeyFile meta)
  mapM_ putInfo notices
  pure meta'

-- | The pure-ish core, testable without Shake. Given the parsed key file (if
-- any) and the meta, returns the redacted meta plus human-readable notices.
-- Guarantees: when sealing applies, the plaintext chatty fields are removed;
-- when anything is missing or fails, the plaintext is *still* stripped if a
-- chatty prompt is present, so the system prompt can never leak to @public/@.
sealChattyMetaIO :: Maybe KeyFile -> Meta -> IO (Meta, [String])
sealChattyMetaIO mKeyFile meta =
  case lookupMeta "chatty.prompt" meta :: Maybe Text of
    Nothing -> pure (meta, []) -- chatty not enabled in this build, stay silent
    Just promptId ->
      case mKeyFile of
        Nothing -> pure (stripPlaintext meta, legacyNotice promptId ("no " <> chattyKeyFile <> " at the project root"))
        Just kf ->
          case lookupKey promptId kf of
            Nothing -> pure (stripPlaintext meta, legacyNotice promptId ("no entry for it in " <> chattyKeyFile))
            Just key -> sealWith promptId key meta

-- | Explain that a chatty deck is being published *without* a sealed config and
-- will therefore use the legacy stored-prompt path at runtime.
legacyNotice :: Text -> String -> [String]
legacyNotice promptId reason =
  [ "# chatty: prompt '" <> p <> "' enabled in LEGACY mode (" <> reason <> ").",
    "#   No sealed config is published; the deck sends '" <> p <> "' to the proxy unchanged.",
    "#   This only works if '" <> p <> "' is a real OpenAI stored-prompt id (pmpt_...)."
  ]
  where
    p = T.unpack promptId

sealWith :: Text -> BS.ByteString -> Meta -> IO (Meta, [String])
sealWith promptId key meta = do
  mInstructions <- resolveInstructions meta
  case mInstructions of
    Nothing ->
      pure
        ( stripPlaintext meta,
          [ "# chatty: prompt '" <> T.unpack promptId <> "' has a key but no chatty.instructions —",
            "#   nothing to seal; the deck falls back to the LEGACY stored-prompt path."
          ]
        )
    Just instructions -> do
      let model = lookupMetaOrElse "gpt-4.1" "chatty.model" meta :: Text
          vectorStore = lookupMetaOrElse "" "chatty.vector-store-id" meta :: Text
          si =
            SealInput
              { siInstructions = instructions,
                siModel = model,
                siParams = paramsValue meta,
                siVectorStoreId = vectorStore
              }
      result <- sealConfig key promptId si
      case result of
        Left err ->
          -- Fail safe: strip plaintext even though no sealed blob was produced.
          pure
            ( stripPlaintext meta,
              [ "# chatty: SEALING FAILED for prompt '" <> T.unpack promptId <> "': " <> T.unpack err <> ".",
                "#   Plaintext config stripped from the published meta; chat will not work until fixed."
              ]
            )
        Right blob ->
          pure
            ( stripPlaintext (setMetaValue "chatty.sealed-config" blob meta),
              [ "# chatty: SEALED config for prompt '"
                  <> T.unpack promptId
                  <> "' (model="
                  <> T.unpack model
                  <> ", instructions="
                  <> show (T.length instructions)
                  <> " chars"
                  <> (if T.null vectorStore then ", no vector store" else ", vector-store=" <> T.unpack vectorStore)
                  <> ")."
              ]
            )

-- | Remove every author-controlled plaintext chatty field from the meta.
stripPlaintext :: Meta -> Meta
stripPlaintext =
  deleteMetaValue "chatty.instructions"
    . deleteMetaValue "chatty.model"
    . deleteMetaValue "chatty.params"

-- | @chatty.params@ as a JSON value (defaults to @{}@).
paramsValue :: Meta -> Value
paramsValue meta = maybe (object []) fromPandocMeta' (getMetaValue "chatty.params" meta)

-- | Resolve @chatty.instructions@ as either a path to a file (read its
-- contents) or inline text. A value that names an existing file is read; any
-- other string is taken verbatim.
resolveInstructions :: Meta -> IO (Maybe Text)
resolveInstructions meta =
  case lookupMeta "chatty.instructions" meta :: Maybe Text of
    Nothing -> pure Nothing
    Just value -> do
      let path = T.unpack value
      isFile <- doesFileExist path
      if isFile
        then Just . decodeUtf8 <$> BS.readFile path
        else pure (Just value)
