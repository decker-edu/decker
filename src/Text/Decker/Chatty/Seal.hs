{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Seal the author-controlled chatty config (system prompt, model, params,
-- vector store id) into an opaque blob that travels with the published deck and
-- is decrypted by the decker-chatty proxy at request time.
--
-- Crypto contract (must stay byte-compatible with the Node proxy — see
-- @test/fixtures/chatty-seal-fixture.json@):
--
--   * AES-256-GCM.
--   * key  = 32 bytes (the @deck-config-key@, base64 in @chatty-key.json@).
--   * nonce = 12 random bytes, prepended to the ciphertext.
--   * AAD  = the prompt-id string (authenticated, not encrypted).
--   * wire = base64( nonce || ciphertext || tag ), tag = 16 bytes.
--
-- The deck-config key is deliberately separate from the OpenAI API key so the
-- API key can rotate without recompiling every deck.
module Text.Decker.Chatty.Seal
  ( SealInput (..),
    sealBytes,
    openBytes,
    sealConfig,
    openConfig,
    payloadJson,
    readKeyFile,
    lookupKey,
    KeyFile (..),
  )
where

import Control.Exception (SomeException, catch)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types
  ( AEADMode (AEAD_GCM),
    AuthTag (..),
    aeadAppendHeader,
    aeadDecrypt,
    aeadEncrypt,
    aeadFinalize,
    aeadInit,
    cipherInit,
  )
import Crypto.Error (CryptoError, eitherCryptoError)
import Crypto.Random (getRandomBytes)
import Data.Aeson (Value (Object, String), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseEither)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | The plaintext author config, before sealing.
data SealInput = SealInput
  { siInstructions :: Text,
    siModel :: Text,
    siParams :: Value,
    siVectorStoreId :: Text
  }
  deriving (Show, Eq)

-- | Canonical JSON plaintext for a 'SealInput'. The proxy only needs to parse
-- this, so cross-language byte-identity is not required here; the frozen
-- fixture pins the bytes that matter for the crypto round-trip.
payloadJson :: SealInput -> ByteString
payloadJson si =
  BSL.toStrict $
    A.encode $
      object
        [ "instructions" .= siInstructions si,
          "model" .= siModel si,
          "params" .= siParams si,
          "vector_store_id" .= siVectorStoreId si
        ]

-- | Seal raw plaintext bytes. @key@ must be 32 bytes, @nonce@ 12 bytes.
-- Returns @base64( nonce || ciphertext || tag )@.
sealBytes :: ByteString -> ByteString -> ByteString -> ByteString -> Either Text Text
sealBytes key nonce aad plaintext = do
  cipher <- mapErr (eitherCryptoError (cipherInit key) :: Either CryptoError AES256)
  aead <- mapErr (eitherCryptoError (aeadInit AEAD_GCM cipher nonce))
  let aead' = aeadAppendHeader aead aad
      (ct, aeadFinal) = aeadEncrypt aead' plaintext
      AuthTag tag = aeadFinalize aeadFinal 16
  Right (TE.decodeUtf8 (B64.encode (nonce <> ct <> BA.convert tag)))
  where
    mapErr = either (Left . T.pack . show) Right

-- | Open a sealed blob (the base64 wire format) back into plaintext bytes.
-- Fails on a bad key, wrong AAD, or any tampering (tag mismatch).
openBytes :: ByteString -> ByteString -> Text -> Either Text ByteString
openBytes key aad blobB64 = do
  blob <- either (Left . T.pack) Right (B64.decode (TE.encodeUtf8 blobB64))
  if BS.length blob < 12 + 16
    then Left "sealed blob too short"
    else do
      let (nonce, rest) = BS.splitAt 12 blob
          (ct, tag) = BS.splitAt (BS.length rest - 16) rest
      cipher <- mapErr (eitherCryptoError (cipherInit key) :: Either CryptoError AES256)
      aead <- mapErr (eitherCryptoError (aeadInit AEAD_GCM cipher nonce))
      let aead' = aeadAppendHeader aead aad
          (pt, aeadFinal) = aeadDecrypt aead' ct
          computed = aeadFinalize aeadFinal 16
      if computed == AuthTag (BA.convert tag)
        then Right pt
        else Left "authentication tag mismatch (tampered, wrong key, or wrong AAD)"
  where
    mapErr = either (Left . T.pack . show) Right

-- | Seal a 'SealInput' with a freshly generated random nonce. AAD is the
-- prompt id. Returns the base64 wire blob.
sealConfig :: ByteString -> Text -> SealInput -> IO (Either Text Text)
sealConfig key promptId si = do
  nonce <- getRandomBytes 12
  pure (sealBytes key nonce (TE.encodeUtf8 promptId) (payloadJson si))

-- | Open a sealed blob and decode the JSON payload back into a 'SealInput'.
openConfig :: ByteString -> Text -> Text -> Either Text SealInput
openConfig key promptId blobB64 = do
  pt <- openBytes key (TE.encodeUtf8 promptId) blobB64
  case A.eitherDecodeStrict pt of
    Left e -> Left (T.pack e)
    Right si -> Right si

instance A.FromJSON SealInput where
  parseJSON = withObject "SealInput" $ \o ->
    SealInput
      <$> o .: "instructions"
      <*> o .: "model"
      <*> (maybe (object []) id <$> o .:? "params")
      <*> o .: "vector_store_id"

-- ---------------------------------------------------------------------------
-- Key file

-- | Contents of the git-controlled @chatty-key.json@: either a single base64
-- deck-config key, or a map from prompt id to base64 key.
data KeyFile
  = SingleKey ByteString
  | KeyMap (KM.KeyMap ByteString)
  deriving (Show, Eq)

instance A.FromJSON KeyFile where
  parseJSON (String s) = SingleKey <$> decodeB64 s
  parseJSON (Object o) =
    KeyMap <$> traverse parseEntry o
    where
      parseEntry (String s) = decodeB64 s
      parseEntry _ = fail "chatty-key.json: each key must be a base64 string"
  parseJSON _ = fail "chatty-key.json must be a base64 string or an object of them"

decodeB64 :: MonadFail m => Text -> m ByteString
decodeB64 t = either fail pure (B64.decode (TE.encodeUtf8 t))

-- | Read and parse the key file. Returns 'Nothing' if the file is absent or
-- unparseable (sealing is then disabled, not an error).
readKeyFile :: FilePath -> IO (Maybe KeyFile)
readKeyFile path = do
  mbs <- (Just <$> BS.readFile path) `catch` \(_ :: SomeException) -> pure Nothing
  pure (mbs >>= A.decodeStrict)

-- | Select the deck-config key for a prompt id from a parsed key file.
lookupKey :: Text -> KeyFile -> Maybe ByteString
lookupKey _ (SingleKey k) = Just k
lookupKey promptId (KeyMap m) = KM.lookup (K.fromText promptId) m
