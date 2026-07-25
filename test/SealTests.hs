{-# LANGUAGE OverloadedStrings #-}

module SealTests (sealTests) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Test.Hspec
import Text.Decker.Chatty.MetaSeal (sealChattyMetaIO)
import Text.Decker.Chatty.Seal
import Text.Decker.Internal.Meta (fromPandocMeta, lookupMeta, setMetaValue)
import Text.Pandoc (nullMeta)

-- The frozen Phase-0 crypto contract, mirrored from
-- test/fixtures/chatty-seal-fixture.json. Keeping the vector inline (rather than
-- parsing the JSON at runtime) makes the contract self-documenting in the test.
fxKeyHex :: BS.ByteString
fxKeyHex = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

fxNonceHex :: BS.ByteString
fxNonceHex = "a0a1a2a3a4a5a6a7a8a9aaab"

fxAad :: Text
fxAad = "pmpt_fixture_0001"

fxPayloadJson :: BS.ByteString
fxPayloadJson =
  "{\"instructions\":\"You are a helpful tutor. Answer concisely.\",\"model\":\"gpt-4.1\",\"params\":{\"temperature\":0.2},\"vector_store_id\":\"vs_fixture_abc123\"}"

fxBlob :: Text
fxBlob =
  "oKGio6SlpqeoqaqrnToVQza/cMoBEe68aQni5FL1NmWy1jAJvG8G7hrHBWenGmeL2lY8T3G8RaZ6DeaLZ3gpJgG5aRstJyV8iFLo39DZ6U0Kh4OQgM/LNr7s4Ji9atnFqLK7QJVXSv/c6ijm7vBe2WaMtauw6Xt8p/6IS1A/E/GtQfROSpC3wJZv0Du2TtYWqzb1ZVIGn+lxe6PlqqQIV2LvJILWZtMM4Vm6OPRJ"

unhex :: BS.ByteString -> BS.ByteString
unhex = either (error . ("bad hex: " <>)) id . B16.decode

sealTests :: SpecWith ()
sealTests = describe "Chatty.Seal" $ do
  let key = unhex fxKeyHex
      nonce = unhex fxNonceHex
      aadBytes = TE.encodeUtf8 fxAad

  it "reproduces the frozen Node fixture blob exactly (cross-impl contract)" $
    sealBytes key nonce aadBytes fxPayloadJson `shouldBe` Right fxBlob

  it "opens the frozen fixture blob back to the original plaintext" $
    openBytes key aadBytes fxBlob `shouldBe` Right fxPayloadJson

  it "round-trips a SealInput through sealConfig/openConfig" $ do
    let si =
          SealInput
            { siInstructions = "Be nice.",
              siModel = "gpt-4.1",
              siParams = object ["temperature" .= (0.5 :: Double)],
              siVectorStoreId = "vs_round_trip"
            }
    blob <- either (error . T.unpack) id <$> sealConfig key "pmpt_rt" si
    openConfig key "pmpt_rt" blob `shouldBe` Right si

  it "fails to open with the wrong AAD (prompt id mismatch)" $
    openBytes key (TE.encodeUtf8 "pmpt_wrong") fxBlob `shouldSatisfy` isLeft

  it "fails to open with the wrong key" $ do
    let badKey = BS.replicate 32 0
    openBytes badKey aadBytes fxBlob `shouldSatisfy` isLeft

  it "fails to open a tampered blob (flipped byte)" $ do
    -- Flip the first byte of the base64 payload; still valid base64 length.
    let tampered = case T.uncons fxBlob of
          Just (c, rest) -> T.cons (if c == 'o' then 'p' else 'o') rest
          Nothing -> fxBlob
    openBytes key aadBytes tampered `shouldSatisfy` isLeft

  it "parses a single-key key file and looks up any prompt" $ do
    let kf = A.decodeStrict ("\"" <> b64key <> "\"") :: Maybe KeyFile
    (kf >>= lookupKey "anything") `shouldBe` Just key

  it "parses a prompt-id -> key map and looks up by id" $ do
    let json = "{\"pmpt_a\":\"" <> b64key <> "\"}"
        kf = A.decodeStrict json :: Maybe KeyFile
    (kf >>= lookupKey "pmpt_a") `shouldBe` Just key
    (kf >>= lookupKey "pmpt_missing") `shouldBe` Nothing

  describe "key-file parsing (parseKeyFileBytes)" $ do
    let lookupIn pid r = case r of KeyFileOk kf -> lookupKey pid kf; _ -> Nothing
    it "accepts a bare base64 key (openssl rand -base64 32 > chatty-key.json)" $
      -- A raw key written straight to the file, with a trailing newline.
      lookupIn "anything" (parseKeyFileBytes (b64key <> "\n")) `shouldBe` Just key
    it "accepts a JSON string key" $
      lookupIn "anything" (parseKeyFileBytes ("\"" <> b64key <> "\"")) `shouldBe` Just key
    it "accepts a JSON prompt-id -> key map" $
      lookupIn "p" (parseKeyFileBytes ("{\"p\":\"" <> b64key <> "\"}")) `shouldBe` Just key
    it "reports a wrong-length key rather than silently ignoring it" $
      case parseKeyFileBytes "c2hvcnQ=" of -- "short", 5 bytes
        KeyFileError _ -> pure ()
        other -> expectationFailure ("expected KeyFileError, got " <> show other)
    it "reports garbage rather than silently ignoring it" $
      case parseKeyFileBytes "this is not a key" of
        KeyFileError _ -> pure ()
        other -> expectationFailure ("expected KeyFileError, got " <> show other)

  describe "leakage guard (sealChattyMetaIO)" $ do
    let secret = "TOP SECRET SYSTEM PROMPT do not leak"
        chattyMeta =
          setMetaValue "chatty.prompt" ("pmpt_a" :: Text)
            . setMetaValue "chatty.instructions" (secret :: Text)
            . setMetaValue "chatty.model" ("gpt-4.1" :: Text)
            . setMetaValue "chatty.vector-store-id" ("vs_leak" :: Text)
            $ nullMeta
        renderMeta m = TL.toStrict (TLE.decodeUtf8 (A.encode (fromPandocMeta m)))

    it "seals and removes the plaintext system prompt from published meta" $ do
      (meta', _) <- sealChattyMetaIO (KeyFileOk (SingleKey key)) chattyMeta
      let json = renderMeta meta'
      json `shouldSatisfy` T.isInfixOf "sealed-config"
      json `shouldNotSatisfy` T.isInfixOf secret
      json `shouldNotSatisfy` T.isInfixOf "instructions"

    it "round-trips the sealed blob from published meta back to the prompt" $ do
      (meta', _) <- sealChattyMetaIO (KeyFileOk (SingleKey key)) chattyMeta
      let mBlob = lookupMeta "chatty.sealed-config" meta' :: Maybe Text
      case mBlob of
        Nothing -> expectationFailure "no chatty.sealed-config in sealed meta"
        Just blob ->
          fmap siInstructions (openConfig key "pmpt_a" blob) `shouldBe` Right secret

    it "strips plaintext even when no key is available (fail-safe)" $ do
      (meta', _) <- sealChattyMetaIO KeyFileAbsent chattyMeta
      let json = renderMeta meta'
      json `shouldNotSatisfy` T.isInfixOf secret

    it "leaves non-chatty meta untouched" $ do
      let plain = setMetaValue "title" ("Hello" :: Text) nullMeta
      (meta', notices) <- sealChattyMetaIO (KeyFileOk (SingleKey key)) plain
      renderMeta meta' `shouldBe` renderMeta plain
      notices `shouldBe` []
  where
    -- base64 of the fixture key
    b64key = "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="
