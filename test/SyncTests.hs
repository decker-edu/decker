{-# LANGUAGE OverloadedStrings #-}

module SyncTests (syncTests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Test.Hspec
import Text.Decker.Chatty.Sync

-- The frozen request-signing contract, mirrored from
-- test/fixtures/chatty-sync-fixture.json and decker-chatty's
-- test/sync.test.mjs.
fxKey :: BS.ByteString
fxKey =
  either (error . ("bad hex: " <>)) id $
    B16.decode "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

fxClient :: SyncClient
fxClient = SyncClient "https://proxy.invalid/sync" "pmpt_fixture_0001" fxKey

fxTimestamp :: Integer
fxTimestamp = 1700000000

signature :: BS.ByteString -> String -> BS.ByteString -> Maybe BS.ByteString
signature method path body =
  lookup "X-Chatty-Signature" (signedHeaders fxClient fxTimestamp method path body)

syncTests :: SpecWith ()
syncTests = describe "Chatty.Sync" $ do
  it "reproduces the frozen Node fixture signatures (cross-impl contract)" $ do
    signature "POST" "/vector_stores/vs_fixture_abc123/files" "{\"file_id\":\"file-abc\"}"
      `shouldBe` Just "lNzZtooSHqfiBfDJbBLRCodotMR+33MER2FrOu2zXWM="
    signature "GET" "/files?purpose=assistants&limit=10000" ""
      `shouldBe` Just "H45jpok0Q1npAjrf73kon+TPOpK+xV1lJDf7Llio6tg="

  it "sends the prompt id and timestamp alongside the signature" $ do
    let hs = signedHeaders fxClient fxTimestamp "GET" "/files" ""
    lookup "X-Chatty-Prompt" hs `shouldBe` Just "pmpt_fixture_0001"
    lookup "X-Chatty-Timestamp" hs `shouldBe` Just "1700000000"

  it "binds the signature to method, path and body" $ do
    let base = signature "POST" "/vector_stores/vs_1/files" "{}"
    signature "DELETE" "/vector_stores/vs_1/files" "{}" `shouldNotBe` base
    signature "POST" "/vector_stores/vs_2/files" "{}" `shouldNotBe` base
    signature "POST" "/vector_stores/vs_1/files" "{ }" `shouldNotBe` base
