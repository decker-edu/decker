{-# LANGUAGE OverloadedStrings #-}

-- | Request signing for the decker-chatty @/sync@ relay.
--
-- Decker does not hold an OpenAI API key. Vector-store requests go to the
-- proxy instead, signed with the prompt's deck-config key from
-- @chatty-key.json@; the proxy verifies the signature and forwards the call to
-- OpenAI with the key it keeps for that prompt.
--
-- Contract (must stay byte-compatible with the Node proxy's @sync.mjs@ — see
-- @test/fixtures/chatty-sync-fixture.json@):
--
--   * headers: @X-Chatty-Prompt@, @X-Chatty-Timestamp@ (unix seconds),
--     @X-Chatty-Signature@.
--   * signature = base64( HMAC-SHA256(deck-config key, canonical) ).
--   * canonical = @decker-chatty-sync-v1@, prompt id, timestamp, method, path
--     with query (relative to the relay root), hex(sha256(body)), joined by
--     newlines.
module Text.Decker.Chatty.Sync
  ( SyncClient (..),
    canonicalRequest,
    signRequest,
    signedHeaders,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (HeaderName)

-- | Where and as whom to sync: the relay URL (@chatty.server@ + @/sync@), the
-- prompt id, and that prompt's 32-byte deck-config key.
data SyncClient = SyncClient
  { scBase :: String,
    scPromptId :: Text,
    scKey :: ByteString
  }

-- | The string that is signed for one request. The path is the already
-- percent-encoded ASCII path and query exactly as sent on the wire.
canonicalRequest :: Text -> Integer -> ByteString -> String -> ByteString -> ByteString
canonicalRequest promptId timestamp method path body =
  BS.intercalate
    "\n"
    [ "decker-chatty-sync-v1",
      TE.encodeUtf8 promptId,
      BS8.pack (show timestamp),
      BS8.map toUpper method,
      BS8.pack path,
      B16.encode (BA.convert (hash body :: Digest SHA256))
    ]

-- | Base64 HMAC-SHA256 of a canonical request.
signRequest :: ByteString -> ByteString -> ByteString
signRequest key canonical =
  B64.encode (BA.convert (hmac key canonical :: HMAC SHA256))

-- | The authentication headers for one request at the given unix time.
signedHeaders :: SyncClient -> Integer -> ByteString -> String -> ByteString -> [(HeaderName, ByteString)]
signedHeaders client timestamp method path body =
  [ ("X-Chatty-Prompt", TE.encodeUtf8 (scPromptId client)),
    ("X-Chatty-Timestamp", BS8.pack (show timestamp)),
    ( "X-Chatty-Signature",
      signRequest (scKey client) (canonicalRequest (scPromptId client) timestamp method path body)
    )
  ]
