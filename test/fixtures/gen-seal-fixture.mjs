// Phase 0: generate the frozen crypto-contract fixture for chatty sealed config.
// AES-256-GCM, wire format: base64( nonce(12) || ciphertext || tag(16) ), AAD = prompt id.
// Run once with a fixed key+nonce to freeze chatty-seal-fixture.json; both the
// Haskell sealing code and the Node proxy must reproduce this exact blob.
import crypto from "node:crypto";

const keyHex = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const nonceHex = "a0a1a2a3a4a5a6a7a8a9aaab";
const promptId = "pmpt_fixture_0001"; // AAD

const payload = {
  instructions: "You are a helpful tutor. Answer concisely.",
  model: "gpt-4.1",
  params: { temperature: 0.2 },
  vector_store_id: "vs_fixture_abc123",
};

const key = Buffer.from(keyHex, "hex");
const iv = Buffer.from(nonceHex, "hex");
const plaintext = Buffer.from(JSON.stringify(payload), "utf8");

const c = crypto.createCipheriv("aes-256-gcm", key, iv);
c.setAAD(Buffer.from(promptId, "utf8"));
const ct = Buffer.concat([c.update(plaintext), c.final()]);
const tag = c.getAuthTag();
const blob = Buffer.concat([iv, ct, tag]).toString("base64");

const fixture = {
  comment:
    "Frozen crypto contract for chatty sealed config (Phase 0). " +
    "Both Haskell and Node must reproduce sealedBlobBase64 from keyHex/nonceHex/aad/payload.",
  keyHex,
  nonceHex,
  aad: promptId,
  payloadJson: JSON.stringify(payload),
  payload,
  sealedBlobBase64: blob,
};

console.log(JSON.stringify(fixture, null, 2));
