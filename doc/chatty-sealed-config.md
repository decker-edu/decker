# Chatty Sealed Config — Design

Status: design, not yet implemented (as of 2026-06-23).

## Problem

OpenAI has deprecated the chat-project / stored-prompt mechanism on
platform.openai.com. Chatty currently depends on it.

### Current (committed) behaviour

- `chatty.js` starts a chat by POSTing `{ prompt: { id }, input,
  previous_response_id, stream }` to `<server>/chatty`.
- The `decker-chatty` proxy (`server.mjs`, committed `HEAD`) is a pure
  pass-through: it reads a single `config.apiKey`, forwards `req.body` to
  `https://api.openai.com/v1/responses` adding `Authorization: Bearer <key>`,
  and streams the SSE response back.
- The `prompt.id` reaches OpenAI, which resolves the stored prompt. **The
  system prompt (instructions), the model selection, and the file_search /
  vector-store configuration all live on the platform**, keyed by prompt id.
- Caddy sits in front of the proxy and handles client authentication.

(Note: the working tree of `decker-chatty` contains uncommitted WIP that turns
`config.json` into a per-prompt `{ apiKey, instructions }` registry and injects
instructions in the proxy. That is *not* the deployed reality and is not the
basis for this design.)

When the stored-prompt API is removed, instructions + model + vector store must
be supplied by us instead. They must travel **with the deck**, because:

- the system prompt must **not** be readable or tamperable by the client slide
  deck, yet
- the deck author must be able to **iterate on the prompt and model params
  frequently** by editing markdown and recompiling, without a sysadmin touching
  the proxy each time.

A pure server-side registry (instructions in a sysadmin-edited config file) was
rejected for exactly that reason: prompts change too often. The chosen division
of labour is:

- **Sysadmin (slow-changing):** the secrets, keyed by prompt id.
- **Deck author (fast-changing):** instructions, model params, vector store id,
  authored in deck markdown and sealed at compile time.

## Approach

Encrypt (seal) the author-controlled config against a secret shared between
decker (compile time) and the proxy (runtime), embed the sealed blob in the
published deck, and have the proxy decrypt and apply it. The browser never holds
plaintext or any key and does **zero crypto** — it forwards an opaque blob.

### Crypto spec (both sides depend on this)

- **Primitive:** AES-256-GCM. Node has it built in (no native dep on the
  proxy); Haskell `crypton` supports it. (libsodium `crypto_secretbox` would be
  a nicer API but forces a native dep into the Node proxy — not worth it.)
- **Key:** `deck-config-key` = 32 random bytes, base64. **Separate from the
  OpenAI API key** so the API key (the secret most likely to rotate) can rotate
  freely without breaking every compiled deck. One key per prompt id.
- **Nonce:** 12 random bytes, fresh per seal, prepended to the ciphertext.
- **AAD (authenticated, not encrypted):** the `prompt-id` string. The proxy
  passes the request's prompt id as AAD; a mismatch fails the tag check.
- **Plaintext payload (JSON):**
  ```json
  { "instructions": "...", "model": "gpt-...", "params": { "...": ... },
    "vector_store_id": "vs_..." }
  ```
- **Wire format:** `base64( nonce || ciphertext || tag )`. Opaque to the client.

Binding the prompt id as AAD and sealing the vector store id inside the payload
prevents a client from swapping blobs between decks or redirecting a chat at a
different store.

## Work items

### A. Authoring schema (decker.yaml / deck meta)

Author writes plaintext in meta (source only, never published):

```yaml
chatty:
  prompt: pmpt_...                  # now purely a key selector into proxy config
  server: "https://…/chatty"
  instructions: ./prompts/tutor.md  # path or inline (mirror proxy path/inline rule)
  model: gpt-4.1
  params: { temperature: 0.2 }
  # vector-store-id already exists for upload; reuse it as the sealed value
```

`chatty.prompt` stops meaning "OpenAI stored prompt" and becomes "which key
entry in the proxy config." Reuse the existing `chatty.vector-store-id`.

### B. Decker sealing (Haskell)

1. New module `Text.Decker.Chatty.Seal` (sibling of `Upload.hs`):
   `sealConfig :: ByteString -> SealInput -> IO Text` using `crypton`
   AES-256-GCM. Add `crypton` (+ `memory`) to `package.yaml`;
   `base64-bytestring` is already a dependency.
2. Read the `deck-config-key` from a **git-controlled file in the project**
   (e.g. `chatty-key.json` at the project root), not from the environment.
   Repo access already lets an author read/edit the plaintext instructions in
   the markdown, so committing the sealing key grants no new power to anyone who
   can read the repo — and crucially it does **not** expose the OpenAI API key,
   which stays only in the proxy (the payoff of keeping the two secrets
   separate). The file holds either a single base64 key or a `prompt-id → key`
   map (see open question 1). Two hard constraints:
   - **It must not be `decker.yaml` / deck meta.** Meta flows into `Decker.meta`
     in the published HTML; the key must be read directly by the sealing code
     and never merged into client-facing meta.
   - **It must never be copied to `public/`.** The key file is source-side only;
     publishing it would hand every deck consumer the ability to decrypt and
     forge sealed blobs. Add it to the set of files excluded from output.
3. Assemble the payload from meta (`instructions` resolved file-or-inline,
   `model`, `params`, `vector-store-id`), seal with AAD = prompt id, produce the
   base64 blob.
4. **Inject into client meta** as `chatty.sealed-config`, and **strip the
   plaintext** `instructions`/`model`/`params` from the meta that reaches the
   published HTML. This is the critical confidentiality step — verify the
   plaintext never lands in `Decker.meta` in the output.
5. Wire into the build during HTML writing / meta finalization (not the `chatty`
   upload command, which is about the vector store). If `DECKER_CHATTY_KEY` is
   unset, skip sealing with a notice — don't break non-chatty builds.

### C. Client (chatty.js)

Minimal change (`resource/decker/support/chatty/chatty.js`):

- Read `sealed = Decker.meta.chatty["sealed-config"]` and
  `prompt = Decker.meta.chatty.prompt`.
- Send the sealed blob on **every** request (the proxy is stateless and needs
  model + tools each turn): body `{ promptId, sealed, input,
  previous_response_id, stream: true }`. Drop the old `{ prompt: { id } }`.
- No crypto, no other logic changes.

### D. Proxy (decker-chatty/server.mjs + config.json)

1. **`config.json` schema:** `prompts[id] = { apiKey, deckConfigKey }` (base64).
2. **Per request:** look up entry by `promptId` → 400 if unknown. Decrypt
   `sealed` with `deckConfigKey`, AAD = `promptId`; tag failure → 400 (covers
   tamper, wrong key, blob/id mismatch).
3. **Reject client overrides:** ignore any `model`/`instructions`/`tools` in the
   request body; take them only from the decrypted payload.
4. **Build upstream body:** always set `model`, `params`, and
   `tools: [{ type: "file_search", vector_store_ids: [payload.vector_store_id] }]`;
   set `instructions` only when `!previous_response_id` (first turn). Then stream
   as today.

Note: because the committed proxy currently injects *nothing* of its own
(instructions + model + vector store all come from the stored prompt), the proxy
must learn to inject all three. This is core to the design, not incidental.

### E. Ops / docs

- Document key generation (`openssl rand -base64 32`), that the key lives in a
  git-controlled project file (e.g. `chatty-key.json`), and that it must equal
  the `deckConfigKey` for that prompt id in the proxy.
- Source repos carrying a `chatty-key.json` must stay restricted to authors;
  students receive the published deck, not the repo. If a repo is ever made
  public, rotate the key and recompile its decks.
- Update `decker-chatty/README.md`, `config.json` example, and the chatty
  section of `users-guide/decker-users-guide.md`.
- Rotation: rotating `apiKey` is free; rotating `deckConfigKey` requires
  recompiling decks for that prompt id.

### F. Migration / back-compat

There is no config `instructions` to fall back to, so dual-mode is: a request
*without* a sealed blob falls through to the existing stored-prompt path
(`prompt.id` forwarded as today), keeping old published decks working **until
OpenAI actually removes stored prompts**. New decks send the sealed blob and the
proxy takes the sealed path. The fallback's lifetime is bounded by OpenAI's
deprecation date, not by our recompile schedule.

### G. Testing

- Haskell: round-trip seal/open unit test, with a fixed key+nonce vector shared
  with the JS side to prove cross-implementation compatibility; assert plaintext
  instructions are absent from rendered HTML meta.
- Proxy: unit tests for unknown id, tampered blob, wrong AAD, sealed-field
  override rejection, first-turn vs. subsequent body assembly.
- End-to-end: compile `test/decks/chatty-deck.md` with a key, point at a local
  proxy with the matching key, confirm a chat starts and file_search hits the
  store.

## Open questions

1. Single deck-config key in the key file vs. a prompt-id→key map in it
   (multi-tenant later)? Lean single key now.
2. Which `params` are author-controllable vs. proxy-clamped (cost/safety
   guardrails like max output tokens)?
3. Keep a permanent escape hatch (config-side instructions) or remove all
   fallback once stored prompts are gone?

## Caveat

Sealing protects the prompt at rest in the deck and in transit only. It does not
prevent prompt-injection extraction of the system prompt once the chat is live.
