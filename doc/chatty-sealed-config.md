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

## Implementation anchors (traced)

Concrete code locations, so a fresh session can go straight to implementation.

### How deck meta reaches the client (the confidentiality-critical path)

The client does **not** get meta inlined only — it fetches the **entire deck
meta as a JSON file** from `public/` at runtime:

- Templates call `initializeDecker("$decker-meta-url$")`
  (`resource/decker/template/{deck,page,handout,index}.html`).
- `decker-meta-url` is set to a per-output `<hash9>.json` filename, and that
  file is written into the output directory:
  - **Decks / pages / handouts:** `writePandocFile`
    (`src/Text/Decker/Writer/Layout.hs:111-117`):
    `BS.writeFile metaPath (encodePretty (fromPandocMeta meta'))`. The *same*
    `meta'` is also inlined into the HTML template via `writeHtml45String`
    (same function, line 119) — so this one function is the single choke point
    for both the JSON file and the inlined template.
  - **Index page:** `renderIndex`
    (`src/Text/Decker/Filter/Index.hs:280-295`): `BS.writeFile metaPath jsonMeta`
    at line 295, plus meta rendered into the template.
- Both serialize through `fromPandocMeta` (`src/Text/Decker/Internal/Meta.hs:112`).

**Consequence:** the full deck meta is published. `chatty.instructions`,
`chatty.model`, and `chatty.params` will leak into `public/<hash>.json` unless
they are removed from the `Meta` *before* these two write sites. `chatty.*` can
also be set globally in `decker.yaml`, so redaction must apply to every output,
not just chatty decks.

**Redaction seam (work item B.4):** add an `Action` that takes `Meta`, reads the
key file, seals the plaintext chatty fields (using the deck's `chatty.prompt` as
AAD and `chatty.vector-store-id` in the payload), replaces them with
`chatty.sealed-config`, and drops `chatty.instructions/model/params`. Apply it at
the top of **both** `writePandocFile` and `renderIndex`, before `fromPandocMeta`
is called. Applying it there (rather than in the filter pipeline) guarantees both
the JSON file and the inlined template are covered by one transform. If the key
file is absent or `chatty.prompt` unset, pass meta through unchanged.

### Decker sealing module (work item B)

- New module `Text.Decker.Chatty.Seal`, sibling of
  `src/Text/Decker/Chatty/Upload.hs`. Mirror `Upload.hs` for patterns: meta
  lookups use `lookupMetaOrElse` (`Internal/Meta.hs`); it already imports
  `Data.Aeson`, `base16`/`base64` available as deps.
- Add `crypton` (+ `memory` for `ByteArray`/`convert`) to `package.yaml` deps.
- crypton AES-256-GCM sketch:
  ```haskell
  import Crypto.Cipher.AES (AES256)
  import Crypto.Cipher.Types (AEADMode(AEAD_GCM), cipherInit, aeadInit,
                              aeadAppendHeader, aeadEncrypt, aeadFinalize)
  import Crypto.Error (throwCryptoError)
  -- key :: ByteString (32 bytes), nonce :: ByteString (12 bytes), aad = prompt id
  let cipher = throwCryptoError (cipherInit key) :: AES256
      aead   = throwCryptoError (aeadInit AEAD_GCM cipher nonce)
      aead'  = aeadAppendHeader aead aad
      (ct, aeadF) = aeadEncrypt aead' plaintext
      tag    = aeadFinalize aeadF 16   -- AuthTag
  -- blob = base64 (nonce <> ct <> convert tag)
  ```
  Decrypt mirrors with `aeadDecrypt`. Verify exact signatures against the
  `crypton` version pinned by `lts-23.28`.
- Key file read: a small reader for the git-controlled `chatty-key.json` (single
  base64 key or `prompt-id → key` map). Read directly — do **not** route through
  `readDeckerMetaIO`/deck meta.

### Client (work item C)

- `resource/decker/support/chatty/chatty.js`: config read at lines 60-62
  (`server`, `prompt`); add `sealed = window.Decker?.meta?.chatty?.["sealed-config"]`.
- The `fetch` is at line 209; body currently `{ prompt: { id: prompt }, input,
  previous_response_id, stream: true }` (lines 212-217). Change to
  `{ promptId: prompt, sealed, input, previous_response_id, stream: true }`.
- Response-id handling at lines 227 and 321 stays as-is.

### Proxy (work item D)

- `decker-chatty/server.mjs`, **committed HEAD** handler (lines 15-40 of the
  committed version): `const apiKey = config.apiKey; const body = { stream: true,
  ...req.body }` then POST to `/v1/responses`. This is the pure pass-through to
  replace. (The working tree has unrelated WIP — ignore it.)
- `decker-chatty/config.json` HEAD is `{ apiKey, port }`; change to
  `{ port, prompts: { <id>: { apiKey, deckConfigKey } } }`.
- Node decrypt sketch:
  ```js
  const buf = Buffer.from(sealed, "base64");
  const iv = buf.subarray(0, 12), tag = buf.subarray(buf.length - 16);
  const ct = buf.subarray(12, buf.length - 16);
  const d = crypto.createDecipheriv("aes-256-gcm", key, iv);
  d.setAAD(Buffer.from(promptId)); d.setAuthTag(tag);
  const payload = JSON.parse(Buffer.concat([d.update(ct), d.final()]));
  ```
- Build upstream body: always set `model`, `params`,
  `tools: [{ type: "file_search", vector_store_ids: [payload.vector_store_id] }]`;
  set `instructions` only when `!previous_response_id` (preserve the first-turn
  rule the WIP already demonstrates).

### Where chatty meta keys already live

`src/Text/Decker/Reader/Markdown.hs:281-284` adds `chatty.filepath`,
`chatty.url-path`, `chatty.included-from` for the upload markdown — a *different*
concern (vector-store sync), not the client config. `chatty.prompt/server/
instructions/model/params` come from deck YAML frontmatter merged with global
`decker.yaml`; by the time `writePandocFile`/`renderIndex` run, that merge is done.

## Caveat

Sealing protects the prompt at rest in the deck and in transit only. It does not
prevent prompt-injection extraction of the system prompt once the chat is live.

## Step-by-step implementation plan

Ordered by dependency. The crypto byte-contract is the linchpin both languages
must agree on, so it goes first and gets a shared test vector that both the
Haskell and Node sides are checked against.

### Phase 0 — Lock the crypto contract (first)

- **0.1** Write the contract down as a fixture: a fixed 32-byte key (hex),
  12-byte nonce (hex), AAD string (a prompt id), and a known plaintext payload →
  expected base64 blob (`nonce‖ciphertext‖tag`). Generate once with a throwaway
  script, then freeze. Both implementations must reproduce this exact blob — this
  is what prevents a silent Haskell↔Node incompatibility.
- **0.2** Freeze the payload schema:
  `{ instructions, model, params, vector_store_id }`. Decide which `params` keys
  are allowed (e.g. `temperature`, `max_output_tokens`) and which the proxy
  clamps (open question 2).

*Exit:* a checked-in JSON fixture plus the expected blob.

### Phase 1 — Haskell sealing (no build wiring yet)

- **1.1** Add `crypton` + `memory` to `package.yaml`; confirm they resolve under
  `lts-23.28`. Verify the exact `crypton` AEAD signatures against the pinned
  version (the sketch above may need tweaks).
- **1.2** New module `Text.Decker.Chatty.Seal`:
  `data SealInput = SealInput { instructions, model :: Text, params :: Value,
  vectorStoreId :: Text }`; `sealConfig :: ByteString -> ByteString ->
  ByteString -> SealInput -> IO Text` (key, nonce, aad=promptId → base64 blob);
  a pure `openConfig` for round-trip tests.
- **1.3** Key-file reader: read git-controlled `chatty-key.json` directly (single
  base64 key or `prompt-id → key` map); `Nothing` ⇒ sealing disabled.
- **1.4** Unit tests: round-trip; **reproduce the Phase-0 fixture exactly**
  (fixed nonce); tamper test (flipped byte / changed AAD fails `open`).

*Exit:* `stack test` green, fixture matches.

### Phase 2 — Wire sealing into the build + authoring schema

- **2.1** Add `sealChattyMeta :: Meta -> Action Meta`: read
  `chatty.prompt/instructions/model/params/vector-store-id`; if key file +
  prompt present, seal, set `chatty.sealed-config`, and **delete**
  `chatty.instructions/model/params`; else pass through with a notice.
- **2.2** Apply at both choke points before `fromPandocMeta`: top of
  `writePandocFile` (`Writer/Layout.hs:112`) and top of `renderIndex`
  (`Filter/Index.hs:281`).
- **2.3** **Leakage guard test** (security-critical, automated): compile a chatty
  deck with a key and assert `public/<hash>.json` contains `chatty.sealed-config`
  and **not** `instructions`/the prompt text.
- **2.4** Document the new meta keys; `chatty.prompt` is now a key selector.

*Exit:* building the test deck with a key seals + leaks nothing; building without
a key file is unchanged.

### Phase 3 — Client (chatty.js)

- **3.1** Read `sealed` from `Decker.meta.chatty["sealed-config"]` (near line 60).
- **3.2** Change the fetch body (lines 212-217) to
  `{ promptId: prompt, sealed, input, previous_response_id, stream: true }`; send
  `sealed` on **every** request; leave SSE handling untouched.
- **3.3** If `sealed` absent but `prompt` present, keep the old
  `{ prompt: { id } }` shape (supports Phase 5 back-compat); decide keep vs.
  hard-cut.

*Exit:* deck posts the new body shape (verify in devtools).

### Phase 4 — Proxy (decker-chatty)

Branch the repo; **start from committed HEAD**, not the WIP working tree
(stash/discard or explicitly reconcile it first).

- **4.1** `config.json` → `{ port, prompts: { <id>: { apiKey, deckConfigKey } } }`.
- **4.2** Load + validate keys at startup (base64 → 32 bytes; fail fast).
- **4.3** Handler: `promptId` → 400 if unknown; decrypt `sealed` with that
  entry's `deckConfigKey`, AAD = `promptId` → 400 on tag failure; **ignore** any
  client `model`/`instructions`/`tools`; build upstream body with `model`,
  `params`, `tools:[{type:"file_search", vector_store_ids:[vector_store_id]}]`
  always, `instructions` only when `!previous_response_id`; stream as today.
- **4.4** Decrypt unit tests including **the Phase-0 fixture**, unknown id,
  tampered blob, wrong AAD, and rejection of client-supplied fields.

*Exit:* proxy tests green; fixture decrypts to the known payload.

### Phase 5 — End-to-end, back-compat, docs

- **5.1** E2E: generate `chatty-key.json` (`openssl rand -base64 32`), matching
  `deckConfigKey` in proxy config, compile the test deck, run the proxy locally,
  confirm chat starts, model/params apply, `file_search` hits the store.
- **5.2** Back-compat: proxy falls through to the old `prompt.id` pass-through
  when `sealed` is absent (works until OpenAI removes stored prompts); remove
  after all decks are recompiled.
- **5.3** Docs/ops: update `decker-chatty/README.md`, `config.json` example,
  `users-guide` chatty section; document key generation, the git-controlled file,
  proxy match requirement, rotation semantics, restrict/rotate-if-public note.
  Verify `chatty-key.json` is not picked up as a static resource into `public/`.
- **5.4** Update this doc's status from "design" to "implemented" with any
  deviations.

### Sequencing notes

- **Commit/PR boundaries:** Phase 1, Phase 2, Phase 3, Phase 4 (separate repo),
  Phase 5.
- Phases 1–2 and 4 can proceed in **parallel** once Phase 0's fixture exists —
  that is the point of locking the contract first.
- **Riskiest steps:** 2.3 (the leakage invariant — the actual security goal),
  1.1 (crypton API/version drift), 0.1 (without the shared fixture, 1.4 and 4.4
  can both pass while being mutually incompatible).
