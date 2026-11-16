# Recording uploads: implementation plan

Status: steps 1–5 implemented (2026-11-04), except speech recognition (5.5).
Line references are against `490aa38a`. See Implementation notes at the end.

## Goal

Recording artifacts (annotations, slide timings, video, captions,
transcripts) are runtime data. The deck loads them from JavaScript; it is not
built from them. Today they are wired into the build as inputs, and uploads go
through the build loop. Three consequences:

1. Every change to a companion file re-renders its deck.
2. Every upload to the dev server triggers a Shake run and a browser reload.
3. A 90-minute recording is held in browser memory and uploaded as one blob
   when recording stops. When that upload fails, the recording is lost.

Target state:

- Companion files are never build inputs. Builds only copy them to `public/`.
- An upload to the dev server never triggers a build or a reload.
- Annotations are uploaded periodically while presenting.
- Video is streamed to the server in 5 s chunks. A crash loses at most the
  last few seconds, and an interrupted recording can be recovered.

Constraints (decided):

- Exactly one browser ever talks to the dev server. No multi-client sync.
- `-poll.json` is uploadable but intentionally never published.
- No deployment depends on companion files in `public/` without a build.
- The server's part file is the only safety net for video. There is no local
  download fallback and no IndexedDB journal.
- Chunk interval: 5 s.
- An interrupted recording is recovered through a dialog on the next record,
  not automatically.
- The old `/replace` and `/append` endpoints are removed, not kept alongside.

The order below is the implementation order. Each step is one PR.

--------------------------------------------------------------------------------

## Current state

**Build.** The `*-deck.html` rule (`app/Decker.hs:253-265`) renders the deck,
then calls `needPublicIfExists` for `-annot.json`, `-manip.json`,
`-times.json`, `-transcript.json`, `-recording.mp4`, `-recording.vtt` and
`needPublicIfExistsGlob` for `-recording-*.vtt`.

- A `need` inside a rule makes the file an input of that rule. Any companion
  change re-runs the full Pandoc pipeline for the deck.
- `needPublicIfExists` uses Shake's tracked `doesFileExist`, so creating or
  deleting a companion also rebuilds the deck.
- `needPublicIfExistsGlob` uses untracked `liftIO Glob.glob`. A new
  `-recording-de.vtt` is not copied until the deck rebuilds for another reason.
- Deleting a public copy (`removeFileA`) is a side effect of the deck rule.

Nothing at compile time reads these files. `whiteboard.js`, `explain.js` and
`speechrecognition.js` derive the companion URLs from the deck URL at runtime.
The server answers OPTIONS for missing files with 204 (`headDirectory`).

The PDF rule (`app/Decker.hs:267-282`) does depend on annotations, because the
export bakes them in. It `need`s the *source* `-annot.json`, but Chrome loads
the copy in `public/`. That copy exists only because the deck rule made it.

**Uploads.** `uploadResource` (`Server.hs:193`) writes the body to a temp file
in the system temp dir (`uniqueTransientFileName`), then `renamePath`s it into
the project dir next to the deck source.

- The file watcher sees the rename and posts `FileChanged`. `runShakeForever`
  runs Shake, the deck re-renders, and `reloadClients` reloads the browser.
- `renamePath` fails with EXDEV when the system temp dir is on a different
  filesystem, e.g. tmpfs `/tmp` on Linux.
- Video uploads go through `/replace` and `/append` (`Server.hs:147-148`,
  `Video.hs:32`). They post `UploadComplete`. The `runShakeForever` loop applies
  it and then calls `reloadClients` unconditionally (`Shake.hs:208-211`). The
  resulting `.webm`/`.mp4` files in the project dir also trigger the watcher.
- `replaceVideoUpload` deletes existing fragments as soon as the new upload
  arrives (`Video.hs:147-148`).

**Whiteboard.** `autosave` (default on) only saves on `beforeunload`
(`whiteboard.js:1586`). `saveAnnotations` (`whiteboard.js:1064`) both downloads
the file to the browser's Downloads folder and PUTs it to the server.

**Explain recorder.** `recorder.start()` without a timeslice
(`explain.js:774`). Chunks collect in `blobs` (`explain.js:650`). `onstop`
(`explain.js:717`) builds one Blob, downloads it and `times.json`, PUTs
`times.json`, then PUTs the whole video to `/replace` or `/append`.
`speechrecognition.js:295-299` uploads `-recording.vtt` and `-transcript.json`
once at the end.

--------------------------------------------------------------------------------

## Step 1 — Companion files leave the build graph

### 1.1 One definition of companion files

New module `Text.Decker.Internal.Companions`:

- `publishedSuffixes`: `-annot.json`, `-manip.json`, `-times.json`,
  `-transcript.json`, `-recording.mp4`, `-recording.vtt`, and the pattern
  `-recording-*.vtt`.
- `uploadableSuffixes`: the JSON/VTT entries of the published list plus
  `-poll.json`. Replaces `uploadable` in `Server.hs:90`.
- `isCompanion :: FilePath -> Bool` (published or uploadable).
- `isRecordingArtifact :: FilePath -> Bool`: `*-recording*.webm`,
  `*-recording.mp4`, `*.part`, `*.part.json` (see Step 4).

The server, the scanner and the watcher all use these. The lists cannot drift.

### 1.2 Scan companions as targets

- Add `_companions :: Dependencies` to `Targets` (`Project.hs`), filled in
  `scanTargets`. For each deck source, collect existing siblings that match
  `publishedSuffixes` and map `public/<path>` → `<path>`.
- `scanTargetsToFile` runs at the start of every Shake run, so the set is
  current. `readTargetsFile` does not `need` the targets file
  (`Project.hs:118`), so a changed companion set does not rebuild any rule that
  calls `getDeps`.
- Expose the set in `depsToMeta` like `static`. `info --json` may list it.

### 1.3 Copy and prune in phonies

- New phony `companions`: `need $ Map.keys (deps ^. companions)`, then
  prune. Pruning deletes files under `public/` that match `publishedSuffixes`
  and are no longer in the set. Copies go through the existing catch-all copy
  rule (`app/Decker.hs`, priority 2).
- `html`, `decks` and the publish path `need ["companions"]`. `observed`
  (dev server) does not: the server reads companions from the project dir
  (Step 2.1).

### 1.4 Clean up the deck and PDF rules

- Remove the seven `needPublicIfExists*` calls from the `*-deck.html` rule.
  Delete `needPublicIfExists`, `needPublicIfExistsGlob` and `removeFileA` if
  nothing else uses them.
- PDF rule: replace `need [annot]` with `need [publicDir </> annot]` (still
  guarded by Shake's `doesFileExist` on the source). Chrome then reads a copy
  that this rule made sure is current.
- `Transcribe.hs` and `Crrrunch.hs` keep producing source files only. The
  `publicDir <//> "*-recording-*.vtt"` copy rule in `Transcribe.hs:52` is
  redundant with the companion set; remove it if nothing else needs it.

### 1.5 Tests

- Build a deck, add `-annot.json`, build again: the deck HTML is not rebuilt,
  and `public/…-annot.json` exists.
- Change, then delete the companion: no deck rebuild. The public copy follows,
  including deletion.
- Add `-recording-de.vtt`: copied on the next build.
- `-poll.json` is never copied.
- PDF rule: `need`s the public annotation copy (rule-level test or manual).

--------------------------------------------------------------------------------

## Step 2 — The dev server stores uploads without builds or reloads

### 2.1 Serve companions from the project dir

- Add a `staticPolicy` that accepts paths where `isCompanion` or
  `isRecordingArtifact` (minus the `.part` files) is true and resolves them
  against `projectDir`. It must come *before* the `publicDir` static middleware
  (`Server.hs:134`), because middleware runs before routes and the stale public
  copy would otherwise win.
- `headDirectory` (OPTIONS) checks the project dir for these suffixes.
- `explain.js` uses HEAD in `resourceExists`. Verify HEAD goes through the new
  middleware.
- MP4 playback needs Range requests. `staticPolicy` serves via `responseFile`,
  and Warp handles ranges. Verify seeking in a long recording.
- The server never writes to `public/`. `public/` stays build output only.

### 2.2 Keep uploads out of the watcher

- `startWatcher` (`Shake.hs`) drops events where `isCompanion` or
  `isRecordingArtifact` is true.
- Accepted downside: if you edit a companion by hand while serving, the page
  does not reload automatically.

### 2.3 No reload after uploads

- Remove `reloadClients` after `UploadComplete` in `runShakeForever`.
- `handleUploads` (`decker serve`) already does not reload. Keep it that way.

### 2.4 Atomic and safe writes

- Replace `renamePath tmp destination` in `uploadResource` with
  `safeRenameFile` (already used by the video path), or write the temp file
  into the destination directory under a name `isRecordingArtifact` covers
  (`.<name>.part`). Either way, the final step must be an atomic rename on the
  same filesystem.
- Backup on first write: the first time a server process overwrites an
  existing companion, copy the old version to
  `<transientDir>/backup/<relative path>`. One backup per file per server
  session, kept outside the project so it never ends up in git. This protects
  against an autosave that overwrites good annotations with an empty set.

### 2.5 Tests

- Server-level: PUT `-annot.json` → the file is in the project dir, GET
  returns the new content, `public/` is untouched, no `FileChanged` is posted.
- Upload with a temp dir on another filesystem (or a test that simulates
  EXDEV) succeeds.
- The backup is written once, on the first overwrite.

--------------------------------------------------------------------------------

## Step 3 — Periodic annotation upload

All in `whiteboard.js`.

### 3.1 Split persisting from downloading

- `saveAnnotations` keeps both behaviours for the explicit Save button.
- New `persistAnnotations()`: PUT only, no download. Autosave uses only this.

### 3.2 Triggers

All triggers check `unsavedAnnotations` and the existing `autosave` flag.

- 8 s after the last change (stroke end, erase, clear, undo, grid toggle).
  Restart the timer on each change.
- On `slidechanged`.
- A ceiling: at least every 60 s while there are unsaved changes.
- `pagehide` and `visibilitychange` (hidden) with `keepalive` when the body is
  under 64 KiB, as `saveAnnotations` already does. The `beforeunload` save
  stays as a last resort.

### 3.3 Guards

- **Load before save.** Autosave is disabled until `loadAnnotations` has
  finished *positively*: either it parsed a 200 response, or the server
  reported that no file exists yet (404/204). A network error or a parse error
  keeps autosave off for the session and shows a warning. Otherwise a failed
  load followed by an autosave would replace good annotations with an empty
  set.
- **One upload at a time.** If changes arrive while a PUT is running, mark the
  state as unsaved and send again after it finishes. Overlapping PUTs could
  complete in the wrong order.
- **Dev server only.** Probe once at startup with OPTIONS on the annotation
  URL. Only a Decker server answers it. On static hosting, autosave stays off.
- **Clear the dirty flag only on success.** `needToSave(false)` only after a
  2xx response for exactly the snapshot that was sent. If the user drew while
  the request was running, stay dirty.

### 3.4 Tests

Manual checklist (no browser test harness yet):

- Draw, wait 8 s: the file in the project dir is updated, no reload, no
  download.
- Kill the server, draw, restart the server: the next trigger uploads.
- Block the GET of `-annot.json` in DevTools: autosave stays off and a warning
  appears.

--------------------------------------------------------------------------------

## Step 4 — Recording sessions on the server

### 4.1 Files

For a deck `foo-deck.md`, a running session uses:

- `foo-recording.webm.part`: the video bytes received so far.
- `foo-recording.part.json`: the sidecar `{mode: "replace"|"append",
  startedAt}`.
- `foo-times.json.part`: the latest timing snapshot.
- `foo-recording.vtt.part` and `foo-transcript.json.part`: speech recognition
  snapshots (Step 5.5).

They live next to the deck source: same filesystem (so the final rename is
atomic), and they survive a reboot. The watcher ignores them (2.2). They do
not match `existingVideos` (`*-recording*.webm`), the crunch glob, or the
companion scan.

### 4.2 Endpoints

`<deck>` is the deck path without `-deck.html`, as the client already derives
it in `deckUrlBase()`.

| Request | Behaviour |
|---|---|
| `POST /recording/<deck>/start` `{mode}` | If a `.part` exists: 409 with `{mode, size, duration, startedAt}` (recovery case). Otherwise create an empty `.webm.part` and the sidecar, return 200 `{size: 0}`. |
| `PUT /recording/<deck>/chunk?offset=N` | No session: 404. `N == size`: append, flush, `fsync`, return 200 `{size}`. `N < size` and `N + len <= size`: already stored (a retry), return 200 `{size}`. Otherwise 409 `{size}`. |
| `PUT /recording/<deck>/times` | Atomic write of `-times.json.part`. |
| `PUT /recording/<deck>/captions` / `/transcript` | Atomic write of the speech parts. |
| `POST /recording/<deck>/finish` `{size}` | Check `size` against the part file (409 on mismatch). Post `RecordingFinish` to the action channel and return 202. |
| `DELETE /recording/<deck>` | Discard: delete all parts of the session. |

- Byte offsets, not sequence numbers. The part file's size is the only state,
  so the protocol survives a server restart mid-lecture, and a retry can
  never append the same bytes twice.
- Appends for one deck are serialized with a lock (a `Map FilePath (MVar ())`
  in the server state). Scotty handles requests concurrently.
- Path validation: as in `uploadRecording` today (the directory exists, and
  the deck source must exist too).
- Chunk bodies are read with `bodyReader` and appended directly. At 5 s per
  chunk they are a few MB.

### 4.3 Finish

Handled in the action-channel loop (`runShakeForever` and `handleUploads`),
so transcoding stays serialized with builds. No `reloadClients`.

1. Read the sidecar.
2. `replace`: delete existing fragments (`existingVideos`) and the old
   `-recording.mp4`, then rename `.webm.part` → `-recording.webm`.
   `append`: rename to the next sequence number, reusing the numbering in
   `appendVideoUpload` (`Video.hs:154`).
3. Rename `-times.json.part` → `-times.json`, and the speech parts likewise
   if present.
4. Delete the sidecar.
5. With `PoserFlag`: transcode as today (`convertVideoMp4` /
   `concatVideoMp4`).

Old fragments and old timings stay untouched until step 2. A failed or
abandoned recording no longer destroys the previous one, and video and
timings are swapped together.

### 4.4 Remove the old paths

- Remove the `/replace/*` and `/append/*` routes, `uploadRecording`,
  `replaceVideoUpload`'s delete-on-arrival behaviour, and the
  `Replace`/`Append` `UploadOperation` constructors. Move what is still useful
  (sequence numbering, transcoding) into the finish step.
- Keep `/recordings/*` (`listRecordings`). The append/replace dialog uses it.

### 4.5 Tests

Haskell tests:

- The offset decision as a pure function: `(size, offset, length) →
  Append | AlreadyHave | Conflict`.
- An append sequence with a duplicated chunk and an out-of-order chunk
  produces a byte-identical file.
- Finish with `replace` and with `append` produces the expected fragment names
  and leaves no `.part` files.
- Start with an existing part returns 409 with the sidecar data.

--------------------------------------------------------------------------------

## Step 5 — Streaming recorder in the browser

All in `explain.js`, plus `speechrecognition.js` for 5.5.

### 5.1 Session start

- `startRecording` calls `start` with the mode it already determines
  (REPLACE/APPEND dialog). On 409, show the recovery dialog (5.4) first.
- `recorder.start(5000)`. `recorder.requestData()` on pause, so the seconds
  before a pause are uploaded right away.

### 5.2 Upload queue

- `ondataavailable` pushes the chunk onto a queue. The in-memory `blobs` array
  and the final `new Blob(blobs)` are removed.
- A single upload loop sends the head of the queue with the current offset.
  - On 200: drop the chunk and set `offset = size`.
  - On 409: set `offset` to the returned size, drop chunks that are already
    fully stored, and continue. A chunk that is partly stored is a protocol
    error: stop and alert.
  - On a network error: retry with backoff, capped at 5 s.
  - On 404 (session gone): stop recording and alert. The header in the first
    chunk is gone, so later chunks are useless on their own.
- Memory only holds the unconfirmed backlog.

### 5.3 Backlog indicator and stop

- The recording indicator shows the unconfirmed backlog in seconds of video.
  Normal up to 10 s, warning above 15 s, alarm above 60 s, so upload trouble
  shows up in minute 3 instead of after minute 90.
- `beforeunload` while recording or with a backlog: prompt.
- Stop: the last `dataavailable` fires before `onstop`. Wait until the queue
  is empty (the dialog shows progress), PUT the final timing snapshot, then
  `finish`. If the server stays unreachable, the dialog says what is on the
  server (size and duration) and that recovery is possible after a restart.
- The local downloads of the WebM and `times.json` at stop are removed.

### 5.4 Recovery dialog

When `start` returns 409, the dialog shows the duration and start time and
offers:

- **Recover**: `finish` with the sidecar's mode, then continue with the normal
  append/replace dialog. The recovered recording counts as existing.
- **Discard**: `DELETE /recording/<deck>`, then continue.
- **Cancel**: abort the recording start.

The recovered WebM has no duration or seek data, the same as MediaRecorder
output today. The ffmpeg steps already handle that.

### 5.5 Timing and speech snapshots

- `Timing.record()` (on slide change) and `pause()` PUT a snapshot to
  `/recording/<deck>/times`. The snapshot is `timeIntervals` with the open
  interval's `timeOut` set to now. One request at a time, as in 3.3.
- In APPEND mode, `timeIntervals` already starts with the old intervals, so
  the snapshot is always a complete `times.json`.
- `speechrecognition.js` PUTs its VTT and transcript snapshots at the same
  points (or every 30 s while recognizing), replacing the upload at the end.

### 5.6 Tests

Manual checklist:

- Record 2 min: the `.part` file grows every 5 s. Stop → `-recording.webm` and
  `-times.json` exist, no `.part` files, no reload.
- Kill the server for 30 s while recording: the backlog indicator rises, then
  drains after restart, and the final file plays without a gap.
- Close the tab while recording, reopen, press record: the recovery dialog
  appears, and Recover produces a playable recording with matching timings.
- REPLACE with an existing recording, then abort via Discard: the old
  recording and old timings are intact.
- APPEND: a new fragment number, and timings continue from the old end.

--------------------------------------------------------------------------------

## Out of scope

- Multi-client sync of annotations or recordings.
- Fixing timing drift in APPEND mode (the offset comes from the wall-clock
  `timeOut`, not the actual fragment duration). Worth a separate look with
  `ffprobe` at finish time.
- The untracked `targets` meta in rendered decks (`addMetaKeyValue "targets"`)
  is a separate staleness issue.

--------------------------------------------------------------------------------

## Implementation notes

### Step 1 (2026-11-04)

- `Text.Decker.Internal.Companions` holds `publishedSuffixes`,
  `uploadableSuffixes`, `isCompanion`, `isRecordingArtifact` and
  `isCompanionOf` (the deck-specific match the scanner uses). `Server.hs` uses
  `isUploadable` instead of its own list.
- Deviation: `uploadableSuffixes` does not contain the `-recording-*.vtt`
  pattern. Those captions come from `decker transcribe`, the old `uploadable`
  list did not accept them, and the browser never uploads them.
- Deviation: `observed` (the dev server) also `need`s `companions` for now.
  Without it, step 1 alone would stop companions from reaching `public/` while
  serving, and the server still reads them from there until step 2.1. Remove it
  in step 2 (marked with a TODO in `app/Decker.hs`).
- `publish -l` copies only the companions of the selected decks
  (`companionsOfDecks`), as the deck rule did before. `publish` without `-l`
  needs `companions`.
- Pruning skips `public/support` and keeps files that are static targets, so a
  static resource that happens to match a companion suffix is not deleted.
- The `publicDir <//> "*-recording-*.vtt"` rule in `Transcribe.hs` stays. It
  belongs to `transcriptionRules`, which nothing uses (`decker transcribe`
  calls `transcribeAllRecordings`), and its `vtts` phony needs the public
  paths. Removing dead code is out of scope here.
- `info --json` does not list companions. Its entries are keyed by source
  document, and companions are not documents.
- Tests: `test/CompanionTests.hs` covers the suffix logic and a build sequence
  (add, change, pattern caption, `-poll.json`, delete) that checks the deck
  HTML's modification time stays the same. The PDF rule change is not covered
  by a test.

### Step 2 (2026-11-04)

- `isServedFromProject` (in `Companions`) decides what the server takes from
  the project dir: companions and finished recordings, but no `.part` files,
  no `-recording.mp4.list` and nothing under `public/` or `private/`. It is
  the one predicate for the static middleware, OPTIONS and uploads.
- Stale public copies: a middleware after the project-dir one answers GET and
  HEAD with 404 when the companion is missing from the project dir but
  `public/` has a copy. It only blocks in that case, so routes such as
  `/recordings/*` (whose paths also look like recordings) still work.
- Uploads are written to `.<name>.<random>.part` in the destination
  directory and renamed into place, so EXDEV cannot happen. A failed body read
  removes the part file. Uploads into `public/` or `private/` are rejected.
- Deviation: backups go to `<transientDir>/backup/<server start time>/<path>`,
  not `<transientDir>/backup/<path>`. With one fixed path, the next session's
  first overwrite would replace the good backup of the previous session.
- Addition: `isRecordingArtifact` also covers `*-recording.mp4.list`, which
  `concatVideoMp4` writes into the project dir. Otherwise an append upload
  with `-P` would still trigger the watcher.
- The temporary `companions` need of `observed` from step 1 is removed.
- PDF rule: back to `need [annot]` on the source. Chrome gets the annotations
  from the server, which now reads them from the project dir, so the public
  copy is no longer what the export reads. This reverses the step 1 change.
- `explain.js` uses HEAD in `resourceExists`; the static middleware handles
  GET and HEAD, so HEAD goes through the project-dir lookup.
- Tests: `test/ServerTests.hs` runs `decker --server` on a temporary project
  and checks PUT/GET/OPTIONS/HEAD, rejected uploads, the absence of a build (the
  public copy stays old for 2 s, while a deck edit, the control, does build),
  no leftover part files, the single backup, the 404 for a stale public copy
  and a Range request (206) on an MP4.

### Step 3 (2026-11-04)

- `persistAnnotations` PUTs without a download. `saveAnnotations` (the Save
  button, and `explain.js` when a recording stops) downloads and then goes
  through `persistAnnotations`, so manual saves share the one-at-a-time queue.
- Deviation: the server answers OPTIONS with an `X-Decker-Server: 1` header,
  and the probe checks for it. A plain OPTIONS answer is not enough: Apache
  answers OPTIONS with 200 too. This is the only server change.
- Triggers: 8 s idle timer, `slidechanged`, a 60 s ceiling, `pagehide`,
  `visibilitychange` (hidden) and `beforeunload`. A failed PUT is retried by
  the next trigger, at the latest by the ceiling timer, not every 8 s.
- The dirty flag is cleared only when a PUT succeeded and no change was made
  since its snapshot (a change counter). A change during a running PUT
  sends again right after it.
- Page hide while a PUT is running: if that PUT does not contain the latest
  changes, the latest snapshot is sent next to it, because the page may not
  live long enough to wait. Its success does not clear the flag, and the queue
  waits for it before sending again, so the last PUT to complete is always the
  latest snapshot.
- Every PUT uses `keepalive` when the body is under 64 KiB, as before, so a
  running autosave survives an unload.
- Load before save: 404 and 204 count as "no annotations yet". A network
  error, another HTTP error, a parse error or the old format keep autosave off.
  The Auto-save button is disabled and its tooltip carries the warning, which
  is also flashed once and logged. The annotations of the session can still be
  saved with the Save button.
- `beforeunload`: with active autosave it saves and only prompts if the last
  PUT failed. With autosave on but no Decker server (static hosting) it only
  downloads, as the only way to keep the annotations; it no longer PUTs. With
  autosave on but a failed load, it prompts.
- Snapshots leave out laser strokes instead of removing them from the DOM, so
  an autosave does not cut a laser stroke that is still fading or being drawn.
- Tests: `test/ServerTests.hs` checks the header. The manual checklist ran in
  headless Chromium over CDP: idle save (one PUT, no download, no reload, public
  copy untouched), slide change saves at once, a corrupt `-annot.json` keeps
  autosave off and the file untouched, and a server restart: the PUT during
  the outage fails, the next trigger uploads.

### Step 4 (2026-11-04)

- New module `Text.Decker.Server.Recording`: the session files, the pure
  `decideChunk`, start/chunk/snapshot/finish/discard as IO functions, the
  routes (`recordingRoutes`) and `transcodeRecording`. A session exists while
  its sidecar exists.
- Deviation: finish moves the files in the request handler, under the deck's
  lock, and answers 200 `{fragment, transcoding}`, not 202. Only the MP4
  conversion (with `-P`) goes to the action loop, as `TranscodeRecording`, so
  it stays serialized with builds. Doing the moves in the loop would leave a
  window in which a new start or chunk could interleave with an unfinished
  finish.
- Deviation: chunk bodies are read completely before anything is written,
  not streamed into the file. A dropped connection then never leaves a
  partial chunk, which the client could only treat as a protocol error. A
  failed write truncates the file to its old size. Each append is synced
  (`ensureFileDurable` from unliftio, a no-op on Windows).
- Addition: `start` takes an optional `offset` (the seconds at which the
  session starts in the recording, the end of the old one for append). It is
  stored in the sidecar, and the 409 answer reports `duration` as the end of
  the timing snapshot minus the offset, or null without a snapshot.
- Finish with an empty video part discards the session, so stopping right
  after the start does not replace a recording with nothing. A missing video
  part (a finish interrupted after moving it) only moves the snapshots. A
  sidecar that cannot be read counts as append, so finishing never deletes
  the existing recording by mistake.
- Leftover parts without a sidecar are removed on start.
- Files are written with plain `writeFile`/`withBinaryFile` and then synced.
  unliftio's own durable writers create files with mode 0600 (atomic write)
  or 0100 (`withBinaryFileDurable`), which broke the next append and would
  carry over to `public/`.
- Removed: `/replace/*`, `/append/*`, `uploadRecording`, `replaceVideoUpload`,
  `appendVideoUpload`, `VideoOperation` and `UploadComplete`. The fragment
  numbering moved into finish (`moveVideo`).
- Bridge until step 5: `explain.js` uploads the finished recording as one
  session (start, the whole video as one chunk at offset 0, times, finish).
  On a 409 it asks whether to discard the unfinished session. It still
  downloads the WebM and `times.json` at stop. Without this, recording would
  be broken between steps 4 and 5.
- Fix: `mkListFile` wrote `file '../<fragment>'`, which resolves outside the
  project for a deck in the project root, so `-P` concatenation failed there.
  It now writes the bare file name, as `Crunch.hs` does.
- Not changed: `-P` copies the video stream (`tomp4-copy`), which only works
  for H.264. Chromium records VP8 with `mimeType: "video/webm"`, so `-P`
  fails for that and leaves the WebM fragments for `decker crunch`. The loop
  now catches the failure instead of dying.
- `speechrecognition.js` is not loaded by any template, and its `uploadFile`
  was never defined. Step 5.5 has to wire it in first.
- Tests: `test/RecordingTests.hs` covers `decideChunk`, a chunk sequence with
  duplicates and out-of-order chunks, no session, the 409 info with duration,
  replace and append finish (names, no parts left, readable permissions), an
  empty or discarded session keeping the old recording, an unreadable
  sidecar, and the deck path validation. `test/ServerTests.hs` runs a session
  over HTTP and checks that `/replace` is gone. Manual: headless Chromium
  with a faked screen and fake camera and microphone recorded through the
  real `explain.js` UI in replace mode, append mode (fragments `-0`/`-1`,
  times continue), and with an unfinished session on the server (409,
  discard, upload). `-P` with an H.264 file: replace gives a 5 s MP4, append
  a concatenated 10 s MP4.

### Step 5 (2026-11-04)

- `RecordingSession` in `explain.js` holds the queue of unconfirmed chunks,
  each with its byte offset (assigned when queued) and its length in seconds
  of recording time. `request` retries network errors and 5xx with a backoff
  from 0.5 s, capped at 5 s, until the server answers. 200 and 409 both
  confirm the server's size: chunks stored completely are dropped, and the
  next chunk must start exactly there, else (a partly stored chunk, or the
  server has less) the session fails. 404 ends it as lost, other 4xx fail it
  as recoverable. A failed session stops the recording.
- Addition: `GET /recording/<deck>/session` returns the running session or
  404. The recovery dialog has to come before the append/replace dialog
  (the recovered recording counts as existing), but `start` needs the mode,
  which that dialog decides. The browser asks first, then starts.
- A session with an empty video part (the recorder failed right after
  `start`) is discarded without asking.
- Starting fails early, with a message, if the server cannot be reached or is
  not a Decker server, instead of losing the recording at the stop.
- Addition: a timing snapshot is also sent with every chunk, not only on
  slide changes and pause, so a recovered recording's timings end within a
  chunk of its video even after a long time on one slide. `SnapshotUploader`
  sends one at a time, and a newer snapshot replaces one still waiting. The
  final snapshot waits for the one in flight before it is sent.
- Fix: `Timing.timeStamp()` stands still while paused. Before, a stop while
  paused added the pause to the last `timeOut`, and slide changes during a
  pause got times beyond the video. Needed for the pause snapshot anyway.
- Backlog indicator: a number next to the record indicator, which stays
  visible while the panel is closed, shown above 10 s, yellow above 15 s,
  red and blinking above 60 s or after a failure. The panel is opaque while
  it warns. The estimate counts whole chunks, so it rises in 5 s steps.
- Stop: the progress dialog opens after 1 s, so a normal stop does not flash
  it. After 10 s without an answer from the server it says how much video
  the server has and how to recover. It can be hidden; the upload continues.
  A new recording is refused while the last one is still uploading.
- `finish` is retried like the chunks. A 404 after a retried finish counts as
  success (the answer of the first attempt was lost).
- Removed: the WebM and `times.json` downloads at stop, the in-memory
  `blobs`, the step 4 bridge and `download()`. The whiteboard's save at stop
  (with its download) stays.
- Not done: 5.5 for speech recognition. `speechrecognition.js` was taken out
  of the explain plugin in 2022 (`8cb15fef`, "until further confirmation
  about quality of service") and references globals that no longer exist.
  Wiring it back in is a product decision, not part of this step. The
  `/captions` and `/transcript` snapshot endpoints are ready for it.
- Tests: `test/ServerTests.hs` checks the session endpoint. The manual
  checklist ran in headless Chromium with a faked screen, driven over CDP by a
  script that also kills and restarts the server: the part grows in 5 s steps
  and stop gives the WebM and matching timings, no reload, no downloads; a
  25 s outage raises the backlog to the warning and drains after the restart,
  and the video decodes without a gap (43.3 s for 43.2 s of timings);
  closing the tab and recovering gives the part file as the recording, with
  timings within 0.1 s, and it converts to MP4 (ffmpeg warns that the WebM
  "ended prematurely", as expected for a cut stream); discarding keeps the
  old video and timings byte for byte; append makes `-0`/`-1`, the timings
  continue at the old end and the pause is excluded; a stop with the server
  down shows the unreachable text, `beforeunload` prompts, and the upload
  finishes after the restart and closes the dialog.
