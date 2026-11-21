# LLM-generated lecture recordings: design notes

Status: idea, under consideration (2026-11-21). Nothing decided, nothing
implemented. Open questions at the end.

## Goal

Let an LLM produce a Decker lecture recording on its own. It would control the
slide presentation (slide and fragment advance, laser pointer, pen
annotations) while a narration track in a clone of the lecturer's own voice
plays. The result should look and sound like a recording made by the
lecturer with the explain plugin.

## How recording works today

- `plugins/explain/explain.js` records pixels. `getDisplayMedia` captures the
  screen and `getUserMedia` the mic, and both go into one `MediaRecorder`
  webm (`captureScreen`, ~line 317; recorder setup ~line 645).
- Slide changes are logged as timestamps in `-times.json` (`Timing`).
- Pen and laser input (`plugins/whiteboard/whiteboard.js`) appear only as
  pixels in that video. Pen input goes through `pointerType == "pen"` code
  paths (~lines 728, 1375, 1408). Laser strokes are not persisted. Pen strokes
  end up in `-annot.json` through autosave.

## Core design decision

Don't let the LLM drive the browser live. A live agent would be slow,
timing-sensitive and impossible to reproduce. Split the work into three
stages. Only the first stage uses the LLM.

```
deck.md + rendered layout ──► LLM ──► performance script (JSON, reviewable/editable)
                                            │
                         voice-cloned TTS ◄─┘  (narration segments → wav + word timestamps)
                                            │
            deterministic player (headless Chromium over CDP) ──► video + -times.json + .vtt
```

### 1. Performance script (LLM)

For each slide the script holds narration segments with actions attached to
specific words:

```json
{ "slide": "sec-gradient", "segments": [
  { "say": "The key term here is the gradient,",
    "actions": [{ "at": "gradient", "laser": {"target": "#eq-1", "dwell": 1.2} }] },
  { "say": "and note that this sign flips.",
    "actions": [{ "at": "sign", "pen": {"shape": "circle", "target": "#eq-1", "sub": "frac>mi:nth(2)"} }] },
  { "next": "fragment" } ]}
```

- The LLM points at *elements*, not raw pixels. The player turns targets into
  coordinates.
- LLM input: the Markdown source, a per-slide layout map (element ids, kinds,
  bounding boxes in slide coordinates), slide screenshots and speaker notes.
- Existing lecture transcripts serve as "lecture like me" style examples.
- The script is a text artifact a human can review and edit. Only changed
  segments are regenerated.
- A validation pass checks that targets exist and that durations are
  plausible.

### 2. Audio (voice-cloned TTS)

- Synthesize per segment, so editing one sentence only re-synthesizes that
  sentence. Cache by segment hash.
- Word timestamps come from the TTS provider or from a forced aligner
  (WhisperX, Montreal Forced Aligner). They schedule the actions.
- Captions (VTT) come for free because the text is known.

### 3. Playback

- Headless Chromium driven over CDP from Node. This matches the existing
  headless-check setup.
- Synthetic input via `Input.dispatchMouseEvent` with `pointerType: "pen"`,
  `force`, `tiltX/Y`, so the real whiteboard pen code path runs. The laser
  uses the same mechanism with the laser tool selected.
- The player writes `-times.json` directly from the timeline.

The stages map naturally onto Shake rules
(`deck.md → -script.json → segment wavs → -recording.mp4`).

## Making pointing look human

- People point 200–400 ms *before* the word they say.
- Laser: smooth, minimum-jerk paths, a short dwell, a little tremor, resting
  when nothing is pointed at.
- Pen: generated shapes (underline a bbox, circle, arrow, bracket, check mark)
  with pressure and speed profiles plus some noise. Leave handwritten text out
  at first.
- Limit marks per slide in the prompt. Generated lectures tend to
  over-annotate.

## Recording options

| Approach | Pros | Cons |
|---|---|---|
| **A. Decker's own recorder** with Chromium flags `--use-file-for-fake-audio-capture=narration.wav` and auto-accepted capture | Output goes through the existing path | Hard to sync the fake mic with the timeline; real-time only; getDisplayMedia in headless is finicky |
| **B. Real-time screencast + mux afterwards** (`Page.startScreencast` or Playwright video, then ffmpeg adds audio; `-times.json` from the timeline) | Simple, robust; the preferred starting point | Real-time; frames can drop under load |
| **C. Frame-stepped rendering** (virtual time, one screenshot per frame) | Deterministic, no dropped frames, can run faster than real time | Reveal transitions and CSS animations need virtual-time control |
| **D. Vector recording**: no video, Decker replays the timeline (slide times, timed strokes, laser track, audio) live | Tiny, sharp at any resolution, editable, can be re-voiced, searchable; also useful for human recordings | Needs a replay mode in Decker; MP4 export becomes a separate render step |

Start with B so the existing player works unchanged. D is the long-term
direction: a pixel video is the least useful format for something generated
from structured data.

## Voice

- Existing lecture recordings plus their transcripts (from Decker's speech
  recognition) are good fine-tuning data, much better than a short sample.
- Cloud: ElevenLabs (professional voice clone, multilingual, character-level
  timestamps).
- Local: open models such as F5-TTS, Chatterbox and XTTS-v2. Check German
  quality and licences before choosing; XTTS has a non-commercial licence.
- Clones sound good sentence by sentence but get monotone over 45 minutes.
  Per-segment synthesis helps, and the script can mark emphasis and pauses.
- Decide on disclosure to students. The EU AI Act's transparency rules cover
  synthetic audio, and university rules may apply. A cloud provider also
  needs a privacy decision.

## Changes needed in Decker

Most of this lives in an external orchestrator (Node). Decker needs three
small hooks:

1. A per-slide layout dump (element ids and boxes), either as a mode of
   `decker info --json` or as a URL flag that makes the deck expose it.
2. A "puppet" URL flag that hides chrome and UI, fixes transition timing, and
   keeps generated strokes out of `-annot.json` autosave.
3. A way to import an externally made recording (video, times, VTT). This
   overlaps with `doc/recording-uploads-plan.md`.

## Suggested phases

0. **Narration only:** LLM script → cloned TTS → timed slide and fragment
   advance → screencast + ffmpeg, plus `-times.json` and VTT. Tests the parts
   with the most risk: voice quality, script quality and sync.
1. **Laser pointer** anchored to elements via CDP input.
2. **Pen annotations** (shapes, not handwriting).
3. **Vector recording / replay mode** (option D), if 0–2 prove useful.

## Open questions

1. **Purpose:** replace recordings, produce drafts to re-record, or *patch*
   existing human recordings when a slide changes? Patching needs a close
   voice and style match.
2. **Language:** German, English, or both? This narrows the voice options.
3. **Voice privacy:** cloud voice service acceptable, or local only?
4. **Output:** must it be the standard `-recording.mp4` the current player
   shows, or is a replay mode (D) acceptable?
