import * as playerModule from "./player.js";
import * as recorder from "./recorder.js";
import UI from "./recorder-ui.js";
import * as recorderInterface from "./recorder-ui.js";

// reference to Reveal deck
export let Reveal;

// configuration reference
export let config;

export let uiState;

export let localization;

export function transition(name) {
  return (_) => uiState.transition(name);
}

// The state of this plugins UI. All possible legal states sre encoded in
// `uiStates`. Each state has set of possible transition actions together with
// the target states defined. For each state change, the DOM elements in
// `signal` have their `data-uistate` attribute set to the name of the current
// state.
class UIState {
  constructor(signal, states) {
    this.signal = signal;
    this.uiStates = states;
    this.state = this.uiStates.INIT;
  }

  is(name) {
    return name == this.state.name;
  }

  in(...names) {
    return names.reduce((a, name) => a || this.is(name), false);
  }

  name() {
    return this.state.name;
  }

  // Performs a named transition. In case anything goes wrong, the state
  // remains unchanged and an error message is logged.
  async transition(name) {
    let transition = this.state.transition[name];
    if (!transition) {
      console.warn("[] no transition named: " + name);
      return;
    }

    // A transition action may cancel a transition by returning false.
    try {
      if (transition.action && !(await transition.action())) {
        console.log("[] transition aborted by action: " + name);
        return;
      }
    } catch (e) {
      console.log("[] transition aborted by exception in action: " + e);
      return;
    }

    // The state exit action is meant ot clean up after a state is left.
    try {
      this.state.exit && this.state.exit();
    } catch (e) {
      console.log("[] transition aborted by exception in exit: " + e);
      return;
    }

    if (this.uiStates[transition.next]) {
      // Switch to the new state and signal all elements.
      this.state = this.uiStates[transition.next];
      for (let element of this.signal) {
        element.setAttribute("data-uistate", this.state.name);
      }
    } else {
      console.warn("[] no state named: " + transition.next);
    }
    // console.log(`[] current state: ${this.state.name}`);
  }
}

// GUI helper (uses named parameters)
export function createElement({
  type,
  id,
  classes,
  title,
  parent,
  onclick = null,
}) {
  let e = document.createElement(type);
  if (id) e.id = id;
  if (classes) e.className = classes;
  if (title) e.title = title;
  if (title) e.setAttribute("aria-label", title);
  if (parent) parent.appendChild(e);
  if (onclick) e.addEventListener("click", onclick);
  return e;
}

export const urls = {
  base: undefined,
  webm: undefined,
  video: undefined,
  times: undefined,
  captions: undefined,
};

export let deckname;

function setupStrings() {
  const path = location.pathname;

  urls.base = path.substring(0, path.lastIndexOf("-"));
  urls.webm = urls.base + "-recording.webm";
  urls.video = urls.base + "-recording.mp4";
  urls.times = urls.base + "-times.json";
  urls.captions = urls.base + "-recording.vtt";

  const filename = path.substring(path.lastIndexOf("/") + 1);
  deckname = filename.substring(0, filename.lastIndexOf("-"));
}

// Manages recording time stamps of Reval slide changes. For each slide shown
// the time interval is stored. Slides can appear many times in a video.
// Navigation can be random.
export class Timing {
  constructor() {
    this.timeIntervals = [];
    this.pauseDuration = 0;
    this.offset = 0;
  }

  // Establishes starting time of the recording and records the first slide.
  start() {
    this.startTime = Date.now();
    this.previousSlide = null;
    this.record();
  }

  // Accumulates amount of time pausing.
  pause() {
    this.pauseStart = Date.now();
    this.pauseSlideId = Reveal.getCurrentSlide().id;
  }

  // Resumes passing time. Restore presentation to slide at which pause was
  // initiated.
  resume() {
    this.pauseDuration += Date.now() - this.pauseStart;
    playerModule.goToSlideId(this.pauseSlideId);
  }

  // Calculates the video stamp stamp for right now.
  timeStamp() {
    return String(
      (Date.now() - this.startTime - this.pauseDuration + this.offset) / 1000
    );
  }

  // Records a time stamp.
  record() {
    let time = this.timeStamp();

    if (this.previousSlide) this.previousSlide.timeOut = time;

    let slideId = Reveal.getCurrentSlide().id;
    this.previousSlide = {
      slideId: slideId,
      timeIn: time,
    };

    this.timeIntervals.push(this.previousSlide);
  }

  // Finishes time stamp recording ans returns a Blob containing the data
  // encoded in JSON.
  finish() {
    if (this.previousSlide) this.previousSlide.timeOut = this.timeStamp();
    let json = JSON.stringify(this.timeIntervals, null, 4);
    return new Blob([json], {
      type: "application/json",
    });
  }
}

export function download(blob, name) {
  let url = URL.createObjectURL(blob);
  let anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = name;

  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
  /* TODO Maybe add config variable to enable these, but they get really annoying ... 
  if (window.postNotification) {
    window.postNotification(
      localization.notification_title,
      localization.notification_download_complete(name)
    );
  }
  */
}

export async function resourceExists(url) {
  try {
    const response = await fetch(url, { methdod: "HEAD" });
    return response.status === 200;
  } catch (error) {
    return false;
  }
}

export async function fetchResourceJSON(url) {
  try {
    const response = await fetch(url);
    return response.json();
  } catch (error) {
    console.log("[] cannot fetch: " + url + ", " + error);
    return null;
  }
}

let zPushCount = 0.0;
let lastZPush = null;

// key event to stop recording if pushed 3 times in quick succession
function maybeStopRecording(evt) {
  // only react on key z/Z
  if (evt.keyCode != 90) return;

  // do not react to alt/ctrl/cmd modifier
  if (evt.altKey || evt.ctrlKey || evt.metaKey) return;

  let now = Date.now();
  if (lastZPush && now - lastZPush < 500) {
    lastZPush = now;
    zPushCount++;
  } else {
    lastZPush = now;
    zPushCount = 1;
  }

  if (zPushCount == 3) {
    switch (uiState.name()) {
      case "RECORDING":
      case "RECORDER_PAUSED":
        uiState.transition("stop");
        break;
    }
  }
}

// key event to toggle recording states
function toggleRecording(evt) {
  // only react on key r/R
  if (evt.keyCode != 82) return;

  // do not react to alt/ctrl/cmd modifier
  if (evt.altKey || evt.ctrlKey || evt.metaKey) return;

  switch (uiState.name()) {
    case "INIT":
    case "PLAYER_READY":
      uiState.transition("setupRecorder");
      break;
    case "RECORDER_READY":
      uiState.transition("record");
      break;
    case "RECORDING":
    case "RECORDER_PAUSED":
      uiState.transition("pause");
      break;
  }
}

function setupCallbacks() {
  // Reveal ignores key events when modifiers are pressed. We therefore use a "normal" keydown callback.
  // We still add a dummy callback prevent other plugins
  // to use "our" keys and to add our keys to the help menu.
  Reveal.addKeyBinding(
    { keyCode: 82, key: "R", description: "Toggle Recording" },
    toggleRecording
  );
  Reveal.addKeyBinding(
    { keyCode: 90, key: "Z", description: "Stop Recording (Triple Click)" },
    maybeStopRecording
  );
  Reveal.addKeyBinding(
    { keyCode: 86, key: "V", description: "Toggle Camera" },
    recorder.toggleCamera
  );

  // Intercept page leave when we are recording
  window.addEventListener("beforeunload", (evt) => {
    if (uiState.in("RECORDER_PAUSED", "RECORDING")) {
      evt.preventDefault();
      return (evt.returnValue =
        "Video recording is still in progress. Are you sure?");
      // Modern browsers override the displayed string with a generic message
    }
  });
}

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);

// export the plugin
const Plugin = {
  id: "explain",

  init: async function (deck) {
    Reveal = deck;

    // don't do anything when exporting to PDF
    if (printMode) return;

    // get config options
    config = Reveal.getConfig().explain || {};
    setupStrings();
    recorder.setGreenScreenConfig(config);

    // setup GUI
    await recorderInterface.createRecordingGUI();
    playerModule.createPlayerGUI();
    recorderInterface.createCameraGUI();

    setupCallbacks();

    uiState = new UIState(
      [
        playerModule.playPanel,
        UI.recordPanel,
        playerModule.playButton,
        UI.cameraPanel,
      ],
      {
        INIT: {
          name: "INIT",
          transition: {
            setupPlayer: {
              action: playerModule.setupPlayer,
              next: "PLAYER_READY",
            },
            setupRecorder: {
              action: recorder.setupRecorder,
              next: "RECORDER_READY",
            },
          },
        },
        PLAYER_READY: {
          name: "PLAYER_READY",
          transition: {
            play: { action: playerModule.play, next: "PLAYING" },
            setupRecorder: {
              action: recorder.setupRecorder,
              next: "RECORDER_READY",
            },
          },
        },
        PLAYING: {
          name: "PLAYING",
          transition: {
            stop: { action: playerModule.stop, next: "PLAYER_READY" },
          },
        },
        RECORDER_READY: {
          name: "RECORDER_READY",
          transition: {
            cancel: { action: null, next: "INIT" },
            record: {
              action: recorder.startRecording,
              next: "RECORDING",
            },
          },
        },
        RECORDING: {
          name: "RECORDING",
          transition: {
            cancel: { action: recorder.stopRecording, next: "INIT" },
            stop: { action: recorder.stopRecording, next: "SAVING" },
            pause: {
              action: recorder.pauseRecording,
              next: "RECORDER_PAUSED",
            },
          },
        },
        RECORDER_PAUSED: {
          name: "RECORDER_PAUSED",
          transition: {
            cancel: { action: recorder.stopRecording, next: "INIT" },
            stop: { action: recorder.stopRecording, next: "SAVING" },
            pause: {
              action: recorder.resumeRecording,
              next: "RECORDING",
            },
          },
        },
        SAVING: {
          name: "SAVING",
          transition: {
            complete: {
              action: recorder.completeRecording,
              next: "INIT",
            },
          },
        },
      }
    );
    // Try to connect to an existing video.
    uiState.transition("setupPlayer");
    Decker.addReloadInhibitor(
      () => !uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")
    );

    let lang = navigator.language;

    localization = {
      append: "Append",
      replace: "Replace",
      cancel: "Cancel",
      no_camera_stream: "No camera stream available.",
      replacement_title: "Append or Replace?",
      replacement_warning:
        "There is already a recording for this presentation. \
      Do you want to append to the existing recording or replace it?",
      accept: "Accept",
      abort: "Abort",
      notification_title: "Decker: Video Recording",
      notification_upload_success: (filename) =>
        `The file '${filename}' was successfully uploaded to your deck's directory.`,
      notification_upload_failed: (filename) =>
        `The file '${filename}' could not be uploaded. Please check your downloads for the backup and add them to your project manually.`,
      notification_download_complete: (filename) =>
        `Download of file '${filename}' as backup completed.`,
    };

    if (lang === "de") {
      localization = {
        append: "Anhängen",
        replace: "Ersetzen",
        cancel: "Abbrechen",
        no_camera_stream: "Kein Kamerastream verfügbar.",
        replacement_title: "Anhängen oder Ersetzen?",
        replacement_warning:
          "Es existiert bereits eine Aufnahme. \
        Soll die Aufnahme an das bereits existierende Video angehangen werden oder es ersetzen?",
        accept: "Akzeptieren",
        abort: "Abbrechen",
        notification_title: "Decker: Videoaufnahme",
        notification_upload_success: (filename) =>
          `Die Datei '${filename}' wurde erfolgreich zu Ihrem Foliensatzverzeichnis hinzugefügt.`,
        notification_upload_failed: (filename) =>
          `Die Datei '${filename}' konnte nicht entgegengenommen werden. Bitte suchen Sie in Ihrem Downloadverzeichnis nach einer Sicherungskopie und fügen Sie diese Ihrem Projekt manuell hinzu.`,
        notification_download_complete: (filename) =>
          `Download der Datei '${filename}' als Sicherung abgeschlossen.`,
      };
    }
  },

  playVideo: playerModule.play,
  stopVideo: playerModule.stop,
  isVideoPlaying: () => {
    return uiState.is("PLAYING");
  },
};

export default Plugin;
