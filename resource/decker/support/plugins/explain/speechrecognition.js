let SpeechRecognitionImpl = undefined;
let captionIndicator = undefined;
let captionToggleButton = undefined;

function deckTranscriptUrl() {
  return deckUrlBase() + "-transcript.json";
}

/* Check if feature is available
 * Usage tutorial: https://developer.mozilla.org/en-US/docs/Web/API/Web_Speech_API/Using_the_Web_Speech_API */
if (
  !!window.SpeechRecognition ||
  !(typeof webkitSpeechRecognition === "undefined")
) {
  SpeechRecognitionImpl = window.SpeechRecognition || webkitSpeechRecognition;
}

let webSpeech_shouldCaption = false;
let webSpeech_speechRecognition = undefined;
let webSpeech_transcript = undefined;
let webSpeech_restartOnEnd = undefined;
let webSpeech_stopRequest = false; /* Set to true if the user actually wants to stop */
let webSpeech_transcriptionStartTime = undefined;

async function toggleCaptioning() {
  if (!webSpeech_shouldCaption) {
    let options = [
      { text: localization.accept, value: "ACCEPT" }, // Object not yet defined
      { text: localization.abort, value: "ABORT" }, // Object not yet defined
    ];
    let choice = await window.showChoice(
      localization.speech_warning,
      options,
      "warning"
    );
    if (choice.submit !== "ACCEPT") {
      return;
    }
  }
  webSpeech_shouldCaption = !webSpeech_shouldCaption;
  updateCaptionButton();
  updateCaptionIndicatior();
}

function updateCaptionIndicatior() {
  if (webSpeech_shouldCaption) {
    captionIndicator.dataset.state = "captioning";
  } else {
    captionIndicator.dataset.state = "";
  }
}

function updateCaptionButton() {
  captionToggleButton.classList.remove("captioning");
  if (webSpeech_shouldCaption) {
    captionToggleButton.classList.add("captioning");
  }
}

/**
 * Instantiates the speech recognition module and sets its parameters.
 */
function setupSpeechRecognition() {
  if (!webSpeech_transcript) webSpeech_transcript = [];
  if (SpeechRecognitionImpl) {
    let speechRecognition = new SpeechRecognitionImpl();
    speechRecognition.continuous = true;
    speechRecognition.interimResults = true;
    speechRecognition.onstart = onTranscriptionStart;
    speechRecognition.onresult = onTranscriptResult;
    speechRecognition.onerror = onTranscriptError;
    speechRecognition.onend = onTranscriptEnd;
    webSpeech_speechRecognition = speechRecognition;
  }
}

/**
 * Adds text to the transcript and resets the timings.
 * @param text
 */
function addToTranscript(text) {
  if (text) {
    let curTime = recorder.timing.timeStamp();
    webSpeech_transcript.push({
      startTime: webSpeech_transcriptionStartTime,
      endTime: curTime,
      text: text,
    });
    webSpeech_transcriptionStartTime = curTime;
  }
}

/**
 * SpeechRecognition callback.
 */
function onTranscriptionStart() {
  console.log("started a new transcription session");
}

/**
 * Checks if the result is final and adds it to the transcript.
 * @param {*} event
 */
function onTranscriptResult(event) {
  for (let i = event.resultIndex; i < event.results.length; i++) {
    if (event.results[i][0].confidence > 0.4) {
      if (event.results[i].isFinal) {
        addToTranscript(event.results[i][0].transcript);
      }
    }
  }
}

/**
 * Logs the error and forces a retry on the recognition. Is followed by an
 * onend event.
 * @param {*} event
 */
function onTranscriptError(event) {
  console.error(
    "[SPEECH RECOGNITION]: ",
    event.message ? event.message : event.error
  );
  if (
    event.error == "no-speech" ||
    event.error == "audio-capture" ||
    event.error == "network" ||
    event.error == "bad-grammar"
  ) {
    // Was part of thttps://github.com/MidCamp/live-captioning but doesn't matter for us.
    // Leaving this here.
  }
}

/**
 * If the transcription ended before we ended the recording we want to restart
 * the speech recognition.
 */
function onTranscriptEnd() {
  if (webSpeech_stopRequest) {
    //If we WANT it to end while still recording.
    webSpeech_stopRequest = false;
    return;
  }
  if (webSpeech_restartOnEnd || uiState.is("RECORDING")) {
    webSpeech_restartOnEnd = false;
    webSpeech_speechRecognition.start();
  }
}

function transcribeToJSON() {
  let json = JSON.stringify(webSpeech_transcript, null, 4);
  return new Blob([json], {
    type: "application/json",
  });
}

async function fetchTranscriptionFromJSON() {
  let exists = await resourceExists(deckTranscriptUrl());
  if (exists) {
    let obj = await fetchResourceJSON(deckTranscriptUrl());
    if (obj === null) {
      console.error(
        "Failed to fetch transcription even though the resource exists."
      );
      return;
    }
    webSpeech_transcript = obj;
  }
}

//Modified from https://github.com/MidCamp/live-captioning
function formatTimeString(timeString) {
  let time = parseFloat(timeString);

  let seconds = Math.floor(time);
  let milliseconds = Math.floor((time - seconds) * 1000);
  let minutes = Math.floor(seconds / 60);
  let hours = Math.floor(minutes / 60);
  let days = Math.floor(hours / 24);
  let millisecondsSeparator = ".";

  hours = hours - days * 24;
  minutes = minutes - days * 24 * 60 - hours * 60;
  seconds = seconds - days * 24 * 60 * 60 - hours * 60 * 60 - minutes * 60;

  return (
    (hours < 10 ? "0" : "") +
    hours +
    (minutes < 10 ? ":0" : ":") +
    minutes +
    (seconds < 10 ? ":0" : ":") +
    seconds +
    millisecondsSeparator +
    String(milliseconds).padStart(3, "0").substring(0, 3)
  );
}

//Taken from https://github.com/MidCamp/live-captioning
function formatTranscriptTimeStamped(transcript, format) {
  if (transcript) {
    let output = "";
    if (format === "webVTT") {
      output += "WEBVTT\n\n";
    }
    for (let i = 0; i < transcript.length; ++i) {
      output += i + 1 + "\n"; // This is not neccessary and might make editing the result more difficult.
      output +=
        formatTimeString(transcript[i].startTime) +
        " --> " +
        formatTimeString(transcript[i].endTime) +
        "\n";
      output += transcript[i].text + "\n\n";
    }
    return output;
  } else {
    return "No transcript available";
  }
}

/**
 * Load data from the localStorage.
 *
 * @param {*} key
 * @returns
 */
function loadFromLocalStorage(key) {
  let savedJSON;

  if (localStorage) {
    try {
      savedJSON = JSON.parse(localStorage.getItem(key));
    } catch (e) {}
  }

  if (typeof savedJSON !== "undefined") {
    return savedJSON;
  }
}

/**
 * Save the data to localStorage.
 *
 * @param key  - local storage key to save the data under
 * @param data - the data to save
 */
function saveToLocalStorage(key, data) {
  if (data && localStorage) {
    if (typeof data === "object") {
      localStorage.setItem(key, JSON.stringify(data));
    } else {
      localStorage.setItem(key, data);
    }
  }
}

/** Code that needs to run in setupRecorder */
async function recorderSetupInjection() {
  fetchTranscriptionFromJSON();
  setupSpeechRecognition();
}

/** Code that needs to run in recorder.onstart */
async function onStartInjection() {
  captionToggleButton.disabled = true;

  if (recordingType === "REPLACE") {
    webSpeech_transcript = [];
  }
  if (webSpeech_shouldCaption) {
    webSpeech_transcriptionStartTime = recorder.timing.timeStamp();
  }
  if (webSpeech_speechRecognition && webSpeech_shouldCaption) {
    webSpeech_speechRecognition.start();
  }
}

async function onStopInjection() {
  captionToggleButton.disabled = false;
  if (webSpeech_shouldCaption && webSpeech_speechRecognition) {
    webSpeech_stopRequest = true;
    webSpeech_speechRecognition.stop();
  }

  let transcription = formatTranscriptTimeStamped(
    webSpeech_transcript,
    "webVTT"
  );
  let vttblob = new Blob([transcription], { type: "vtt" });
  let transcriptBlob = transcribeToJSON();
  /* Upload data generated by WebSpeech API
   * Only replace if we generated stuff in the first place,
   * else existing stuff generated externally might be overwritten */
  if (webSpeech_shouldCaption) {
    await uploadFile({
      data: vttblob,
      filename: deckCaptioningUrl(),
    });
    await uploadFile({
      data: transcriptBlob,
      filename: deckTranscriptUrl(),
    });

    download(vttblob, videoFilenameBase() + "-recording.vtt");
    download(transcriptBlob, videoFilenameBase() + "-transcript.json");
  }
}

async function onPauseInjection() {
  if (webSpeech_shouldCaption && webSpeech_speechRecognition) {
    webSpeech_stopRequest = true;
    webSpeech_speechRecognition.stop();
  }
}

async function onResumeInjection() {
  if (webSpeech_shouldCaption && webSpeech_speechRecognition) {
    webSpeech_speechRecognition.start();
  }
}

async function createUI() {
  captionIndicator = createElement({
    type: "i",
    id: "caption-indicator",
    classes: "fas indicator",
    parent: row,
  });

  captionToggleButton = createElement({
    type: "button",
    classes: "explain caption-button fas fa-closed-captioning",
    title: "Create Captions while recording",
    parent: toggleRow,
    onclick: toggleCaptioning,
  });
}

async function gainSliderOnChanged() {
  // value === 0
  if (
    webSpeech_shouldCaption &&
    webSpeech_speechRecognition &&
    uiState.is("RECORDING")
  ) {
    webSpeech_stopRequest = true;
    webSpeech_speechRecognition.stop();
  }
  // value > 0
  if (
    webSpeech_shouldCaption &&
    webSpeech_speechRecognition &&
    uiState.is("RECORDING")
  ) {
    webSpeech_speechRecognition.start();
  }
}

function downloadTranscription() {
  let text = formatTranscriptTimeStamped(webSpeech_transcript, "webVTT");
  let blob = new Blob([text], { type: "vtt" });
  let a = document.createElement("a");
  a.download = "transcription.vtt";
  a.href = URL.createObjectURL(blob);
  a.dataset.downloadurl = ["vtt", a.download, a.href].join(":");
  a.style.display = "none";
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  setTimeout(function () {
    URL.revokeObjectURL(a.href);
  }, 1500);
}

let speech_warning_eng =
  "Using this feature will use your Browser's WebSpeech API to transcribe your voice. \
To facilitate this, your voice will be sent to your Browser's manufacturer's Cloud Service \
(Google or Apple). Do you accept this?";

let speech_warning_deu =
  "Diese Funktion wird die eingebaute WebSpeech API Ihres Browsers benutzen, \
um Ihre Stimme zu transkribieren. Die dabei aufgezeichneten Daten werden dazu an den Hersteller \
Ihres Browsers gesendet. Sind Sie damit einverstanden?";
