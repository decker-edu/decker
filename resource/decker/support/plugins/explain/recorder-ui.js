import * as mainModule from "./explain.js";
import * as recorder from "./recorder.js";
import { greenScreenConfig } from "./recorder.js";

const UI = {
  recordPanel: undefined,
  recordToggle: undefined,
  recordIndicator: undefined,
  volumeMeter: undefined,
  voiceIndicator: undefined,
  voiceGainSlider: undefined,
  desktopIndicator: undefined,
  desktopGainSlider: undefined,
  micIndicator: undefined,
  micSelect: undefined,
  camIndicator: undefined,
  camSelect: undefined,
  cameraCaptureSize: undefined,
  screenCaptureSize: undefined,
  recordingTime: undefined,
  recordButton: undefined,
  pauseButton: undefined,
  stopButton: undefined,
  cameraPanel: undefined,
  cameraVideo: undefined,
  cameraCanvas: undefined,
  updateRecordIndicator: updateRecordIndicator,
  openRecordPanel: openRecordPanel,
  closeRecordPanel: closeRecordPanel,
  toggleRecordPanel: toggleRecordPanel,
};

export default UI;

function focusable(element) {
  return [
    ...element.querySelectorAll(
      'button,[href],select,textarea,input:not([type="hidden"]),[tabindex]:not([tabindex="-1"])'
    ),
  ];
}

export async function createRecordingGUI() {
  UI.recordPanel = mainModule.createElement({
    type: "div",
    id: "record-panel",
    parent: document.body,
  });

  UI.recordToggle = mainModule.createElement({
    type: "button",
    classes: "fas fa-caret-down toggle-button",
    title: "Open Record Menu",
    parent: UI.recordPanel,
    onclick: (event) => {
      toggleRecordPanel();
    },
  });

  let row;

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.recordIndicator = mainModule.createElement({
    type: "i",
    id: "record-indicator",
    classes: "fas indicator",
    parent: row,
  });

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.volumeMeter = mainModule.createElement({
    type: "meter",
    id: "audio-meter",
    parent: row,
  });
  UI.volumeMeter.value = -60;
  UI.volumeMeter.min = -60;
  UI.volumeMeter.low = -20;
  UI.volumeMeter.high = -9;
  UI.volumeMeter.max = 0;

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.voiceIndicator = mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-microphone",
    parent: row,
  });

  UI.voiceGainSlider = mainModule.createElement({
    type: "input",
    id: "voice-gain-slider",
    classes: "gain-slider",
    title: "Microphone Audio Gain",
    parent: row,
  });

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.desktopIndicator = mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-tv",
    parent: row,
  });

  UI.desktopGainSlider = mainModule.createElement({
    type: "input",
    id: "desktop-gain-slider",
    classes: "gain-slider",
    title: "Desktop Audio Gain",
    parent: row,
  });
  setupGainSlider(UI.desktopGainSlider);

  // mic selector
  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.micIndicator = mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-microphone",
    title: "Select microphone",
    parent: row,
  });

  UI.micSelect = mainModule.createElement({
    type: "select",
    id: "mic-select",
    classes: "input-select",
    title: "Select microphone",
    parent: row,
  });
  UI.micSelect.onchange = recorder.captureMicrophone;

  // camera selector
  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.camIndicator = mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-camera",
    title: "Select camera",
    parent: row,
  });

  UI.camSelect = mainModule.createElement({
    type: "select",
    id: "cam-select",
    classes: "input-select",
    title: "Select camera",
    parent: row,
  });
  UI.camSelect.onchange = recorder.captureCamera;

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-camera",
    title: "Camera capture size",
    parent: row,
  });

  UI.cameraCaptureSize = mainModule.createElement({
    type: "span",
    classes: "capture-size",
    parent: row,
  });

  mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-tv",
    title: "Screen capture size",
    parent: row,
  });

  UI.screenCaptureSize = mainModule.createElement({
    type: "span",
    classes: "capture-size",
    parent: row,
  });

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  mainModule.createElement({
    type: "i",
    classes: "indicator fas fa-stopwatch",
    title: "Recording time",
    parent: row,
  });

  UI.recordingTime = mainModule.createElement({
    type: "span",
    classes: "capture-size",
    title: "Recording timer",
    parent: row,
  });

  row = mainModule.createElement({
    type: "div",
    classes: "controls-row",
    parent: UI.recordPanel,
  });

  UI.recordButton = mainModule.createElement({
    type: "button",
    classes: "explain record-button fas fa-play-circle",
    title: "Start recording",
    parent: row,
    onclick: mainModule.transition("record"),
  });

  UI.pauseButton = mainModule.createElement({
    type: "button",
    classes: "explain pause-button fas fa-pause-circle",
    title: "Pause/resume recording",
    parent: row,
    onclick: mainModule.transition("pause"),
  });

  UI.stopButton = mainModule.createElement({
    type: "button",
    classes: "explain stop-button fas fa-stop-circle",
    title: "Stop recording",
    parent: row,
    onclick: mainModule.transition("stop"),
  });

  /* inert everything but the toggle button */
  let able = focusable(UI.recordPanel);
  for (let element of able) {
    element.inert = true;
  }
  UI.recordToggle.inert = false;
  /* inert everything but the toggle button */

  setupGainSlider(UI.voiceGainSlider);
}

function setupGainSlider(slider) {
  slider.type = "range";
  slider.min = 0;
  slider.max = 2;
  slider.step = 0.1;
  slider.storage = "decker-" + slider.id;
  slider.value = localStorage.getItem(slider.storage)
    ? localStorage.getItem(slider.storage)
    : 1.0;

  slider.output = mainModule.createElement({
    type: "output",
    parent: slider.parentElement,
  });

  slider.oninput = function () {
    if (this.gain) this.gain.gain.value = this.value;
    this.output.innerHTML = this.value;
    localStorage.setItem(this.storage, this.value);
  };
  slider.oninput(); // call once to set output
}

export function createCameraGUI() {
  UI.cameraVideo = mainModule.createElement({
    type: "video",
    id: "camera-video",
    classes: "camera-panel",
    parent: document.body,
  });
  UI.cameraVideo.muted = true; // don't want audio in this stream

  UI.cameraCanvas = mainModule.createElement({
    type: "canvas",
    id: "camera-canvas",
    classes: "camera-panel",
    parent: document.body,
  });
  if (greenScreenConfig.gsBackground) {
    UI.cameraCanvas.style.backgroundImage = `url('${greenScreenConfig.gsBackground}')`;
    UI.cameraCanvas.style.backgroundSize = "cover";
    UI.cameraCanvas.setAttribute("data-has-background", true);
  } else {
    UI.cameraCanvas.setAttribute("data-has-background", false);
  }

  // camera panel is set to either cameraVideo or cameraCanvas
  if (greenScreenConfig.useGreenScreen) {
    UI.cameraVideo.style.display = "none";
    UI.cameraPanel = UI.cameraCanvas;
  } else {
    UI.cameraPanel = UI.cameraVideo;
  }

  // initialize translation and scaling
  UI.cameraPanel.dragging = false;
  UI.cameraPanel.dx = 0.0;
  UI.cameraPanel.dy = 0.0;
  UI.cameraPanel.scale = 1.0;
  UI.cameraPanel.transform = "";
  UI.cameraPanel.style.cursor = "inherit";

  // start dragging
  UI.cameraPanel.onmousedown = (e) => {
    if (!UI.cameraPanel.classList.contains("fullscreen")) {
      e.preventDefault();
      UI.cameraPanel.dragging = true;
      UI.cameraPanel.style.cursor = "move";
      UI.cameraPanel.lastX = e.screenX;
      UI.cameraPanel.lastY = e.screenY;

      // translate on mouse move
      UI.cameraPanel.onmousemove = (e) => {
        if (cameraPanel.dragging) {
          const x = e.screenX;
          const y = e.screenY;
          UI.cameraPanel.dx += x - cameraPanel.lastX;
          UI.cameraPanel.dy += y - cameraPanel.lastY;
          UI.cameraPanel.lastX = x;
          UI.cameraPanel.lastY = y;
          UI.cameraPanel.style.transform = `translate(${cameraPanel.dx}px, ${cameraPanel.dy}px) scale(${cameraPanel.scale})`;
        }
      };
    }
  };

  // stop dragging
  UI.cameraPanel.onmouseup = UI.cameraPanel.onmouseleave = (e) => {
    UI.cameraPanel.style.cursor = "inherit";
    UI.cameraPanel.dragging = false;
    UI.cameraPanel.onmousemove = null;
  };

  // use mouse wheel to scale video
  UI.cameraPanel.onmousewheel = (e) => {
    if (!UI.cameraPanel.classList.contains("fullscreen")) {
      UI.cameraPanel.scale += e.deltaY * -0.01;
      UI.cameraPanel.scale = Math.max(0.1, Math.min(10.0, cameraPanel.scale));
      UI.cameraPanel.style.transform = `translate(${cameraPanel.dx}px, ${cameraPanel.dy}px) scale(${cameraPanel.scale})`;
    }
  };

  // use double click on video to toggle fullscreen
  UI.cameraPanel.ondblclick = (e) => {
    if (UI.cameraPanel.classList.toggle("fullscreen")) {
      UI.cameraPanel.transform = UI.cameraPanel.style.transform;
      UI.cameraPanel.style.transform = "";
    } else {
      UI.cameraPanel.style.transform = cameraPanel.transform;
    }
  };
}

export function updateRecordIndicator(recorder) {
  UI.recordIndicator.dataset.state = recorder ? recorder.state : "";
}

export function openRecordPanel() {
  UI.recordPanel.classList.add("open");
  UI.recordToggle.classList.remove("fa-caret-down");
  UI.recordToggle.classList.add("fa-caret-up");
  UI.recordToggle.title = "Close Record Menu";
  UI.recordToggle.setAttribute("aria-label", "Close Record Menu");
  let able = focusable(UI.recordPanel);
  for (let element of able) {
    element.inert = false;
  }
}

export function closeRecordPanel() {
  UI.recordPanel.classList.remove("open");
  UI.recordToggle.classList.remove("fa-caret-up");
  UI.recordToggle.classList.add("fa-caret-down");
  UI.recordToggle.title = "Open Record Menu";
  UI.recordToggle.setAttribute("aria-label", "Open Record Menu");
  let able = focusable(UI.recordPanel);
  for (let element of able) {
    element.inert = true;
  }
  UI.recordToggle.inert = false;
  if (document.activeElement) {
    document.activeElement.blur();
  }
}

function toggleRecordPanel() {
  if (UI.recordPanel.classList.contains("open")) {
    closeRecordPanel();
  } else {
    openRecordPanel();
  }
}
