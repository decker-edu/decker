"use strict";

let ExplainPlugin = (function () {
  // GUI elements
  let playPanel, playButton, player;
  let recordPanel, recordIndicator, voiceIndicator, desktopIndicator;
  let voiceGainSlider, desktopGainSlider;
  let cameraVideo;

  // recording stuff
  let blobs;
  let recorder;
  let stream;
  let voiceStream, desktopStream, cameraStream;
  let voiceGain, desktopGain;
  let volumeMeter;

  // playback stuff
  let explainVideoUrl, explainTimes;

  let uiState;

  function transition(name) {
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
      console.log(this.uiStates);
    }

    is(name) {
      return name == this.state.name;
    }

    in(...names) {
      return names.reduce((a, name) => a || this.is(name), false);
    }

    // Performs a named transition. In case anything goes wrong, the state
    // remains unchanged and an error message is logged.
    async transition(name) {
      console.log(`[] state: ${this.state.name}, transition: ${name}`);
      let transition = this.state.transition[name];
      if (!transition) {
        console.log("[] no transition named: " + name);
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
        console.log("[] no state named: " + transition.next);
      }
      console.log(`[] current state: ${this.state.name}`);
    }
  }

  // is the user generating a PDF?
  const printMode = /print-pdf/gi.test(window.location.search);

  // GUI helper (uses named parameters)
  function createElement({ type, id, classes, title, parent, onclick = null }) {
    let e = document.createElement(type);
    if (id) e.id = id;
    if (classes) e.className = classes;
    if (title) e.title = title;
    if (parent) parent.appendChild(e);
    if (onclick) e.addEventListener("click", onclick);
    return e;
  }

  function deckUrlBase() {
    let path = location.pathname;
    return path.substring(0, path.lastIndexOf("-"));
  }

  function deckVideoUrl() {
    return deckUrlBase() + "-recording.mp4";
  }

  function deckTimesUrl() {
    return deckUrlBase() + "-times.json";
  }

  function videoFilenameBase() {
    const pathname = window.location.pathname;
    let filename = pathname.substring(pathname.lastIndexOf("/") + 1);
    filename = filename.substring(0, filename.lastIndexOf("-"));
    return filename;
  }

  function videoFilename() {
    return videoFilenameBase() + ".mp4";
  }

  function currentSlide() {
    let time = player.currentTime();
    let slide = 0;
    while (slide < explainTimes.length && time > explainTimes[slide])
      slide += 1;
    return slide - 1;
  }

  function next() {
    let slide = currentSlide() + 1;
    if (explainTimes[slide]) player.currentTime(explainTimes[slide]);
  }

  function prev() {
    let slide = currentSlide() - 1;
    if (explainTimes[slide]) player.currentTime(explainTimes[slide]);
  }

  function stop() {
    player.pause();
    let slide = currentSlide();
    Reveal.slide(slide);
    return true;
  }

  function play() {
    let slide = Reveal.getState().indexh;
    if (currentSlide() != slide) {
      if (explainTimes[slide]) player.currentTime(explainTimes[slide]);
      else player.currentTime(0);
    }

    player.play();
    player.focus();
    return true;
  }

  class Timing {
    constructor() {
      this.times = {};
    }

    start(index, slide) {
      this.startTime = Date.now();
      this.record(index, slide);
    }

    record(index, slide) {
      let time = String((Date.now() - this.startTime) / 1000);
      if (!slide.firstShown) {
        console.log("[] " + time + " " + index + ", " + slide.id);
        slide.firstShown = time;
        this.times[time] = {
          slideIndex: index,
          slideId: slide.id,
        };
      } else {
        console.log("[] ignored " + index + ", " + slide.id);
      }
    }

    finish() {
      return new Blob([JSON.stringify(this.times, null, 4)], {
        type: "application/json",
      });
    }
  }

  async function setupRecorder() {
    // get display stream
    console.log("get display stream");
    desktopStream = await navigator.mediaDevices.getDisplayMedia({
      video: {
        frameRate: 30,
        width: 1280,
        height: 720,
        cursor: "always",
        resizeMode: "crop-and-scale",
      },
      audio: true,
    });

    if (!desktopStream) return false;

    if (desktopStream.getAudioTracks().length > 0) {
      let label = desktopStream.getAudioTracks()[0].label;
      desktopIndicator.title = label;
      desktopGainSlider.disabled = false;
    } else {
      desktopIndicator.removeAttribute("title");
      desktopGainSlider.disabled = true;
    }

    // get microphone stream
    console.log("get voice stream");
    voiceStream = await navigator.mediaDevices.getUserMedia({
      video: false,
      audio: {
        echoCancellation: false,
        noiseSuppression: true,
      },
    });
    if (voiceStream.getAudioTracks().length > 0) {
      let label = voiceStream.getAudioTracks()[0].label;
      voiceIndicator.title = label;
      voiceGainSlider.disabled = false;
    } else {
      voiceIndicator.removeAttribute("title");
      voiceGainSlider.disabled = true;
    }

    // merge tracks into recording stream
    const tracks = [
      ...desktopStream.getVideoTracks(),
      ...mergeAudioStreams(desktopStream, voiceStream),
    ];
    stream = new MediaStream(tracks);

    // get camera stream
    console.log("get camera stream");
    cameraStream = await navigator.mediaDevices.getUserMedia({
      video: {
        width: 1280,
        height: 720,
        frameRate: { max: 30 },
      },
      audio: false,
    });
    cameraVideo.srcObject = cameraStream;
    return true;
  }

  function download(blob, name) {
    let url = URL.createObjectURL(blob);
    let anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = name;

    document.body.appendChild(anchor);
    anchor.click();
    document.body.removeChild(anchor);
  }

  function startRecording() {
    // clear blobs array
    blobs = [];

    // setup recorder
    recorder = new MediaRecorder(stream, {
      mimeType: 'video/webm; codecs=h264"',
    });

    recorder.ondataavailable = (e) => blobs.push(e.data);

    recorder.onstart = () => {
      console.log("[] recorder started");
      recorder.timing = new Timing();
      recorder.timing.start(0, Reveal.getSlides()[0]);
      Reveal.slide(0);
      Reveal.addEventListener("slidechanged", (e) =>
        recorder.timing.record(e.indexh, e.currentSlide)
      );

      updateRecordIndicator();
    };

    recorder.onstop = async () => {
      console.log("[] recorder stopped");
      let vblob = new Blob(blobs, { type: "video/webm" });
      let tblob = recorder.timing.finish();

      if (
        !(await guardedUploadBlob(deckTimesUrl(), tblob)) ||
        !(await uploadBlob(deckUrlBase() + "-recording.webm", vblob))
      ) {
        download(vblob, videoFilenameBase() + "-recording.webm");
        download(tblob, videoFilenameBase() + "-times.json");
      }

      recorder = null;
      stream = null;

      updateRecordIndicator();
    };

    recorder.onpause = updateRecordIndicator;
    recorder.onresume = updateRecordIndicator;

    recorder.start();
    return true;
  }

  function pauseRecording() {
    recorder.pause();
    return true;
  }

  function resumeRecording() {
    recorder.resume();
    return true;
  }

  function stopRecording() {
    recorder.stop();
    stream.getTracks().forEach((s) => s.stop());
    return true;
  }

  function createAudioMeter(audioContext, meter) {
    let analyser = audioContext.createAnalyser();
    analyser.fftSize = 512;
    analyser.buffer = new Float32Array(analyser.frequencyBinCount);
    analyser.fade = 0.9;
    analyser.vol = 0;
    analyser.db = 0;

    analyser.draw = function () {
      // get data from analyser node
      analyser.getFloatTimeDomainData(analyser.buffer);
      // compute maximum
      let vol = analyser.buffer.reduce((previous, current) => {
        return Math.max(previous, current);
      });
      // fade out with previous frame
      analyser.vol = Math.max(vol, analyser.vol * analyser.fade);
      // convert to dB, assign to meter
      analyser.db = 20 * Math.log10(Math.max(0.001, analyser.vol));
      meter.value = analyser.db;
      // draw again next frame
      window.requestAnimationFrame(analyser.draw);
    };

    return analyser;
  }

  // adapted from https://paul.kinlan.me/screen-recorderrecording-microphone-and-the-desktop-audio-at-the-same-time/
  const mergeAudioStreams = (desktopStream, voiceStream) => {
    const context = new AudioContext();
    let hasDesktop = false;
    let hasVoice = false;

    // create audio meter
    let audioMeter = createAudioMeter(context, volumeMeter);

    if (desktopStream && desktopStream.getAudioTracks().length > 0) {
      hasDesktop = true;
      // connect gain to slider
      desktopGain = context.createGain();
      desktopGain.gain.value = desktopGainSlider.value;
      desktopGainSlider.gain = desktopGain;
      // connect source->gain->meter
      const source1 = context.createMediaStreamSource(desktopStream);
      source1.connect(desktopGain).connect(audioMeter);
    }

    if (voiceStream && voiceStream.getAudioTracks().length > 0) {
      hasVoice = true;
      // connect gain to slider
      voiceGain = context.createGain();
      voiceGain.gain.value = voiceGainSlider.value;
      voiceGainSlider.gain = voiceGain;
      // connect source->gain->meter
      const source2 = context.createMediaStreamSource(voiceStream);
      source2.connect(voiceGain).connect(audioMeter);
    }

    // connect source(s)->gain(s)->meter->destination
    const destination = context.createMediaStreamDestination();
    audioMeter.connect(destination);
    audioMeter.draw();

    return hasDesktop || hasVoice ? destination.stream.getAudioTracks() : [];
  };

  function createPlayerGUI() {
    playPanel = createElement({
      type: "div",
      id: "dvo-panel",
      parent: document.body,
    });

    playButton = createElement({
      type: "button",
      id: "dvo-play",
      classes: "fas fa-play",
      title: "Play video recording",
      parent: document.body,
      onclick: transition("play"),
    });

    let video = createElement({
      type: "video",
      id: "dvo-video",
      classes: "video-js",
      parent: playPanel,
    });

    // setup video-js
    player = videojs("dvo-video", {
      width: "100%",
      height: "100%",
      controls: true,
      autoplay: false,
      preload: "metadata",
      playbackRates: [0.5, 0.75, 1, 1.25, 1.5, 2],
      controlBar: {
        playToggle: true,
        volumePanel: true,
        currentTimeDisplay: true,
        timeDivider: false,
        durationDisplay: false,
        remainingTimeDisplay: true,
        playbackRateMenuButton: true,
        fullscreenToggle: false,
        pictureInPictureToggle: false,
      },
      userActions: {
        hotkeys: function (event) {
          event.stopPropagation();

          switch (event.code) {
            // space: play/pause
            case "Space":
              if (this.paused()) this.play();
              else this.pause();
              break;

            // left/right: skip slides
            case "ArrowLeft":
              prev();
              break;
            case "ArrowRight":
              next();
              break;

            // esc: stop and hide video
            case "Escape":
              uiState.transition("stop");
              break;

            // t: record time stamp, print to console
            case "KeyT":
              explainTimes.push(Math.floor(player.currentTime()));
              printTimeStamps();
              break;
          }
        },
      },
    });

    player.on("ended", transition("stop"));

    player.on("error", (_) => {
      console.error(
        'ExplainPlugin: Could not open video "' + explainVideoUrl + '"'
      );
      uiState.transition("stop");
    });

    // get videojs button class
    let Button = videojs.getComponent("Button");

    // register and add close button
    let closeButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass("vjs-icon-cancel");
        this.controlText("Close video");
      },
      handleClick: transition("stop"),
    });
    videojs.registerComponent("closeButton", closeButton);
    player.getChild("controlBar").addChild("closeButton", {}, 0);

    // register and add prev button
    let prevButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass("vjs-icon-previous-item");
        this.controlText("Jump to previous slide");
      },
      handleClick: function () {
        prev();
      },
    });
    videojs.registerComponent("prevButton", prevButton);
    player.getChild("controlBar").addChild("prevButton", {}, 1);

    // register and add next button
    let nextButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass("vjs-icon-next-item");
        this.controlText("Jump to next slide");
      },
      handleClick: function () {
        next();
      },
    });
    videojs.registerComponent("nextButton", nextButton);
    player.getChild("controlBar").addChild("nextButton", {}, 3);
  }

  function createRecordingGUI() {
    recordPanel = createElement({
      type: "div",
      id: "record-panel",
      parent: document.body,
    });

    let row;
    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: recordPanel,
    });

    recordIndicator = createElement({
      type: "i",
      id: "record-indicator",
      classes: "indicator",
      parent: row,
    });

    volumeMeter = createElement({
      type: "meter",
      id: "audio-meter",
      parent: row,
    });
    volumeMeter.value = -60;
    volumeMeter.min = -60;
    volumeMeter.low = -20;
    volumeMeter.high = -9;
    volumeMeter.max = 0;

    let controls = createElement({
      type: "div",
      id: "record-controls",
      parent: recordPanel,
    });

    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls,
    });

    let recordButton = createElement({
      type: "button",
      classes: "record-button fas fa-play-circle",
      title: "Start recording",
      parent: row,
      onclick: transition("record"),
    });

    let pauseButton = createElement({
      type: "button",
      classes: "pause-button fas fa-pause-circle",
      title: "Pause/resume recording",
      parent: row,
      onclick: transition("pause"),
    });

    let stopButton = createElement({
      type: "button",
      classes: "stop-button fas fa-stop-circle",
      title: "Stop recording",
      parent: row,
      onclick: transition("stop"),
    });

    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls,
    });

    voiceIndicator = createElement({
      type: "i",
      classes: "indicator fas fa-microphone",
      parent: row,
    });

    voiceGainSlider = createElement({
      type: "input",
      id: "voice-gain-slider",
      classes: "gain-slider",
      title: "Microphone Audio Gain",
      parent: row,
    });
    setupGainSlider(voiceGain, voiceGainSlider);

    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls,
    });

    desktopIndicator = createElement({
      type: "i",
      classes: "indicator fas fa-tv",
      parent: row,
    });

    desktopGainSlider = createElement({
      type: "input",
      id: "desktop-gain-slider",
      classes: "gain-slider",
      title: "Desktop Audio Gain",
      parent: row,
    });
    setupGainSlider(desktopGain, desktopGainSlider);
  }

  function setupGainSlider(gain, slider) {
    slider.type = "range";
    slider.min = 0;
    slider.max = 2;
    slider.step = 0.1;
    slider.storage = "decker-" + slider.id;
    slider.value = localStorage.getItem(slider.storage)
      ? localStorage.getItem(slider.storage)
      : 1.0;

    slider.output = createElement({
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

  function updateRecordIndicator() {
    recordIndicator.dataset.state = recorder ? recorder.state : "";
  }

  function toggleClass(e, c) {
    if (e.classList.contains(c)) {
      e.classList.remove(c);
      return false;
    } else {
      e.classList.add(c);
      return true;
    }
  }

  function createCameraGUI() {
    cameraVideo = createElement({
      type: "video",
      id: "camera-video",
      parent: document.body,
      onclick: (e) => toggleClass(e.target, "fullscreen"),
    });
    cameraVideo.muted = true; // dont' want audio in this stream
  }

  function toggleCamera() {
    if (uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")) {
      console.log(uiState.state.name);
      if (toggleClass(cameraVideo, "visible")) {
        cameraVideo.play();
      } else {
        cameraVideo.pause();
      }
    } else {
      cameraVideo.classList.remove("visible");
    }
  }

  let voiceGainBak = 1.0;
  function toggleMicrophone() {
    if (voiceGainSlider.value == 0) {
      voiceGainSlider.value = voiceGainBak;
    } else {
      voiceGainBak = voiceGainSlider.value;
      voiceGainSlider.value = 0;
    }
    voiceGainSlider.oninput();
  }

  function printTimeStamps() {
    for (let i = 0; i < explainTimes.length; i++) {
      let t = explainTimes[i];
      let h = Math.floor(t / 60 / 60);
      let m = Math.floor(t / 60) - h * 60;
      let s = t % 60;
      let formatted =
        h.toString().padStart(2, "0") +
        ":" +
        m.toString().padStart(2, "0") +
        ":" +
        s.toString().padStart(2, "0");
      console.log("slide " + (i + 1) + ": time " + formatted + " = " + t + "s");
    }
    console.log(explainTimes);
  }

  async function resourceExists(url) {
    return fetch(url, { method: "HEAD" })
      .then((r) => r.ok)
      .catch((_) => {
        return false;
      });
  }

  async function fetchResourceJSON(url) {
    return fetch(url)
      .then((r) => r.json())
      .catch((e) => {
        console.log("[] cannot fetch: " + url + ", " + e);
        return null;
      });
  }

  async function uploadBlob(url, blob) {
    return fetch(url, { method: "PUT", body: blob })
      .then((r) => r.ok)
      .catch((e) => {
        console.log(
          "[] cannot upload " + blob.size + " bytes to: " + url + ", " + e
        );
        return false;
      });
  }

  async function guardedUploadBlob(url, blob) {
    if (
      (await resourceExists(url)) &&
      !confirm("Really overwrite existing recording?")
    ) {
      return false;
    } else {
      return uploadBlob(url, blob);
    }
  }

  // setup key binding
  Reveal.addKeyBinding(
    { keyCode: 65, key: "A", description: "Toggle Microphone" },
    toggleMicrophone
  );
  Reveal.addKeyBinding(
    { keyCode: 82, key: "R", description: "Setup Recording" },
    transition("setupRecorder")
  );
  Reveal.addKeyBinding(
    { keyCode: 86, key: "V", description: "Toggle Camera" },
    toggleCamera
  );

  async function setupPlayer() {
    let config = Reveal.getConfig().explain;

    if (config) {
      if (config.video) {
        explainVideoUrl = config.video;
        if (explainVideoUrl.endsWith("/")) {
          explainVideoUrl += videoFilename();
        }
      }
      explainTimes = config.times ? JSON.parse(config.times) : [0];
      console.log("[] explanation source configured");
    } else if (
      (await resourceExists(deckVideoUrl())) &&
      (await resourceExists(deckTimesUrl()))
    ) {
      explainVideoUrl = deckVideoUrl();
      let deckTimes = await fetchResourceJSON(deckTimesUrl());
      explainTimes = Object.keys(deckTimes);
      console.log("[] explanation source implicit");
    } else {
      console.log("[] no explanation source");
    }

    if (explainVideoUrl && explainTimes) {
      // if we have a video and timing, use it.
      player.src({ type: "video/mp4", src: explainVideoUrl });
      return true;
    } else {
      return false;
    }
  }

  return {
    init: async function () {
      // don't do anything when exporting to PDF
      if (printMode) return;

      createRecordingGUI();
      createPlayerGUI();
      createCameraGUI();

      uiState = new UIState([playPanel, recordPanel, playButton, cameraVideo], {
        INIT: {
          name: "INIT",
          transition: {
            setupPlayer: { action: setupPlayer, next: "PLAYER_READY" },
            setupRecorder: { action: setupRecorder, next: "RECORDER_READY" },
          },
        },
        PLAYER_READY: {
          name: "PLAYER_READY",
          transition: {
            play: { action: play, next: "PLAYING" },
            setupRecorder: { action: setupRecorder, next: "RECORDER_READY" },
          },
        },
        PLAYING: {
          name: "PLAYING",
          transition: {
            stop: { action: stop, next: "PLAYER_READY" },
          },
        },
        RECORDER_READY: {
          name: "RECORDER_READY",
          transition: {
            cancel: { action: null, next: "INIT" },
            record: { action: startRecording, next: "RECORDING" },
          },
        },
        RECORDING: {
          name: "RECORDING",
          transition: {
            stop: { action: stopRecording, next: "INIT" },
            pause: { action: pauseRecording, next: "RECORDER_PAUSED" },
          },
        },
        RECORDER_PAUSED: {
          name: "RECORDER_PAUSED",
          transition: {
            stop: { action: stopRecording, next: "INIT" },
            pause: { action: resumeRecording, next: "RECORDING" },
          },
        },
      });
      // Try to connect to an existing video.
      uiState.transition("setupPlayer");
    },
  };
})();

Reveal.registerPlugin("explain", ExplainPlugin);
