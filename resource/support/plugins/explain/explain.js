"use strict";

let ExplainPlugin = (function () {
  // GUI elements
  let playPanel, playButton, player;
  let recordPanel, recordIndicator, voiceIndicator, desktopIndicator;
  let recordButton, pauseButton, stopButton;
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
  let explainVideoUrl, explainTimesUrl, explainTimes;

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

    name() {
      return this.state.name;
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

  // Derives path and basename for explain data files from the document
  // location.
  function deckUrlBase() {
    let path = location.pathname;
    return path.substring(0, path.lastIndexOf("-"));
  }

  // Derives the explain video url from the document location.
  function deckVideoUrl() {
    return deckUrlBase() + "-recording.mp4";
  }

  // Derives the explain times url from the document location.
  function deckTimesUrl() {
    return deckUrlBase() + "-times.json";
  }

  // Derives the basenam for explain data downloads from the document location.
  function videoFilenameBase() {
    const pathname = window.location.pathname;
    let filename = pathname.substring(pathname.lastIndexOf("/") + 1);
    filename = filename.substring(0, filename.lastIndexOf("-"));
    return filename;
  }

  // Navigates Reveal to the indexed slide in the explainTimes array.
  function goToSlide(index) {
    if (explainTimes[index]) {
      let slideId = explainTimes[index].slideId;
      var indices = Reveal.getIndices(document.getElementById(slideId));
      Reveal.slide(indices.h, indices.v);
    }
  }

  function goToSlideId(slideId) {
    var indices = Reveal.getIndices(document.getElementById(slideId));
    Reveal.slide(indices.h, indices.v);
  }

  // Jumps the video tp the in-time timestamp stored at index.
  function jumpToTime(index) {
    if (explainTimes[index]) {
      player.currentTime(explainTimes[index].timeIn);
    }
  }

  // Looks up the index of the current Reveal slide in the explainTimes array.
  function currentRevealSlideIndex() {
    let slideId = Reveal.getCurrentSlide().id;
    return explainTimes.findIndex((i) => i.slideId === slideId);
  }

  // Looks up the index of the current slide in the video.
  function currentVideoSlideIndex() {
    let time = player.currentTime();
    return explainTimes.findIndex((i) => i.timeIn <= time && time <= i.timeOut);
  }

  // Jumps the video to the in-time of the next slide.
  function next() {
    jumpToTime(currentVideoSlideIndex() + 1);
  }

  // Jumps the video to the in-time of the previous slide.
  function prev() {
    jumpToTime(currentVideoSlideIndex() - 1);
  }

  // Stops the video and navigates Reveal to the current slide.
  function stop() {
    player.pause();
    goToSlide(currentVideoSlideIndex());
    return true;
  }

  // Starts the video at the current Reveal slide.
  function play() {
    let revealIndex = currentRevealSlideIndex();
    let videoIndex = Math.max(0, currentVideoSlideIndex());
    // Only jump the video if not on the correct slide. Otherwise just continue
    // playing.
    if (videoIndex != revealIndex) {
      jumpToTime(revealIndex);
    }

    player.play();

    // we have to focus the player, otherwise keyboard events are sent to the slide
    // we have to delay this a bit, since first the playButton gets the focus due
    // to the mouse click.
    setTimeout(() => {
      player.focus();
    }, 100);

    return true;
  }

  // Manages recording tim stamps of Reval slide changes. For each slide shown
  // the time interval is stored. Slides can appear many times in a video.
  // Navigation can be random.
  class Timing {
    constructor() {
      this.timeIntervals = [];
      this.pauseDuration = 0;
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
      goToSlideId(this.pauseSlideId);
    }

    // Calculates the video stamp stamp for right now.
    timeStamp() {
      return String((Date.now() - this.startTime - this.pauseDuration) / 1000);
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
      console.log(json);
      return new Blob([json], {
        type: "application/json",
      });
    }
  }

  async function setupRecorder() {
    const config = Reveal.getConfig().explain;
    const recWidth =
      config && config.recWidth ? config.recWidth : Reveal.getConfig().width;
    const recHeight =
      config && config.recHeight ? config.recHeight : Reveal.getConfig().height;
    const camWidth = config && config.camWidth ? config.camWidth : 1280;
    const camHeight = config && config.camHeight ? config.camHeight : 720;

    // get display stream
    console.log("get display stream (" + recWidth + "x" + recHeight + ")");
    desktopStream = await navigator.mediaDevices.getDisplayMedia({
      video: {
        frameRate: 30,
        width: recWidth,
        height: recHeight,
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

    // inform user when tracks get lost
    stream.getTracks().forEach((track) => {
      track.onended = () => {
        alert("VideoRecording: Track " + track.label + " has ended.");
        uiState.transition("cancel");
      };
    });

    // get camera stream
    console.log("get camera stream (" + camWidth + "x" + camHeight + ")");
    cameraStream = await navigator.mediaDevices.getUserMedia({
      video: {
        width: camWidth,
        height: camHeight,
        frameRate: { max: 30 },
      },
      audio: false,
    });
    cameraVideo.srcObject = cameraStream;

    recordButton.disabled = undefined;
    pauseButton.disabled = true;
    stopButton.disabled = true;

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
      Reveal.slide(0);
      recorder.timing = new Timing();
      recorder.timing.start();
      Reveal.addEventListener("slidechanged", (_) => recorder.timing.record());

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

    recorder.onpause = () => {
      recorder.timing.pause();
      updateRecordIndicator();
    };

    recorder.onresume = () => {
      recorder.timing.resume();
      updateRecordIndicator();
    };

    recorder.onerror = (evt) => {
      alert("VideoRecording Error: " + evt.name);
      uiState.transition("cancel");
    };

    recorder.start();
    recordButton.disabled = true;
    pauseButton.disabled = undefined;
    stopButton.disabled = undefined;

    return true;
  }

  function pauseRecording() {
    recorder.pause();
    recordButton.disabled = true;
    pauseButton.disabled = undefined;
    stopButton.disabled = true;
    return true;
  }

  function resumeRecording() {
    recorder.resume();
    recordButton.disabled = true;
    pauseButton.disabled = undefined;
    stopButton.disabled = undefined;
    return true;
  }

  function stopRecording() {
    recorder.stop();
    stream.getTracks().forEach((s) => s.stop());
    recordButton.disabled = undefined;
    pauseButton.disabled = true;
    stopButton.disabled = true;
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
      id: "explain-panel",
      parent: document.body,
    });

    playButton = createElement({
      type: "button",
      id: "explain-play",
      classes: "explain fas fa-play",
      title: "Play video recording",
      parent: document.body,
      onclick: transition("play"),
    });

    let video = createElement({
      type: "video",
      id: "explain-video",
      classes: "video-js",
      parent: playPanel,
    });

    // setup video-js
    player = videojs("explain-video", {
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

    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: recordPanel,
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

    recordButton = createElement({
      type: "button",
      classes: "explain record-button fas fa-play-circle",
      title: "Start recording",
      parent: row,
      onclick: transition("record"),
    });

    pauseButton = createElement({
      type: "button",
      classes: "explain pause-button fas fa-pause-circle",
      title: "Pause/resume recording",
      parent: row,
      onclick: transition("pause"),
    });

    stopButton = createElement({
      type: "button",
      classes: "explain stop-button fas fa-stop-circle",
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

  function createCameraGUI() {
    cameraVideo = createElement({
      type: "video",
      id: "camera-video",
      parent: document.body,
      onclick: (e) => e.target.classList.toggle("fullscreen"),
    });
    cameraVideo.muted = true; // dont' want audio in this stream
  }

  function toggleCamera() {
    if (uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")) {
      if (cameraVideo.classList.toggle("visible")) {
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

  // Reveal ignores key events when modifiers are pressed. We therefore use a "normal" keydown callback.
  // We still add a dummy callback to Reveal, to prevent other plugins
  // to use "our" keys and to add our keys to the help menu.
  Reveal.addKeyBinding(
    { keyCode: 82, key: "R", description: "Toggle Recording" },
    toggleRecording
  );
  Reveal.addKeyBinding(
    { keyCode: 86, key: "V", description: "Toggle Camera" },
    toggleCamera
  );


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

  async function setupPlayer() {
    let config = Reveal.getConfig().explain;
    explainVideoUrl = config && config.video ? config.video : deckVideoUrl();
    explainTimesUrl = config && config.times ? config.times : deckTimesUrl();

    try {
      let videoExists = await resourceExists(explainVideoUrl);
      let timesExists = await resourceExists(explainTimesUrl);

      if (videoExists && timesExists) {
        explainTimes = await fetchResourceJSON(explainTimesUrl);
        player.src({ type: "video/mp4", src: explainVideoUrl });
        return true;
      } else {
        return false;
      }
    } catch (_) {
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
            cancel: { action: stopRecording, next: "INIT" },
            stop: { action: stopRecording, next: "INIT" },
            pause: { action: pauseRecording, next: "RECORDER_PAUSED" },
          },
        },
        RECORDER_PAUSED: {
          name: "RECORDER_PAUSED",
          transition: {
            cancel: { action: stopRecording, next: "INIT" },
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
