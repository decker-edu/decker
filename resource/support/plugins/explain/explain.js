"use strict";


let ExplainPlugin = (function () {

  // GUI elements
  let playPanel, playButton, player;
  let recordPanel, recordIndicator, voiceIndicator, desktopIndicator;
  let voiceGainSlider, desktopGainSlider;

  // recording stuff
  let blobs;
  let recorder;
  let stream;
  let voiceStream, desktopStream;
  let voiceGain, desktopGain;
  let volumeMeter;

  // playback stuff
  let explainVideoUrl, explainTimes;


  // GUI helper (uses named parameters)
  function createElement({type, id, classes, title, parent}) {
    let e = document.createElement(type);
    if (id) e.id = id;
    if (classes) e.className = classes;
    if (title) e.title = title;
    if (parent) parent.appendChild(e);
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
    let filename = pathname.substring(pathname.lastIndexOf('/') + 1);
    filename = filename.substring(0, filename.lastIndexOf("-"));
    return filename;
  }

  function videoFilename() {
    return videoFilenameBase() + '.mp4';
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
    playPanel.removeAttribute("data-visible");
    player.pause();
    let slide = currentSlide();
    Reveal.slide(slide);
  }


  function play() {
    playPanel.setAttribute("data-visible", 1);

    let ended = () => {
      player.off("ended", ended);
      stop();
    };

    let time = player.currentTime();
    let slide = Reveal.getState().indexh;
    if (currentSlide(time) != slide) {
      if (explainTimes[slide])
        player.currentTime(explainTimes[slide]);
      else
        player.currentTime(0);
    }

    player.on("ended", ended);
    player.play();
    player.focus();
  }



  class Timing {
    constructor() {
      this.times = {}
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
          slideId: slide.id
        };
      } else {
        console.log("[] ignored " + index + ", " + slide.id);
      }
    }

    finish() {
      return new Blob([JSON.stringify(this.times, null, 4)], {type: "application/json"});
    }
  }


  async function setupRecording() {

    // get display stream
    console.log('get display stream');
    desktopStream = await navigator.mediaDevices.getDisplayMedia({
      video: {
        frameRate: 30,
        width: 1280,
        height: 720,
        cursor: 'always',
        resizeMode: 'crop-and-scale'
      },
      audio: true
    });
    if (desktopStream.getAudioTracks().length > 0)
    {
      let label = desktopStream.getAudioTracks()[0].label;
      desktopIndicator.title = label;
      desktopGainSlider.disabled = false;
    }
    else {
      desktopIndicator.removeAttribute('title');
      desktopGainSlider.disabled = true;
    }


    // get microphone stream
    console.log('get voice stream');
    voiceStream = await navigator.mediaDevices.getUserMedia({
      video: false,
      audio: {
        echoCancellation: false,
        noiseSuppression: true
      }
    });
    if (voiceStream.getAudioTracks().length > 0)
    {
      let label = voiceStream.getAudioTracks()[0].label;
      voiceIndicator.title = label;
      voiceGainSlider.disabled = false;
    }
    else {
      voiceIndicator.removeAttribute('title');
      voiceGainSlider.disabled = true;
    }


    // merge tracks into recording stream
    const tracks = [
      ...desktopStream.getVideoTracks(),
      ...mergeAudioStreams(desktopStream, voiceStream)
    ];
    stream = new MediaStream(tracks);


    // show recording panel
    recordPanel.style.visibility = "visible";
  }


  function download(blob, name) {
    let url = URL.createObjectURL(blob);
    let anchor = document.createElement('a');
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
    recorder = new MediaRecorder(stream, {mimeType: 'video/webm; codecs=h264"'});

    recorder.ondataavailable = (e) => blobs.push(e.data);

    recorder.onstart = () => {
      console.log("[] recorder started")
      recorder.timing = new Timing();
      recorder.timing.start(0, Reveal.getSlides()[0]);
      Reveal.slide(0);
      Reveal.addEventListener('slidechanged',
        e => recorder.timing.record(e.indexh, e.currentSlide));

      updateRecordIndicator();
    };

    recorder.onstop = async () => {
      console.log("[] recorder stopped")
      let vblob = new Blob(blobs, {type: 'video/webm'});
      let tblob = recorder.timing.finish();

      if (
        !await guardedUploadBlob(deckTimesUrl(), tblob) ||
        !await uploadBlob(deckUrlBase() + "-recording.webm", vblob)
      ) {
        download(vblob, videoFilenameBase() + '-recording.webm');
        download(tblob, videoFilenameBase() + '-times.json');
      }

      recorder = null;
      stream = null;

      updateRecordIndicator();
    };

    recorder.onpause = updateRecordIndicator;
    recorder.onresume = updateRecordIndicator;

    recorder.start();
  }


  function pauseRecording() {
    if (recorder) {
      switch (recorder.state)
      {
        case 'paused':
          recorder.resume();
          break;
        case 'recording':
          recorder.pause();
          break;
      }
    }
  }


  function stopRecording() {
    recorder.stop();
    stream.getTracks().forEach(s => s.stop());
  }


  function createAudioMeter(audioContext, meter) {

    let analyser = audioContext.createAnalyser();
    analyser.fftSize = 512; 
    analyser.buffer = new Float32Array(analyser.frequencyBinCount);
    analyser.fade = 0.9;
    analyser.vol = 0;
    analyser.db  = 0;

    analyser.draw = function() {
      // get data from analyser node
      analyser.getFloatTimeDomainData(analyser.buffer);
      // compute maximum
      let vol = analyser.buffer.reduce((previous, current) => { return Math.max(previous, current); });
      // fade out with previous frame
      analyser.vol = Math.max(vol, analyser.vol * analyser.fade);
      // convert to dB, assign to meter
      analyser.db = 20 * Math.log10( Math.max(0.001, analyser.vol) );
      meter.value = analyser.db;
      // draw again next frame
      window.requestAnimationFrame(analyser.draw);
    }

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

    return (hasDesktop || hasVoice) ? destination.stream.getAudioTracks() : [];
  };


  function createPlayerGUI() {
    playPanel = createElement({
      type: "div",
      id: "dvo-panel",
      parent: document.body
    });

    playButton = createElement({
      type: "button",
      id: "dvo-play",
      classes: "dvo-button fas fa-play",
      title: "Play video recording",
      parent: document.body
    });
    playButton.addEventListener("click", play);

    let video = createElement({
      type: "video",
      id: 'dvo-video',
      classes: "video-js",
      parent: playPanel
    });

    // setup video-js
    player = videojs('dvo-video', {
      width: "100%",
      height: "100%",
      controls: true,
      autoplay: false,
      preload: 'metadata',
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
        pictureInPictureToggle: false
      },
      userActions: {
        hotkeys: function (event) {
          event.stopPropagation();

          switch (event.code) {
            // space: play/pause
            case 'Space':
              if (this.paused())
                this.play();
              else
                this.pause();
              break;

            // left/right: skip slides
            case 'ArrowLeft':
              prev();
              break;
            case 'ArrowRight':
              next();
              break;

            // esc: stop and hide video
            case 'Escape':
              stop();
              break;

            // t: record time stamp, print to console
            case 'KeyT':
              explainTimes.push(Math.floor(player.currentTime()));
              printTimeStamps();
              break;
          }
        }
      }
    });

    player.on('error', _ => {
      console.error("ExplainPlugin: Could not open video \"" + explainVideoUrl + "\"");
      playButton.style.visibility = 'hidden';
    });


    // get videojs button class
    let Button = videojs.getComponent('Button');


    // register and add close button
    let closeButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass('vjs-icon-cancel');
        this.controlText('Close video');
      },
      handleClick: function () { stop(); }
    });
    videojs.registerComponent('closeButton', closeButton);
    player.getChild('controlBar').addChild('closeButton', {}, 0);


    // register and add prev button
    let prevButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass('vjs-icon-previous-item');
        this.controlText('Jump to previous slide');
      },
      handleClick: function () { prev(); }
    });
    videojs.registerComponent('prevButton', prevButton);
    player.getChild('controlBar').addChild('prevButton', {}, 1);


    // register and add next button
    let nextButton = videojs.extend(Button, {
      constructor: function () {
        Button.apply(this, arguments);
        this.addClass('vjs-icon-next-item');
        this.controlText('Jump to next slide');
      },
      handleClick: function () { next(); }
    });
    videojs.registerComponent('nextButton', nextButton);
    player.getChild('controlBar').addChild('nextButton', {}, 3);
  }


  function createRecordingGUI() 
  {
    recordPanel = createElement({ 
      type: "div", 
      id: "record-panel", 
      parent: document.body 
    });
    recordPanel.style.visibility = "hidden";


    let row;
    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: recordPanel
    });

    recordIndicator = createElement({
      type: "i", 
      id: "record-indicator", 
      classes: "indicator", 
      parent: row
    });

    volumeMeter = createElement({
      type: "meter", 
      id: "audio-meter", 
      parent: row
    });
    volumeMeter.value = -60;
    volumeMeter.min  = -60;
    volumeMeter.low  = -20;
    volumeMeter.high = -9;
    volumeMeter.max  = 0;


    let controls = createElement({
      type: "div", 
      id: "record-controls", 
      parent: recordPanel
    });
    
    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls
    });

    let recordButton = createElement({
      type: "button", 
      classes: "dvo-button record-button fas fa-play-circle", 
      title: "Start recording",
      parent: row
    });
    recordButton.addEventListener("click", startRecording);

    let pauseButton = createElement({
      type: "button", 
      classes: "dvo-button record-button fas fa-pause-circle",
      title: "Pause/resume recording",
      parent: row
    });
    pauseButton.addEventListener("click", pauseRecording);

    let stopButton = createElement({
      type: "button", 
      classes: "dvo-button record-button fas fa-stop-circle",
      title: "Stop recording",
      parent: row
    });
    stopButton.addEventListener("click", stopRecording);


    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls
    });
    voiceIndicator = createElement({
      type: "i", 
      classes: "indicator fas fa-microphone", 
      parent: row
    });
    voiceGainSlider = createElement({
      type: "input",
      id: "voice-gain-slider",
      classes: "gain-slider",
      title: "Microphone Audio Gain",
      parent: row
    });
    setupGainSlider(voiceGain, voiceGainSlider);


    row = createElement({
      type: "div",
      classes: "controls-row",
      parent: controls
    });
    desktopIndicator = createElement({
      type: "i", 
      classes: "indicator fas fa-tv", 
      parent: row
    });
    desktopGainSlider = createElement({
      type: "input", 
      id: "desktop-gain-slider",
      classes: "gain-slider",
      title: "Desktop Audio Gain",
      parent: row
    });
    setupGainSlider(desktopGain, desktopGainSlider);
  }


  function setupGainSlider(gain, slider) {
    slider.type = "range";
    slider.min = 0;
    slider.max = 2;
    slider.step = 0.1;
    slider.storage = "decker-" + slider.id;
    slider.value = localStorage.getItem(slider.storage) ? localStorage.getItem(slider.storage) : 1.0;

    slider.output = createElement({
      type: "output",
      parent: slider.parentElement
    });

    slider.oninput = function() { 
      if (this.gain) this.gain.gain.value = this.value;
      this.output.innerHTML = this.value;
      localStorage.setItem(this.storage, this.value);
    };
    slider.oninput(); // call once to set output
  }


  function updateRecordIndicator() {
    recordIndicator.dataset.state = recorder ? recorder.state : '';
  }


  function printTimeStamps() {
    for (let i = 0; i < explainTimes.length; i++) {
      let t = explainTimes[i];
      let h = Math.floor(t / 60 / 60);
      let m = Math.floor(t / 60) - (h * 60);
      let s = t % 60;
      let formatted = h.toString().padStart(2, '0') + ':' + m.toString().padStart(2, '0') + ':' + s.toString().padStart(2, '0');
      console.log("slide " + (i + 1) + ": time " + formatted + " = " + t + "s");
    }
    console.log(explainTimes);
  }

  async function resourceExists(url) {
    return fetch(url, {method: "HEAD"})
      .then(r => {
        return r.ok;
      })
      .catch(_ => {
        return false;
      });
  }

  async function fetchResourceJSON(url) {
    return fetch(url)
      .then(r => {
        return r.json();
      })
      .catch(e => {
        console.log("[] cannot fetch: " + url + ", " + e);
        return null;
      });
  }

  async function uploadBlob(url, blob) {
    return fetch(url, {method: "PUT", body: blob})
      .then(r => {
        return r.ok;
      })
      .catch(e => {
        console.log("[] cannot upload " + blob.size + " bytes to: " + url + ", " + e);
        return false;
      })
  }

  async function guardedUploadBlob(url, blob) {
    if (await resourceExists(url) && !confirm("Really overwrite existing recording?"))
      return false;
    else
      return uploadBlob(url, blob);
  }

  // setup key binding
  Reveal.addKeyBinding({keyCode: 82, key: 'R', description: 'Setup Recording'}, setupRecording);


  return {
    init: async function () {

      // get config
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
      } else if (await resourceExists(deckVideoUrl()) && await resourceExists(deckTimesUrl())) {
        explainVideoUrl = deckVideoUrl();
        let deckTimes = await fetchResourceJSON(deckTimesUrl());
        explainTimes = Object.keys(deckTimes);
        console.log("[] explanation source implicit");
      } else {
        console.log("[] no explanation source");
      }

      createRecordingGUI();
      createPlayerGUI();


      // if we have a video, use it
      if (explainVideoUrl && explainTimes) {
        player.src({type: 'video/mp4', src: explainVideoUrl});
      }
      // otherwise let's record one
      else {
        playButton.style.display = 'none';
      }
    }
  };

})();

Reveal.registerPlugin('explain', ExplainPlugin);
