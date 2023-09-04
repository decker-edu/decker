import * as mainModule from "./explain.js";
import { config, urls, localization, Timing, Reveal } from "./explain.js";
import UI from "./recorder-ui.js";

export const greenScreenConfig = {
  useGreenScreen: false,
  gsBackground: undefined,
  gsKey: { r: 0, g: 255, b: 0 },
  gsSimilarity: 0.4,
  gsSmoothness: 0.08,
  gsSpill: 0.1,
};

// recording stuff
let blobs;
let recorder;
let stream;
let voiceStream, desktopStream, cameraStream;
let voiceGain, desktopGain;
let recordingTimer;
let recordingType; //REPLACE or APPEND
let recordingResumeTime;

async function captureScreen() {
  const recWidth = config && config.recWidth ? config.recWidth : undefined;
  const recHeight = config && config.recHeight ? config.recHeight : undefined;

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
    preferCurrentTab: true,
  });

  let video = desktopStream.getVideoTracks()[0].getSettings();
  console.log("display stream size: ", video.width, video.height);
  UI.screenCaptureSize.textContent = `${video.width}x${video.height}`;

  if (desktopStream.getAudioTracks().length > 0) {
    let label = desktopStream.getAudioTracks()[0].label;
    UI.desktopIndicator.title = label;
    UI.desktopGainSlider.disabled = false;
  } else {
    UI.desktopIndicator.removeAttribute("title");
    UI.desktopGainSlider.disabled = true;
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

export async function captureMicrophone() {
  console.log("get voice stream");
  console.log("mic id: " + UI.micSelect.value);

  try {
    voiceStream = await navigator.mediaDevices.getUserMedia({
      video: false,
      audio: {
        deviceId: UI.micSelect.value
          ? { exact: UI.micSelect.value }
          : undefined,
        echoCancellation: false,
        noiseSuppression: true,
      },
    });
  } catch (error) {
    console.error(error);
    voiceStream = undefined;
  }

  // if mic capture succeeded...
  if (voiceStream && voiceStream.getAudioTracks().length > 0) {
    // ...update GUI
    const selectedMicrophone = voiceStream.getAudioTracks()[0].label;
    UI.voiceIndicator.title = selectedMicrophone;
    UI.micIndicator.title = selectedMicrophone;
    UI.voiceGainSlider.disabled = false;
    UI.micSelect.selectedIndex = -1;
    for (let i = 0; i < UI.micSelect.options.length; i++) {
      if (UI.micSelect.options[i].text == selectedMicrophone) {
        UI.micSelect.selectedIndex = i;
        break;
      }
    }
    // ...remember selected mic
    if (UI.micSelect.selectedIndex != -1) {
      // store label, since ID changes after reboot
      localStorage.setItem("decker-microphone", selectedMicrophone);
    }
  }
  // if mic capture failed...
  else {
    UI.voiceIndicator.removeAttribute("title");
    UI.micIndicator.removeAttribute("title");
    UI.voiceGainSlider.disabled = true;
    UI.micSelect.value = null;
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

export async function captureCamera() {
  const camWidth = config && config.camWidth ? config.camWidth : undefined;
  const camHeight = config && config.camHeight ? config.camHeight : undefined;

  console.log("get camera stream (" + camWidth + "x" + camHeight + ")");
  console.log("cam id: " + UI.camSelect.value);

  // get camera stream
  try {
    cameraStream = await navigator.mediaDevices.getUserMedia({
      video: {
        deviceId: UI.camSelect.value
          ? { exact: UI.camSelect.value }
          : undefined,
        width: camWidth,
        height: camHeight,
        frameRate: { max: 30 },
      },
      audio: false,
    });
  } catch (error) {
    console.error(error);
    cameraStream = undefined;
  }

  // if camera capture succeeded...
  if (cameraStream && cameraStream.getVideoTracks().length > 0) {
    // ...update GUI
    const selectedCamera = cameraStream.getVideoTracks()[0].label;
    const cameraSettings = cameraStream.getVideoTracks()[0].getSettings();
    UI.cameraCaptureSize.textContent = `${cameraSettings.width}x${cameraSettings.height}`;
    UI.camIndicator.title = selectedCamera;
    UI.camSelect.selectedIndex = -1;
    for (let i = 0; i < UI.camSelect.options.length; i++) {
      if (UI.camSelect.options[i].text == selectedCamera) {
        UI.camSelect.selectedIndex = i;
        break;
      }
    }
    // ...remember selected camera
    if (UI.camSelect.selectedIndex != -1) {
      // store label, since ID changes after reboot
      localStorage.setItem("decker-camera", selectedCamera);
    }

    // ...connect camera to video element
    if (UI.cameraPanel.classList.contains("visible")) {
      UI.cameraVideo.pause();
      UI.cameraVideo.srcObject = cameraStream;
      UI.cameraVideo.play();
    } else {
      // cameraVideo.srcObject = cameraStream;
    }
  }
  // if camera capture failed...
  else {
    UI.camIndicator.removeAttribute("title");
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

function mergeStreams() {
  const tracks = [
    ...desktopStream.getVideoTracks(),
    ...mergeAudioStreams(desktopStream, voiceStream),
  ];
  stream = new MediaStream(tracks);

  // inform user when tracks get lost
  stream.getTracks().forEach((track) => {
    track.onended = () => {
      alert("VideoRecording: Track " + track.label + " has ended.");
      mainModule.uiState.transition("cancel");
    };
  });
}

async function getDevices() {
  // collect list of cameras and microphones
  try {
    const devices = await navigator.mediaDevices.enumerateDevices();
    devices.forEach((device) => {
      switch (device.kind) {
        case "audioinput": {
          const option = document.createElement("option");
          option.value = device.deviceId;
          option.text = device.label || `microphone ${micSelect.length + 1}`;
          UI.micSelect.add(option);
          break;
        }
        case "videoinput": {
          const option = document.createElement("option");
          option.value = device.deviceId;
          option.text = device.label || `camera ${camSelect.length + 1}`;
          UI.camSelect.add(option);
          break;
        }
      }
    });

    // select previously chosen camera
    UI.camSelect.selectedIndex = -1;
    const selectedCamera = localStorage.getItem("decker-camera");
    if (selectedCamera) {
      for (let i = 0; i < UI.camSelect.options.length; i++) {
        if (UI.camSelect.options[i].text == selectedCamera) {
          UI.camSelect.selectedIndex = i;
          break;
        }
      }
    }
    // select previously chosen microphone
    UI.micSelect.selectedIndex = -1;
    const selectedMicrophone = localStorage.getItem("decker-microphone");
    if (selectedMicrophone) {
      for (let i = 0; i < UI.micSelect.options.length; i++) {
        if (UI.micSelect.options[i].text == selectedMicrophone) {
          UI.micSelect.selectedIndex = i;
          break;
        }
      }
    }
  } catch (e) {
    console.log("cannot list microphones and cameras:" + e);
  }
}

// adapted from https://jameshfisher.com/2020/08/11/production-ready-green-screen-in-the-browser/
function setupGreenScreen() {
  const gl = UI.cameraCanvas.getContext("webgl", { premultipliedAlpha: false });

  const vsource = String.raw`attribute vec2 c; 
    void main(void) { 
      gl_Position=vec4(c, 0.0, 1.0); 
    }`;

  const fsource = String.raw`precision mediump float;
    uniform sampler2D tex;
    uniform float texWidth;
    uniform float texHeight;
    uniform vec3  keyColor;
    uniform float similarity;
    uniform float smoothness;
    uniform float spill;

    // From https://github.com/obsproject/obs-studio/blob/master/plugins/obs-filters/data/chroma_key_filter_v2.effect
    vec2 rgb2uv(vec3 rgb) {
      return vec2(
        0.501961 - 0.100644*rgb.r - 0.338572*rgb.g + 0.439216*rgb.b,
        0.501961 + 0.439216*rgb.r - 0.398942*rgb.g - 0.040274*rgb.b
      );
    }

    vec4 ProcessChromaKey(vec2 texCoord) {
      vec4 rgba = texture2D(tex, texCoord);
      float chromaDist = distance(rgb2uv(rgba.rgb), rgb2uv(keyColor));
      float baseMask = chromaDist - similarity;
      float fullMask = pow(clamp(baseMask / smoothness, 0., 1.), 1.5);
      rgba.a = fullMask;
      float spillVal = pow(clamp(baseMask / spill, 0., 1.), 1.5);
      float desat = clamp(rgba.r * 0.2126 + rgba.g * 0.7152 + rgba.b * 0.0722, 0., 1.);
      rgba.rgb = mix(vec3(desat, desat, desat), rgba.rgb, spillVal);
      return rgba;
    }

    void main(void) {
      vec2 texCoord = vec2(gl_FragCoord.x/texWidth, 1.0 - (gl_FragCoord.y/texHeight));
      gl_FragColor = ProcessChromaKey(texCoord);
    }`;

  const vs = gl.createShader(gl.VERTEX_SHADER);
  gl.shaderSource(vs, vsource);
  gl.compileShader(vs);

  const fs = gl.createShader(gl.FRAGMENT_SHADER);
  gl.shaderSource(fs, fsource);
  gl.compileShader(fs);

  const prog = gl.createProgram();
  gl.attachShader(prog, vs);
  gl.attachShader(prog, fs);
  gl.linkProgram(prog);
  gl.useProgram(prog);

  const vb = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vb);
  gl.bufferData(
    gl.ARRAY_BUFFER,
    new Float32Array([-1, 1, -1, -1, 1, -1, 1, 1]),
    gl.STATIC_DRAW
  );

  const coordLoc = gl.getAttribLocation(prog, "c");
  gl.vertexAttribPointer(coordLoc, 2, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(coordLoc);

  gl.activeTexture(gl.TEXTURE0);
  const tex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, tex);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

  const texLoc = gl.getUniformLocation(prog, "tex");
  const texWidthLoc = gl.getUniformLocation(prog, "texWidth");
  const texHeightLoc = gl.getUniformLocation(prog, "texHeight");
  const keyColorLoc = gl.getUniformLocation(prog, "keyColor");
  const similarityLoc = gl.getUniformLocation(prog, "similarity");
  const smoothnessLoc = gl.getUniformLocation(prog, "smoothness");
  const spillLoc = gl.getUniformLocation(prog, "spill");

  function processFrame(now, metadata) {
    if (cameraCanvas.width != metadata.width) {
      cameraCanvas.width = metadata.width;
      cameraCanvas.height = metadata.height;
    }
    gl.viewport(0, 0, metadata.width, metadata.height);
    gl.texImage2D(
      gl.TEXTURE_2D,
      0,
      gl.RGB,
      gl.RGB,
      gl.UNSIGNED_BYTE,
      cameraVideo
    );

    gl.uniform1i(texLoc, 0);
    gl.uniform1f(texWidthLoc, metadata.width);
    gl.uniform1f(texHeightLoc, metadata.height);
    gl.uniform3f(
      keyColorLoc,
      gsKey.r / 255.0,
      gsKey.g / 255.0,
      gsKey.b / 255.0
    );
    gl.uniform1f(similarityLoc, gsSimilarity);
    gl.uniform1f(smoothnessLoc, gsSmoothness);
    gl.uniform1f(spillLoc, gsSpill);

    gl.drawArrays(gl.TRIANGLE_FAN, 0, 4);
    cameraVideo.requestVideoFrameCallback(processFrame);
  }

  cameraVideo.requestVideoFrameCallback(processFrame);
}

export function toggleCamera() {
  if (mainModule.uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")) {
    if (!cameraStream) {
      window.showInformation(localization.no_camera_stream);
      return;
    }
    if (UI.cameraPanel.classList.toggle("visible")) {
      if (UI.cameraVideo.srcObject !== cameraStream) {
        UI.cameraVideo.srcObject = cameraStream;
      }
      UI.cameraVideo.play();
    } else {
      UI.cameraVideo.pause();
    }
  } else {
    UI.cameraPanel.classList.remove("visible");
  }
}

export function setGreenScreenConfig(config) {
  greenScreenConfig.useGreenScreen = config.useGreenScreen || false;
  greenScreenConfig.gsBackground = config.greenScreenBackground || undefined;
  greenScreenConfig.gsKey = config.greenScreenKey || { r: 0, g: 255, b: 0 };
  greenScreenConfig.gsSimilarity = config.greenScreenSimilarity || 0.4;
  greenScreenConfig.gsSmoothness = config.greenScreenSmoothness || 0.08;
  greenScreenConfig.gsSpill = config.greenScreenSpill || 0.1;
}

async function listRecordings(path) {
  try {
    const response = await fetch(`/recordings${path}`, { method: "GET" });
    return response.json();
  } catch (error) {
    console.log("[] cannot list recordings: " + path + ", " + e);
    return [];
  }
}

async function showExistingWarning(existingRecordings) {
  let options = [
    { text: localization.append, value: "APPEND" },
    { text: localization.replace, value: "REPLACE" },
    { text: localization.cancel, value: "CANCEL" },
  ];
  let messageElement = document.createElement("div");
  let messageText = document.createElement("p");
  messageText.innerText = localization.replacement_warning;
  messageElement.appendChild(messageText);
  let messageList = document.createElement("ul");
  for (const recording of existingRecordings) {
    let item = document.createElement("li");
    item.innerText = recording;
    messageList.appendChild(item);
  }
  messageElement.appendChild(messageList);
  let choice = await window.showDialog(
    localization.replacement_title,
    messageElement,
    options,
    "warning"
  );
  return choice.submit;
}

let streamSocket;

export async function startRecording() {
  // Stop Decker from auto reloading the page if we recorded something
  window.Decker.addReloadInhibitor(() => false);

  // clear blobs array
  blobs = [];

  let timesExists = await mainModule.resourceExists(urls.times);
  let existingRecordings = await listRecordings(urls.webm);
  let explainTimes;

  recordingType = "REPLACE";
  recordingResumeTime = 0;

  if (existingRecordings.length > 0 && timesExists) {
    const choice = await showExistingWarning(existingRecordings);
    if (choice === "APPEND") {
      explainTimes = await mainModule.fetchResourceJSON(urls.times);
      recordingType = "APPEND";
      recordingResumeTime = explainTimes[explainTimes.length - 1].timeOut;
    } else if (choice === "REPLACE") {
      recordingType = "REPLACE";
      recordingResumeTime = 0;
    } else {
      return false;
    }
  }

  UI.closeRecordPanel();

  // setup recorder (let the browser choose the codec)
  recorder = new MediaRecorder(stream, {
    // mimeType: "video/webm; codecs=h264",
    mimeType: "video/webm",
  });

  recorder.ondataavailable = (e) => {
    blobs.push(e.data);
    const livestream = Decker.meta.livestream;
    if (livestream && livestream.encoder) {
      streamSocket.send(e.data);
    }
  };

  let recordSlideChange = () => recorder.timing.record();

  recorder.onstart = () => {
    console.log("[] recorder started");
    const livestream = Decker.meta.livestream;
    if (livestream && livestream.encoder) {
      const endpoint = document.getElementById("rtmp-endpoint");
      const streamkey = document.getElementById("streamkey");
      streamSocket = new WebSocket(
        Decker.meta.livestream.encoder +
          "?endpoint=" +
          encodeURIComponent(endpoint.value + "/" + streamkey.value)
      );
    }
    recorder.timing = new Timing();
    if (recordingType === "APPEND") {
      recorder.timing.offset = parseFloat(recordingResumeTime) * 1000;
      recorder.timing.timeIntervals = explainTimes;
    }
    recorder.timing.start();
    Reveal.addEventListener("slidechanged", recordSlideChange);

    UI.updateRecordIndicator(recorder);
    recordingTimer = setInterval(updateRecordingTimer, 1000);
  };

  function updateRecordingTimer() {
    let seconds = recorder.timing.timeStamp();
    let time = new Date(null);
    time.setSeconds(seconds);
    UI.recordingTime.innerText = time.toISOString().substring(11, 19);
    recorder.requestData();
  }

  async function uploadFile(file) {
    console.log("[] about to upload: ", file);
    try {
      const response = await fetch(file.filename, {
        method: "PUT",
        body: file.data,
      });
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_success(file.filename)
        );
      }
      return response.ok;
    } catch (error) {
      console.error(
        "[] cannot upload data to:",
        file.filename,
        "reason:",
        error
      );
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_failed(file.filename)
        );
      }
      return false;
    }
  }

  async function replaceVideo(file) {
    console.log("[] about to upload (replace): ", file);
    let path = `/replace${file.filename}`;
    try {
      const response = await fetch(path, { method: "PUT", body: file.data });
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_success(file.filename)
        );
      }
      return response.ok;
    } catch (error) {
      console.error(
        "[] cannot upload (replace) video to:",
        path,
        "reason:",
        error
      );
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_failed(file.filename)
        );
      }
      return false;
    }
  }

  async function appendVideo(file) {
    console.log("[] about to upload (append): ", file);
    let path = `/append${file.filename}`;
    try {
      const response = await fetch(path, { method: "PUT", body: file.data });
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_success(file.filename)
        );
      }
      return response.ok;
    } catch (error) {
      console.error("[] cannot upload (append) video to:", path, "reason:", e);
      if (window.postNotification) {
        window.postNotification(
          localization.notification_title,
          localization.notification_upload_failed(file.filename)
        );
      }
      return false;
    }
  }

  recorder.onstop = async () => {
    console.log("[] recorder stopped");
    streamSocket.close();

    UI.recordIndicator.dataset.state = "saving";

    let vblob = new Blob(blobs, { type: "video/webm" });
    let tblob = recorder.timing.finish();

    let timesUpload;
    let videoUpload;

    mainModule.download(vblob, mainModule.deckname + "-recording.webm");
    mainModule.download(tblob, mainModule.deckname + "-times.json");
    try {
      let exists = await mainModule.resourceExists(urls.times);
      /* Upload slide timings */
      timesUpload = uploadFile({
        data: tblob,
        filename: urls.times,
      });
      if (exists && recordingType === "APPEND") {
        videoUpload = appendVideo({
          data: vblob,
          filename: urls.webm,
        });
      } else {
        videoUpload = replaceVideo({
          data: vblob,
          filename: urls.webm,
        });
      }
    } catch (e) {
      console.error(`[] FAILED to upload ${tblob.size} bytes to ${urls.times}`);
      console.error(`[] FAILED to upload ${vblob.size} bytes to ${urls.webm}`);
    }

    Reveal.removeEventListener("slidechanged", recordSlideChange);
    clearInterval(recordingTimer);

    recorder = null;
    stream = null;

    UI.closeRecordPanel();

    await Promise.all([timesUpload, videoUpload]);

    mainModule.uiState.transition("complete");
    UI.updateRecordIndicator(recorder);
  };

  recorder.onpause = () => {
    recorder.timing.pause();
    clearInterval(recordingTimer);
    UI.updateRecordIndicator(recorder);
  };

  recorder.onresume = () => {
    recorder.timing.resume();
    recordingTimer = setInterval(updateRecordingTimer, 1000);
    UI.updateRecordIndicator(recorder);
  };

  recorder.onerror = (evt) => {
    alert("VideoRecording Error: " + evt.name);
    mainModule.uiState.transition("cancel");
  };

  const dialog = UI.streamDialog;
  await dialog.modalPromise();
  recorder.start();
  UI.recordButton.disabled = true;
  UI.pauseButton.disabled = undefined;
  UI.stopButton.disabled = undefined;
  UI.micSelect.disabled = true;
  UI.camSelect.disabled = true;
  return true;
}

export function pauseRecording() {
  recorder.pause();
  UI.recordButton.disabled = true;
  UI.pauseButton.disabled = undefined;
  UI.stopButton.disabled = undefined;
  return true;
}

export function resumeRecording() {
  recorder.resume();
  UI.recordButton.disabled = true;
  UI.pauseButton.disabled = undefined;
  UI.stopButton.disabled = undefined;
  return true;
}

export function stopRecording() {
  recorder.stop();
  stream.getTracks().forEach((s) => s.stop());
  UI.recordButton.disabled = undefined;
  UI.pauseButton.disabled = true;
  UI.stopButton.disabled = true;
  UI.micSelect.disabled = undefined;
  UI.camSelect.disabled = undefined;

  // for dummy users: also save whiteboard annotations
  if (Reveal.hasPlugin("whiteboard")) {
    Reveal.getPlugin("whiteboard").saveAnnotations();
  }

  return true;
}

export function completeRecording() {
  console.log("[] recording completed");
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
  let audioMeter = createAudioMeter(context, UI.volumeMeter);

  if (desktopStream && desktopStream.getAudioTracks().length > 0) {
    hasDesktop = true;
    // connect gain to slider
    desktopGain = context.createGain();
    desktopGain.gain.value = UI.desktopGainSlider.value;
    UI.desktopGainSlider.gain = desktopGain;
    // connect source->gain->meter
    const source1 = context.createMediaStreamSource(desktopStream);
    source1.connect(desktopGain).connect(audioMeter);
  }

  if (voiceStream && voiceStream.getAudioTracks().length > 0) {
    hasVoice = true;
    // connect gain to slider
    voiceGain = context.createGain();
    voiceGain.gain.value = UI.voiceGainSlider.value;
    UI.voiceGainSlider.gain = voiceGain;
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

export async function setupRecorder() {
  try {
    stream = null;

    // if we call this the first time, collect cameras and microphones
    if (UI.camSelect.childElementCount + UI.micSelect.childElementCount == 0) {
      await getDevices();
    }

    // capture video/audio stream of desktop signal
    await captureScreen();

    // capture audio stream of microphone
    await captureMicrophone();

    // merge desktop and microphone streams into one stream to be recorded
    mergeStreams();

    // setup shaders for greenscreen (has to be done before captureCamera())
    if (greenScreenConfig.useGreenScreen) {
      setupGreenScreen();
    }

    // capture video stream of webcam
    await captureCamera();

    UI.recordButton.disabled = undefined;
    UI.pauseButton.disabled = true;
    UI.stopButton.disabled = true;

    // open panel to select camera and mic
    UI.openRecordPanel();

    return true;
  } catch (e) {
    console.error(e);
    alert(`Recording setup failed.\n${e.message}`);
    return false;
  }
}
