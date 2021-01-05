"use strict";


let ExplainPlugin = (function(){

  // get config
  let config = Reveal.getConfig().explain;
  let vurl, times;
  if (config) {
    if (config.video) {
      vurl = config.video;
      if (vurl.endsWith("/")) {
        vurl += videoFilename();
      }
    }
    times = config.times ? JSON.parse(config.times) : [0];
  }


  // GUI elements
  let recordButton, playButton, stopButton, nextButton, prevButton;
  let panel, video, playbackRate;

  // recording stuff
  let blobs;
  let blob;
  let rec;
  let stream;
  let voiceStream;
  let desktopStream;
  let recording = false;


  function videoFilenameBase() {
    let filename = location.pathname;
    filename = filename.substring(1, filename.lastIndexOf("."));
    return filename;
  }
  function videoFilename() {
    return videoFilenameBase() + '.mp4';
  }


  function currentSlide() {
    let time = video.currentTime;
    let slide = 0;
    while (slide < times.length && time > times[slide])
      slide += 1;
    return slide-1;
  }


  function next() {
    let slide = currentSlide() + 1;
    if (times[slide]) video.currentTime = times[slide];
  }


  function prev() {
    let slide = currentSlide() - 1;
    if (times[slide]) video.currentTime = times[slide];
  }


  function stop() {
    panel.removeAttribute("data-visible");
    video.pause();
    let slide = currentSlide();
    Reveal.slide(slide);
  }


  function play() {
    panel.setAttribute("data-visible", 1);

    let ended = () => {
      video.removeEventListener("ended", ended);
      stop();
    };

    let time = video.currentTime;
    let slide = Reveal.getState().indexh;
    if (currentSlide(time) != slide) {
      if (times[slide])
        video.currentTime = times[slide];
      else
        video.currentTime = 0;
    }

    video.addEventListener("ended", ended);
    video.play();
    video.focus();
  }


  function record() {
    if (!recording) {
      startRecording();
      recording = true;
    }
    else {
      stopRecording();
      recording = false;
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
   
    // get microphone stream
    console.log('get voice stream');
    voiceStream = await navigator.mediaDevices.getUserMedia({
      video: false,
      audio: {
        echoCancellation: true,
        noiseSuppression: true
      }
    });
  
    // merge tracks into recording stream
    const tracks = [
      ...desktopStream.getVideoTracks(), 
      ...mergeAudioStreams(desktopStream, voiceStream)
    ];
    stream = new MediaStream(tracks);

    // clear blobs array
    blobs = [];
  
    // setup recorder
    rec = new MediaRecorder(stream, {mimeType: 'video/webm; codecs=h264"'});
    rec.ondataavailable = (e) => blobs.push(e.data);
    rec.onstop = async () => {
      blob = new Blob(blobs, {type: 'video/webm'});
      let url = window.URL.createObjectURL(blob);
      let download = document.createElement('a');
      document.body.appendChild(download);
      download.href = url;
      download.download = videoFilenameBase() + '.webm';
      download.click();
      document.body.removeChild(download);
    };

    // show record button
    recordButton.style.display = 'block';
  }


  function startRecording() {
    recordButton.title = "Stop video recording";
    recordButton.classList.remove("fa-circle");
    recordButton.classList.add("fa-stop");

    rec.start();
  }


  function stopRecording() {
    recordButton.title = "Start video recording";
    recordButton.classList.remove("fa-stop");
    recordButton.classList.add("fa-circle");

    rec.stop();
    stream.getTracks().forEach(s => s.stop())
    stream = null;
  }


  const mergeAudioStreams = (desktopStream, voiceStream) => {
    const context = new AudioContext();
    const destination = context.createMediaStreamDestination();
    let hasDesktop = false;
    let hasVoice = false;
    if (desktopStream && desktopStream.getAudioTracks().length > 0) {
      const source1 = context.createMediaStreamSource(desktopStream);
      const desktopGain = context.createGain();
      desktopGain.gain.value = 0.7;
      source1.connect(desktopGain).connect(destination);
      hasDesktop = true;
      console.log("recording desktop audio");
    }
    
    if (voiceStream && voiceStream.getAudioTracks().length > 0) {
      const source2 = context.createMediaStreamSource(voiceStream);
      const voiceGain = context.createGain();
      voiceGain.gain.value = 1.0;
      source2.connect(voiceGain).connect(destination);
      hasVoice = true;
      console.log("recording mic audio");
    }
     
    return (hasDesktop || hasVoice) ? destination.stream.getAudioTracks() : [];
  };



  function printTimeStamps() {
    for (let i = 0; i < times.length; i++) {
      let t = times[i];
      let h = Math.floor(t / 60 / 60);
      let m = Math.floor(t / 60) - (h * 60);
      let s = t % 60;
      let formatted = h.toString().padStart(2, '0') + ':' + m.toString().padStart(2, '0') + ':' + s.toString().padStart(2, '0');
      console.log("slide " + (i+1) + ": time " + formatted + " = " + t + "s");
    } 
    console.log(times);
  }


  // setup key binding
  Reveal.addKeyBinding( { keyCode: 82, key: 'R', description: 'Setup Recording' }, setupRecording );


	return {
		init: function() { 

      recordButton = document.createElement("button");
      recordButton.id = "dvo-record";
      recordButton.classList.add("dvo-button", "fas", "fa-circle");
      recordButton.title = "Start video recording";
      recordButton.addEventListener("click", record);
      document.body.appendChild(recordButton);
      recordButton.style.display = 'none';

      playButton = document.createElement("button");
      playButton.id = "dvo-play";
      playButton.classList.add("dvo-button", "fas", "fa-play");
      playButton.addEventListener("click", play);
      playButton.title = "Play video recording";
      document.body.appendChild(playButton);

      panel = document.createElement("div");
      panel.id = "dvo-panel";
      document.body.appendChild(panel);

      playbackRate = document.createElement("input");
      playbackRate.id = "dvo-rate";
      playbackRate.title = "Adjust playback rate";
      playbackRate.type = "number";
      playbackRate.min = 0.25;
      playbackRate.max = 2.0;
      playbackRate.step = 0.25;
      playbackRate.value = 1.0;
      playbackRate.addEventListener("input", (evt) => {
        video.playbackRate = evt.target.value;
      });

      prevButton = document.createElement("button");
      prevButton.id = "dvo-prev";
      prevButton.classList.add("dvo-button", "fas", "fa-step-backward");
      prevButton.addEventListener("click", prev);
      prevButton.title = "Jump to previous slide";

      nextButton = document.createElement("button");
      nextButton.id = "dvo-next";
      nextButton.classList.add("dvo-button", "fas", "fa-step-forward");
      nextButton.addEventListener("click", next);
      nextButton.title = "Jump to next slide";

      stopButton = document.createElement("button");
      stopButton.id = "dvo-stop";
      stopButton.classList.add("dvo-button", "fas", "fa-stop");
      stopButton.addEventListener("click", stop);
      stopButton.title = "Stop video";

      let controls = document.createElement('div');
      controls.id = 'dvo-controls';
      controls.appendChild(playbackRate);
      controls.appendChild(prevButton);
      controls.appendChild(nextButton);
      controls.appendChild(stopButton);
      panel.appendChild(controls);

      video = document.createElement("video");
      video.setAttribute("id", "dvo-video");
      video.setAttribute("controls", "1");
      video.setAttribute("controlsList", "nofullscreen nodownload");
      video.setAttribute("preload", "1");
      video.addEventListener('keydown', evt => {
        evt.stopPropagation();
        // ESC
        if (evt.keyCode == 27) {
          stopVideo();
        }
        else if (evt.key == 't') {
          times.push(Math.floor(video.currentTime));
          printTimeStamps();
        }
      });
      video.addEventListener('error', (evt) => {
        console.error("ExplainPlugin: Could not open video \"" + vurl + "\"");
        playButton.style.visibility = 'hidden';
      });
      panel.appendChild(video);


      // if we have a video, use it
      if (vurl && times) {
        video.setAttribute("src", vurl);
        recordButton.style.display = 'none';
      }
      // otherwise let's record one
      else {
        playButton.style.display = 'none';
      }
    }
  };

})();

Reveal.registerPlugin( 'explain', ExplainPlugin );
