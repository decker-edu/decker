"use strict";


let ExplainPlugin = (function(){

  // get config
  let config = Reveal.getConfig().explain;
  let vurl, times;
  if (config) {
    if (config.video) {
      vurl = config.video;
      if (vurl.endsWith("/")) {
        let filename = location.pathname;
        filename = filename.substring(0, filename.lastIndexOf("."));
        filename = filename + ".mp4";
        vurl += filename;
      }
    }
    times = config.times ? JSON.parse(config.times) : [0];
  }
  // console.log(vurl);
  // console.log(times);


  // GUI elements
  let playButton, stopButton, nextButton, prevButton;
  let panel, video, playbackRate;


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


	return {
		init: function() { 
      // need video and times
      if (vurl && times) {

        playButton = document.createElement("button");
        playButton.id = "dvo-play";
        playButton.classList.add("dvo-button", "fas", "fa-play");
        playButton.addEventListener("click", play);
        playButton.title = "Play video recording";

        panel = document.createElement("div");
        panel.id = "dvo-panel";

        playbackRate = document.createElement("input");
        playbackRate.id = "dvo-rate";
        playbackRate.title = "Adjust playback rate";
        playbackRate.type = "number";
        playbackRate.min  = 0.25;
        playbackRate.max  = 2.0;
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

        let controls = document.createElement( 'div' );
        controls.id = 'dvo-controls';
        controls.appendChild(playbackRate);
        controls.appendChild(prevButton);
        controls.appendChild(nextButton);
        controls.appendChild(stopButton);

        video = document.createElement("video");
        video.setAttribute("id", "dvo-video");
        video.setAttribute("controls", "1");
        video.setAttribute("controlsList", "nofullscreen nodownload");
        video.setAttribute("preload", "1");
        video.setAttribute("src", vurl);
        video.addEventListener('keydown', evt => {
          evt.stopPropagation();
          // ESC
          if (evt.keyCode == 27) {
            stopVideo();
          }
          else if (evt.key == 't') {
            times.push(Math.floor(video.currentTime));
            console.log(times);
          }
          else if (evt.key == 'T') {
            times = [ 0 ];
            console.log(times);
          }
        });
        video.addEventListener('error', (evt) => {
          console.error("ExplainPlugin: Could not open video \""+vurl+"\"");
          playButton.style.visibility = 'hidden';
        });

        panel.appendChild(controls);
        panel.appendChild(video);
        document.body.appendChild(panel);
        document.body.appendChild(playButton);
      }
      else {
        console.error("ExplainPlugin needs video and time stamps");
      }
    }
  };

})();

Reveal.registerPlugin( 'explain', ExplainPlugin );
