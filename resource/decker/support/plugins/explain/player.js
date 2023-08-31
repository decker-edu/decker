import * as mainModule from "./explain.js";
import { urls, Reveal } from "./explain.js";

export let player, playPanel, playButton, video;

let explainTimesPlay;

// Navigates Reveal to the indexed slide in the explainTimes array.
function goToSlide(index) {
  if (explainTimesPlay[index]) {
    let slideId = explainTimesPlay[index].slideId;
    let indices = Reveal.getIndices(document.getElementById(slideId));
    Reveal.slide(indices.h, indices.v);
  }
}

export function goToSlideId(slideId) {
  let indices = Reveal.getIndices(document.getElementById(slideId));
  Reveal.slide(indices.h, indices.v);
}

// Jumps the video tp the in-time timestamp stored at index.
function jumpToTime(index) {
  if (explainTimesPlay[index]) {
    player.currentTime(explainTimesPlay[index].timeIn);
  }
}

// Looks up the index of the current Reveal slide in the explainTimes array.
function currentRevealSlideIndex() {
  let slideId = Reveal.getCurrentSlide().id;
  return explainTimesPlay.findIndex((i) => i.slideId === slideId);
}

// Looks up the index of the current slide in the video.
function currentVideoSlideIndex() {
  let time = player.currentTime();
  return explainTimesPlay.findIndex(
    (i) => i.timeIn <= time && time <= i.timeOut
  );
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
export function stop() {
  player.pause();
  goToSlide(currentVideoSlideIndex());
  return true;
}

// Starts the video at the current Reveal slide.
export function play() {
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

export async function setupPlayer() {
  const config = Decker.meta.explain;
  urls.video = config && config.video ? config.video : urls.video;
  urls.times = config && config.times ? config.times : urls.times;
  urls.captions =
    config && config.transcript ? config.transcript : urls.captions;
  let videoExists = false;
  let timesExists = false;

  try {
    // if in electron app and user specified base url for videos:
    // if times exist locally, we assume the video exists on remote server
    if (window.electronApp && config && config.electronVideoUrl) {
      urls.video =
        config.electronVideoUrl + mainModule.deckname + "-recording.mp4";
      videoExists = true;
      timesExists = await mainModule.resourceExists(urls.times);
    }
    // in browser: check if video and times exist
    else {
      videoExists = await mainModule.resourceExists(urls.video);
      timesExists = await mainModule.resourceExists(urls.times);
    }

    if (videoExists && timesExists) {
      explainTimesPlay = await mainModule.fetchResourceJSON(urls.times);
      player.src({ type: "video/mp4", src: urls.video });

      let captionsExist = await mainModule.resourceExists(urls.captions);
      if (captionsExist) {
        let captionsOptions = {
          kind: "captions",
          srclang: document.documentElement.lang,
          src: urls.captions,
        };
        player.addRemoteTextTrack(captionsOptions, false);
      }
      return true;
    } else {
      console.log("[] play: no video available");
      return false;
    }
  } catch (_) {
    return false;
  }
}

export function createPlayerGUI() {
  playPanel = mainModule.createElement({
    type: "div",
    id: "explain-panel",
    parent: document.body,
  });

  playButton = mainModule.createElement({
    type: "button",
    id: "explain-play",
    classes: "explain fa-button fas fa-play",
    title: "Play video recording",
    onclick: mainModule.transition("play"),
  });

  if (mainModule.Reveal.hasPlugin("ui-anchors")) {
    mainModule.Reveal.getPlugin("ui-anchors").placeButton(
      playButton,
      "TOP_RIGHT"
    );
  } else {
    document.body.appendChild(playButton);
  }

  let video = mainModule.createElement({
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
    playbackRates: [0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 3],
    controlBar: {
      playToggle: true,
      volumePanel: true,
      currentTimeDisplay: true,
      timeDivider: false,
      durationDisplay: false,
      remainingTimeDisplay: true,
      playbackRateMenuButton: true,
      fullscreenToggle: true,
      pictureInPictureToggle: false,
    },
    userActions: {
      // disable going to fullscreen by double click
      doubleClick: false,
      // our keyboard shortcuts
      hotkeys: function (event) {
        event.stopPropagation();
        event.preventDefault();

        switch (event.code) {
          // space or k: play/pause
          case "Space":
          case "KeyK":
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

          // up/down: increase/decrease volume by 5%
          case "ArrowUp":
            this.volume(Math.min(1.0, this.volume() + 0.05));
            break;
          case "ArrowDown":
            this.volume(Math.max(0.0, this.volume() - 0.05));
            break;

          // c: toggle captions
          case "KeyC":
            let tracks = player.textTracks();
            for (let i = 0; i < tracks.length; i++) {
              if (tracks[i].kind === "captions") {
                tracks[i].mode =
                  tracks[i].mode === "showing" ? "disabled" : "showing";
              }
            }
            break;

          // j/l: jump backward/forward by 10sec
          case "KeyJ":
            player.currentTime(player.currentTime() - 10);
            break;
          case "KeyL":
            player.currentTime(player.currentTime() + 10);
            break;

          // m: mute/unmute
          case "KeyM":
            this.muted(!this.muted());
            break;

          // esc: stop and hide video
          case "Escape":
            mainModule.uiState.transition("stop");
            break;
        }
      },
    },
  });
  // use double tap on left/right part of player to jump backward/forward by 10sec
  let lastTap = null;
  player.on("touchstart", (evt) => {
    const now = Date.now();
    if (lastTap && now - lastTap < 300) {
      // we have a double tap
      const tapX = evt.touches[0].clientX;
      const playerWidth = player.el().clientWidth;
      if (tapX > 0.66 * playerWidth) {
        // right border: skip forward
        player.currentTime(player.currentTime() + 10);
      } else if (tapX < 0.33 * playerWidth) {
        // left border: skip backward
        player.currentTime(player.currentTime() - 10);
      } else {
        // center: play/pause
        if (player.paused()) player.play();
        else player.pause();
      }
    }
    lastTap = now;
  });

  player.on("ended", mainModule.transition("stop"));

  player.on("error", (_) => {
    console.error('ExplainPlugin: Could not open video "' + urls.video + '"');
    mainModule.uiState.transition("stop");
  });

  player.getChild("ControlBar").addChild(
    "button",
    {
      controlText: "Close video",
      className: "vjs-icon-cancel",
      clickHandler: function () {
        uiState.transition("stop");
      },
    },
    0
  );

  player.getChild("ControlBar").addChild(
    "button",
    {
      controlText: "Jump to previous slide",
      className: "vjs-icon-previous-item",
      clickHandler: function () {
        prev();
      },
    },
    1
  );

  player.getChild("ControlBar").addChild(
    "button",
    {
      controlText: "Jump to next slide",
      className: "vjs-icon-next-item",
      clickHandler: function () {
        next();
      },
    },
    3
  );
}
