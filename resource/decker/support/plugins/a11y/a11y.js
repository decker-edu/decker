import {
  setupFlyingFocus,
  hideFlyingFocus,
} from "../../flyingFocus/flying-focus.js";

import { modifyMedia, restoreMedia } from "../../js/media-a11y.js";

let Reveal;

let a11yMode;

/**
 * Adds inert to all inactive slides and adds an on-slidechanged callback to reveal
 * to toggle inert on the slides changed.
 * This might cause performance issues on more complex DOM trees but they have yet to be observed.
 * As such this function is - for now - just "here" for future reference.
 */
function fixTabsByInert() {
  let slides = document.querySelectorAll("section");
  slides.forEach((slide) => {
    slide.inert = true;
  });
  Reveal.on("ready", (event) => {
    if (event && event.currentSlide) {
      event.currentSlide.inert = false;
    }
  });
  Reveal.on("slidechanged", (event) => {
    if (event.previousSlide) {
      // First shown slide has no previous slide and causes an error if we do not check for this
      let parent = event.previousSlide.parentElement;
      if (parent.classList.contains("stack")) {
        // Check if we were part of a slide stack and make the stack inert when we leave it.
        parent.inert = true;
      }
      event.previousSlide.inert = true;
    }
    let parent = event.currentSlide.parentElement;
    if (parent.classList.contains("stack")) {
      // Check if we are part of a slide stack and remove inert from the stack so this is not inert as part
      // of its subtree.
      parent.inert = false;
    }
    event.currentSlide.inert = false;
  });
}

/* setup flying focus and its callbacks */
function addFlyingFocusCallbacks() {
  setupFlyingFocus();
  Reveal.on("slidechanged", (event) => {
    hideFlyingFocus();
  });
}

let previousKeyboardConfig;

function addCustomSpacebarHandler() {
  const selects = document.getElementsByTagName("SELECT");
  for (const select of selects) {
    select.addEventListener("focus", (event) => {
      previousKeyboardConfig = Reveal.getConfig().keyboard;
      Reveal.configure({ keyboard: false });
    });
    select.addEventListener("blur", (event) => {
      Reveal.configure({ keyboard: previousKeyboardConfig });
    });
  }
}

const Plugin = {
  id: "a11y",
  init: (reveal) => {
    Reveal = reveal;
    // This may no longer be necessary if we clearly recommend the handout mode to people using assistive technology
    // fixTabsByInert();
    addFlyingFocusCallbacks();
    addCustomSpacebarHandler();
    reveal.addKeyBinding(
      {
        keyCode: 65,
        key: "A",
        description: "Toggle Decker Accessibility Adjustments (Triple Click)",
      },

      Decker.tripleClick(() => {
        a11yMode = !a11yMode;

        if (a11yMode) {
          document.documentElement.classList.add("a11y");
          const videos = document.getElementsByTagName("VIDEO");
          for (const video of videos) {
            modifyMedia(video);
          }
          const audios = document.getElementsByTagName("AUDIO");
          for (const audio of audios) {
            modifyMedia(audio);
          }
          Decker.flash.message(
            `<span>Accessible Colors: <strong style="color:var(--accent3);">ON</strong></span>`
          );
        } else {
          document.documentElement.classList.remove("a11y");
          const videos = document.getElementsByTagName("VIDEO");
          for (const video of videos) {
            restoreMedia(video);
          }
          const audios = document.getElementsByTagName("AUDIO");
          for (const audio of audios) {
            restoreMedia(audio);
          }
          Decker.flash.message(
            `<span>Accessible Colors: <strong style="color:var(--accent1);">OFF</strong></span>`
          );
        }
      })
    );
  },
};

export default Plugin;
