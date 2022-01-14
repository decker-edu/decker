import { hideFlyingFocus } from "../../flyingFocus/flying-focus.js";

let Reveal;

/**
 * Adds inert to all inactive slides and adds an on-slidechanged callback to reveal
 * to toggle inert on the slides changed.
 * TODO: Figure out if we want to have this instead of a viewDistance of 1.
 * This might cause performance issues on more complex DOM trees but they have yet to be observed.
 * As such this function is - for now - just "here" for future reference.
 */
 function fixTabsByInert() {
  let slides = document.querySelectorAll("section");
  slides.forEach((slide) => {
    if(slide.hasAttribute("hidden")) {
      slide.inert = true;
    }
  })
  Reveal.on('slidechanged', event => {
    event.previousSlide.inert = true;
    event.currentSlide.inert = false;
  } );
}

/* SUGGESTION Add Flying Focus here instead of deck.html ? */
function addFlyingFocusCallbacks() {
  Reveal.on("slidechanged", event => {
    hideFlyingFocus();
  })
}

const Plugin = {
  id: "a11y",
  init: (reveal) => {
    Reveal = reveal;
    fixTabsByInert();
    addFlyingFocusCallbacks();
  }
};

export default Plugin;