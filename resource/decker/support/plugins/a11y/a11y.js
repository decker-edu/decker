import {
  setupFlyingFocus,
  hideFlyingFocus,
} from "../../flyingFocus/flying-focus.js";

import { modifyMedia, restoreMedia } from "../../js/media-a11y.js";

let Reveal;

let a11yMode;

let pluginButton = undefined;

function addScreenReaderSlideNumbers() {
  const slides = document.querySelectorAll(".slides > section");
  slides.forEach((slide, h) => {
    const subslides = slide.querySelectorAll("section");
    if (subslides.length > 0) {
      subslides.forEach((subslide, v) => {
        addScreenReaderSlideNumber(subslide, h, v);
      });
      return;
    }
    addScreenReaderSlideNumber(slide, h);
  });
}

function addScreenReaderSlideNumber(slide, h, v) {
  const header = slide.querySelector("h1");
  if (header && header.textContent.trim() !== "") {
    const innerHTML = header.innerHTML;
    const replacementHTML = `<span class="sr-only">${localization.slide} ${
      h + 1
    }${v ? "." + v : ""}, </span>${innerHTML}`;
    header.innerHTML = replacementHTML;
  }
}

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

function toggleAccessibility() {
  a11yMode = !a11yMode;

  if (a11yMode) {
    pluginButton.ariaPressed = true;
    pluginButton.setLabel(localization.deactivate_accessibility);
    document.documentElement.classList.add("a11y");
    const videos = document.getElementsByTagName("VIDEO");
    for (const video of videos) {
      modifyMedia(video);
    }
    const audios = document.getElementsByTagName("AUDIO");
    for (const audio of audios) {
      modifyMedia(audio);
    }
    Decker.flash.message(localization.accessible_colors_on);
    if (window.MathJax) {
      window.MathJax.startup.document.options.enableMenu = true;
      window.MathJax.startup.document.menu.menu
        .findID("Accessibility", "Activate")
        .variable.setter(true);
      window.MathJax.startup.document.menu.loadingPromise.then(() => {
        window.MathJax.startup.document.rerender();
      });
    }
  } else {
    pluginButton.ariaPressed = false;
    pluginButton.setLabel(localization.activate_accessibility);
    document.documentElement.classList.remove("a11y");
    const videos = document.getElementsByTagName("VIDEO");
    for (const video of videos) {
      restoreMedia(video);
    }
    const audios = document.getElementsByTagName("AUDIO");
    for (const audio of audios) {
      restoreMedia(audio);
    }
    Decker.flash.message(localization.accessible_colors_off);
    if (window.MathJax) {
      // Does it make sense to remove this again if once activated?
      window.MathJax.startup.document.options.enableMenu = false;
      window.MathJax.startup.document.menu.menu
        .findID("Accessibility", "Activate")
        .variable.setter(false);
      window.MathJax.startup.document.menu.loadingPromise.then(() => {
        window.MathJax.startup.document.rerender();
      });
    }
  }
}

const localization = {
  activate_accessibility: "Activate Accessibility Features (A,A,A)",
  deactivate_accessibility: "Deactivate Accessibility Features (A,A,A)",
  accessible_colors_on: `<span>Accessible Colors: <strong style="color:var(--accent3);">ON</strong></span>`,
  accessible_colors_off: `<span>Accessible Colors: <strong style="color:var(--accent1);">OFF</strong></span>`,
  slide: "Slide",
};

if (navigator.language === "de") {
  localization.activate_accessibility =
    "Barrierefreie Funktionen anschalten (A,A,A)";
  localization.deactivate_accessibility =
    "Barrierefreie Funktionen abschalten (A,A,A)";
  localization.accessible_colors_on = `<span>Kontrastfarben: <strong style="color:var(--accent3);">AN</strong></span>`;
  localization.accessible_colors_off = `<span>Kontrastfarben: <strong style="color:var(--accent1);">AUS</strong></span>`;
  localization.slide = "Folie";
}

const a11y = /a11y/gi.test(window.location.search);

const Plugin = {
  id: "a11y",
  init: (reveal) => {
    Reveal = reveal;
    fixTabsByInert();
    addFlyingFocusCallbacks();
    addCustomSpacebarHandler();
    addScreenReaderSlideNumbers();
    reveal.addKeyBinding(
      {
        keyCode: 65,
        key: "A",
        description: "Toggle Decker Accessibility Adjustments (Triple Click)",
      },

      Decker.tripleClick(toggleAccessibility)
    );
    reveal.addEventListener("ready", () => {
      const menuPlugin = reveal.getPlugin("decker-menu");
      if (!!menuPlugin && !!menuPlugin.addPluginButton) {
        pluginButton = menuPlugin.addPluginButton(
          "decker-menu-a11y-button",
          "fa-universal-access",
          localization.activate_accessibility,
          toggleAccessibility
        );
      }
    });
    if (a11y) {
      Reveal.addEventListener("ready", () => {
        toggleAccessibility();
      });
    }
  },
};

export default Plugin;
