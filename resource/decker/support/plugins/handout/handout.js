import { modifyMedia, restoreMedia } from "../../js/media-a11y.js";

let Reveal;
let centralSlide;
let handoutSlideMode = false;

const previousRevealConfiguration = {
  controls: undefined,
  progress: undefined,
  fragments: undefined,
  slideNumber: undefined,
  disableLayout: undefined,
};

let visibleSlideIntersectionObserver = undefined;
let srcIntersectionObserver = undefined;
let visibleSlides = new Set();

/**
 * The slide scale should represent the ratio between reveal's
 * "canvas size" and the window's actual size. It is used to scale up the slides
 * in handout mode to be (almost) fullscreen width on default zoom factor.
 * This value should be constant but it is currently impossible to get the window's actual
 * on screen size to allow you to calculate this value as a constant.
 * In a window with a screen width of 1920 and a reveal width of 1280
 * this value should always be 1.5, regardless of internal zoom level.
 * Sadly, zooming in and out on desktop changes all retrievable values:
 * window.innerWidth, window.outerWidth, even the window.screen API AND
 * the new visualViewport API only reports the new, but no "original" values
 * AND does not report a proper scale factor (it is always 1 on desktop as it
 * is supposed to track pinch-zoom events).
 *
 * This value can (or rather should) not be pre-calculated as
 * the user might enter the page already zoomed in, so the initial
 * value is not trustworthy.
 *
 * You could calculate the value back to its original form by
 * multiplying slide and viewport widths with the devicePixelRatio.
 *
 * This does not work on Apple devices though as Safari always reports
 * a devicePixelRatio of '2' on Retina displays regardless of the
 * actual zoom factor. This is reported as a "bug" on WebKit's
 * issue tracker and has been ignored for the past 8 years:
 *
 * https://bugs.webkit.org/show_bug.cgi?id=124862
 *
 * Apple/Safari seem to be the only browsers to not scale
 * the pixel device ratio based on the user's zoom level.
 *
 * The fact that there is no API or system to detect the user's
 * zoom level across all devices seems to have been an issue for
 * at least 12 years.
 * https://css-tricks.com/can-javascript-detect-the-browsers-zoom-level/
 */
let slideScale = 1;
let userScale = 1;

const handoutContainer = document.createElement("div");
const handoutSlides = document.createElement("div");
handoutContainer.appendChild(handoutSlides);

let storedMetaViewport = undefined;

let pluginButton = undefined;

let localization = {
  activate_handout_mode: "Activate Handout Mode (H,H,H)",
  deactivate_handout_mode: "Deactivate Handout Mode (H,H,H)",
  handout_mode_on: `<span>Handout Mode: <strong style="color:var(--accent3);">ON</strong></span>`,
  handout_mode_off: `<span>Handout Mode: <strong style="color:var(--accent1);">OFF</strong></span>`,
  comment_header: "Questions and Comments",
};

if (navigator.language === "de") {
  localization.activate_handout_mode = "Handout-Modus anschalten (H,H,H)";
  localization.deactivate_handout_mode = "Handout-Modus abschalten (H,H,H)";
  localization.handout_mode_on = `<span>Handout-Modus: <strong style="color:var(--accent3);">AN</strong></span>`;
  localization.handout_mode_off = `<span>Handout-Modus: <strong style="color:var(--accent1);">AUS</strong></span>`;
  localization.comment_header = "Fragen und Kommentare";
}

/* clicking on a slide will set it to the current slide */
handoutSlides.addEventListener(
  "click",
  (evt) => {
    const target = evt.target;
    if (target) {
      const slide = target.closest("section");
      if (slide && slide != centralSlide) {
        setCurrentSlide(slide);
      }
    }
  },
  true
);

function activateHandoutMode() {
  /* Store and modify viewport meta tag to allow mobile device zooming */
  const meta = document.querySelector("meta[name=viewport]");
  if (meta) {
    storedMetaViewport = meta.getAttribute("content");
    const scalable = storedMetaViewport.replace(
      /user-scalable=no/,
      "user-scalable=yes"
    );
    const unlimited = scalable.replace(
      /\s*maximum-scale=([0-9]|\.)*\s*,?\s*/,
      " "
    );
    meta.setAttribute("content", unlimited);
  }
  const currentSlide = Reveal.getCurrentSlide();
  const allSlides = Reveal.getSlides();

  // Switch state of view menu button
  if (pluginButton) {
    pluginButton.setLabel(localization.deactivate_handout_mode);
  }

  // Store current reveal config and disable everything but keyboard shortcuts
  const currentConfiguration = Reveal.getConfig();
  previousRevealConfiguration.controls = currentConfiguration.controls;
  previousRevealConfiguration.progress = currentConfiguration.progress;
  previousRevealConfiguration.fragments = currentConfiguration.fragments;
  previousRevealConfiguration.slideNumber = currentConfiguration.slideNumber;
  previousRevealConfiguration.disableLayout =
    currentConfiguration.disableLayout;
  previousRevealConfiguration.keyboard = currentConfiguration.keyboard;
  Reveal.configure({
    controls: false,
    progress: false,
    fragments: false,
    slideNumber: false,
    disableLayout: true,
    keyboard: false,
  });

  // add class to root to enable special rules from handout.css
  document.documentElement.classList.add("handout");
  handoutSlideMode = true;

  // setup classes for fake containers
  handoutContainer.id = "handout-container";
  handoutContainer.classList.add("reveal");
  handoutSlides.classList.add("slides");
  const revealElem = Reveal.getRevealElement();
  const slidesElement = Reveal.getSlidesElement();
  makeSlidesVisible(slidesElement);
  const topLevelSections = slidesElement.querySelectorAll(":scope > section");
  storeIndices(topLevelSections);

  // setup background images
  slidesElement
    .querySelectorAll("section[data-background-image]")
    .forEach((section) => {
      const div = document.createElement("div");
      div.classList.add("handoutBackground");
      const url = section.getAttribute("data-background-image");
      div.style.backgroundImage = `url("${url}")`;
      const size = section.getAttribute("data-background-size");
      if (size) div.style.backgroundSize = size;
      const position = section.getAttribute("data-background-position");
      if (position) div.style.backgroundPosition = position;
      const repeat = section.getAttribute("data-background-repeat");
      if (repeat) div.style.backgroundRepeat = repeat;
      section.appendChild(div);
    });

  // setup background videos
  slidesElement
    .querySelectorAll("section[data-background-video]")
    .forEach((section) => {
      const video = document.createElement("video");
      video.classList.add("handoutBackground");
      video.dataset.src = section.getAttribute("data-background-video");
      const loop = section.getAttribute("data-background-video-loop");
      if (loop) video.loop = true;
      const muted = section.getAttribute("data-background-video-muted");
      if (muted) video.muted = true;
      section.appendChild(video);
    });

  // setup background iframes
  slidesElement
    .querySelectorAll("section[data-background-iframe]")
    .forEach((section) => {
      const iframe = document.createElement("iframe");
      iframe.classList.add("handoutBackground");
      iframe.dataset.src = section.getAttribute("data-background-iframe");
      section.appendChild(iframe);
    });

  /* Switch controls on and autoplay off */
  slidesElement
    .querySelectorAll("audio,video")
    .forEach((av) => modifyMedia(av));

  /* Move slides into the fake container */
  for (const section of topLevelSections) {
    handoutSlides.appendChild(section);
  }

  // setup slides feedback
  if (Reveal.hasPlugin("feedback")) {
    const feedback = Reveal.getPlugin("feedback");
    const engine = feedback.getEngine();
    if (engine && engine.api) {
      for (const slide of allSlides) {
        engine.api
          .getComments(engine.deckId, slide.id, null)
          .then((comments) => {
            if (comments.length > 0) {
              const container = document.createElement("div");
              container.className = "handout-feedback-container";
              slide.appendChild(container);
              const heading = document.createElement("h4");
              heading.innerText = localization.comment_header;
              container.appendChild(heading);
              const commentWrapper = document.createElement("div");
              commentWrapper.className = "handout-feedback-comments";
              container.appendChild(commentWrapper);
              for (const comment of comments) {
                const message = document.createElement("div");
                message.className = "handout-feedback-comment";
                message.innerHTML = comment.html;
                commentWrapper.appendChild(message);
                window.MathJax.typeset([message]);
                for (const answer of comment.answers) {
                  const message = document.createElement("div");
                  message.className = "handout-feedback-answer";
                  message.innerHTML = answer.html;
                  commentWrapper.appendChild(message);
                  window.MathJax.typeset([message]);
                }
              }
            }
          })
          .catch((error) => {
            console.error("[HANDOUT FEEDBACK] Error while fetching comments.");
            console.error(error);
          });
      }
    }
  }

  // create intersection observers
  createVisibleSlideIntersectionObserver(topLevelSections);
  createSRCIntersectionObserver();

  /* Attach fake container to actual DOM and finallize setup*/
  revealElem.parentElement.insertBefore(handoutContainer, revealElem);
  attachWindowEventListeners();

  /* adjust height of extra whiteboard slides */
  handoutContainer
    .querySelectorAll("svg.whiteboard")
    .forEach(makeWhiteboardVisible);

  /* Scroll to the current slide (I like smooth more but it gets cancelled inside some decks) */
  currentSlide.scrollIntoView({ behavior: "instant", start: "top" });

  /* patch Reveal functions for slide navigation */
  bak_getCurrentSlide = Reveal.getCurrentSlide;
  Reveal.getCurrentSlide = getCurrentSlide;
  bak_getIndices = Reveal.getIndices;
  Reveal.getIndices = getIndices;
  bak_slide = Reveal.slide;
  Reveal.slide = slide;
}

function disassembleHandoutMode() {
  // restore Reveal functions
  Reveal.getCurrentSlide = bak_getCurrentSlide;
  Reveal.getIndices = bak_getIndices;
  Reveal.slide = bak_slide;

  /* Restore old viewport meta */
  const meta = document.querySelector("meta[name=viewport]");
  if (meta) {
    meta.setAttribute("content", storedMetaViewport);
  }

  const commentContainers = document.querySelectorAll(
    ".handout-feedback-container"
  );
  for (const container of commentContainers) {
    container.remove();
  }

  // Change state of view menu button
  if (pluginButton) {
    pluginButton.setLabel(localization.activate_handout_mode);
  }

  // Restore configuration
  Reveal.configure(previousRevealConfiguration);

  // Remove class from root
  document.documentElement.classList.remove("handout");
  handoutSlideMode = false;

  let revealContainer = Reveal.getRevealElement();
  let slides = handoutSlides.childNodes;
  // Create a 2nd list to iterate over because we will be removing elements from the childNodes list
  let iterate = [...slides];
  let revealSlidesElement = Reveal.getSlidesElement();

  // Restore audio/video (if not also locked by a11y-mode)
  handoutContainer
    .querySelectorAll("audio,video")
    .forEach((av) => restoreMedia(av));

  // remove background images/videos/iframes
  handoutContainer.querySelectorAll(".handoutBackground").forEach((e) => {
    e.remove();
  });

  // Reattach slides to original slides container
  for (const slide of iterate) {
    revealSlidesElement.appendChild(slide);
  }
  handoutContainer.parentElement.insertBefore(
    revealContainer,
    handoutContainer.nextSibling
  );
  detachWindowEventListeners();

  // delete intersection observers
  visibleSlideIntersectionObserver = undefined;
  srcIntersectionObserver = undefined;

  /* Remove the fake container from the DOM */
  handoutContainer.remove();

  /* Force reveal to do recalculations on returned slides */
  Reveal.sync();
  Reveal.layout();

  /* If we could determine a current slide from scrolling, move reveal to it */
  if (centralSlide) {
    const indices = Reveal.getIndices(centralSlide);
    Reveal.slide(indices.h);
  }
}

let bak_getCurrentSlide;
let bak_getIndices;
let bak_slide;

function getCurrentSlide() {
  return centralSlide;
}

function getIndices(slide) {
  let h = Array.prototype.indexOf.call(handoutSlides.children, slide);
  let v, f;
  return { h, v, f };
}

function slide(h, v, f) {
  // console.log("go to slide ", h, v, f);
  let slide = handoutSlides.children[h];
  slide.scrollIntoView({ block: "center" });
}

/* Remove inert, hidden and aria-hidden attributes of slides */
function makeSlidesVisible(slideElement) {
  const slides = slideElement.querySelectorAll("section");
  for (const slide of slides) {
    slide.inert = false;
    slide.hidden = false;
    slide.style.display = "block";
    slide.removeAttribute("aria-hidden");
  }
}

/**
 * Stores the slide's horizontal index in their dataset so scrolling can update
 * the user's progress for the index page's bars.
 * @param {*} slideElementList
 */
function storeIndices(slideElementList) {
  for (const section of slideElementList) {
    const indices = Reveal.getIndices(section);
    if (section.classList.contains("stack")) {
      const subsections = section.querySelectorAll(":scope > section");
      for (const subsection of subsections) {
        if (indices && indices.h) {
          subsection.dataset.hIndex = indices.h;
        }
      }
    }
    if (indices && indices.h) {
      section.dataset.hIndex = indices.h;
    }
  }
}

/**
 * Out of all the currently visible slides, pick the one most central.
 * Update menu plugin and decker plugin.
 */
function updateCurrentSlide(event) {
  if (!handoutSlideMode) return;

  const containerRect = document.body.getBoundingClientRect();
  const containerCenter = (containerRect.bottom + containerRect.top) / 2;

  let minDist = Number.MAX_VALUE;
  let minSlide = undefined;
  for (const slide of visibleSlides) {
    const slideRect = slide.getBoundingClientRect();
    const slideCenter = (slideRect.bottom + slideRect.top) / 2;
    const dist = Math.abs(slideCenter - containerCenter);
    if (dist < minDist) {
      minDist = dist;
      minSlide = slide;
    }
  }

  // If the current slide changed
  if (centralSlide !== minSlide) {
    setCurrentSlide(minSlide);
  }
}

function setCurrentSlide(slide) {
  // visualize central slide
  if (centralSlide) centralSlide.classList.remove("current");
  slide.classList.add("current");

  centralSlide = slide;

  // Inform menu plugin: highlight current slide
  const menuPlugin = Reveal.getPlugin("decker-menu");
  if (menuPlugin) menuPlugin.updateCurrentSlideMark(centralSlide);

  // Inform decker plugin: update last visited slide and progress percentage
  const deckerPlugin = Reveal.getPlugin("decker");
  if (deckerPlugin) deckerPlugin.updateProgress(centralSlide);

  // Inform feedback plugin: update list of questions
  const feedbackPlugin = Reveal.getPlugin("feedback");
  if (feedbackPlugin) feedbackPlugin.slideChanged(centralSlide);

  // update location hash (without triggering onhashchanged!)
  history.replaceState(null, null, "#/" + centralSlide.id);
}

/**
 * Create an intersection observer, if it not already exists,
 * that manages the slides that are currently visible so the
 * onscroll callback can be optimised.
 */
function createVisibleSlideIntersectionObserver(slideElementList) {
  // Callback to add or remove slide to the list of currently visible slides
  const visibilityCallback = function (entries, observer) {
    // Do nothing if we left handout mode
    if (!handoutSlideMode) return;
    for (const entry of entries) {
      if (entry.isIntersecting) {
        visibleSlides.add(entry.target);
      } else {
        visibleSlides.delete(entry.target);
      }
    }
    updateCurrentSlide();
  };

  // Only trigger if a section becomes partly visible or disappears entirely
  const visibilityObserverOptions = {
    root: document.body,
    threshold: [0, 0.95],
  };
  visibleSlideIntersectionObserver = new IntersectionObserver(
    visibilityCallback,
    visibilityObserverOptions
  );

  // Observe all actual sections, not the container sections of vertical stacks
  for (const section of slideElementList) {
    if (section.classList.contains("stack")) {
      const subsections = section.querySelectorAll("section");
      for (const subsection of subsections) {
        visibleSlideIntersectionObserver.observe(subsection);
      }
    } else {
      visibleSlideIntersectionObserver.observe(section);
    }
  }
}

/**
 * Create an intersection observer that observes all video, audio and iframe
 * elements to only load their src if they are close to being visible.
 */
function createSRCIntersectionObserver() {
  const observerOptions = {
    root: document.body,
    rootMargin: "50%",
    threshold: [0],
  };

  const toggleSrc = function (entries, observer) {
    // Do nothing if we have an iframe in fullscreen mode
    if (!handoutSlideMode) return;
    if (
      document.fullscreenElement &&
      document.fullscreenElement !== document.documentElement
    ) {
      return;
    }
    entries.forEach((entry) => {
      if (entry.isIntersecting) {
        if (!entry.target.src) {
          entry.target.src = entry.target.dataset.src;
        }
      } else {
        if (entry.target.src) {
          entry.target.removeAttribute("src");
        }
      }
    });
  };

  srcIntersectionObserver = new IntersectionObserver(
    toggleSrc,
    observerOptions
  );

  handoutContainer
    .querySelectorAll("[data-src]")
    .forEach((elem) => srcIntersectionObserver.observe(elem));
}

/**
 * Scale slide container to fit screen width without changing internal slide resolution
 */
function onWindowResize(event) {
  /* Update internal slide scaling only upon activation to allow later resizing with CTRL + +/- */
  const viewport = document.getElementsByClassName("reveal-viewport")[0];
  const slideWidth = Reveal.getConfig().width;
  const viewportWidth = viewport.offsetWidth;

  slideScale = viewportWidth / slideWidth;
  updateScaling();
}

/* update scaling based on viewport/slide dimensions and user settings */
function updateScaling() {
  // clamp to (slightly smaller than) one to avoid horizontal scrollbar
  if (userScale > 0.95 && userScale < 1.05) userScale = 0.99;
  if (localStorage) {
    localStorage.setItem("handoutScale", userScale);
  }
  // This is where slideScale is used to make the default "fullscreen"
  const scale = slideScale * userScale;
  handoutSlides.style.setProperty("--scale-factor", scale);
  const containerRect = handoutContainer.getBoundingClientRect();
  const slidesRect = handoutSlides.getBoundingClientRect();
  // if the slides are larger than the viewport: scale from top left to fit to screen
  if (slidesRect.width > containerRect.width) {
    handoutSlides.style.transformOrigin = "top left";
    handoutSlides.style.margin = "0";
    handoutSlides.style.left = "0";
    handoutSlides.style.translate = "0";
  } else {
    handoutSlides.style.transformOrigin = null;
    handoutSlides.style.margin = null;
    handoutSlides.style.left = null;
    handoutSlides.style.translate = null;
  }
  if (centralSlide) {
    centralSlide.scrollIntoView({ behavior: "instant", block: "center" });
  }
}

/* return slide scaling factor */
function scaling() {
  return slideScale * userScale;
}

/**
 * Key listener to handle scrolling in handout mode
 * Do not use smooth scrolling, since it messes with intersection observers
 */
function onWindowKeydown(event) {
  const viewport = document.getElementsByClassName("reveal-viewport")[0];
  const viewportHeight = viewport.offsetHeight;
  const slideHeight = Reveal.getConfig().height * scaling();
  const pageHeight = Math.max(
    Math.floor(viewportHeight / slideHeight) * slideHeight,
    slideHeight
  );

  switch (event.key) {
    case "ArrowUp":
    case "ArrowLeft":
      handoutContainer.scrollBy(0, -slideHeight);
      break;

    case "ArrowDown":
    case "ArrowRight":
      handoutContainer.scrollBy(0, slideHeight);
      break;

    case "PageUp":
      document.body.scrollBy({ left: 0, top: -pageHeight, behavior: "smooth" });
      event.preventDefault();
      break;

    case "PageDown":
      document.body.scrollBy({ left: 0, top: pageHeight, behavior: "smooth" });
      event.preventDefault();
      break;

    case "Home":
      const first = handoutSlides.firstElementChild;
      if (first) {
        first.scrollIntoView({
          block: "start",
          inline: "nearest",
        });
      }
      break;

    case "End":
      const last = handoutSlides.lastElementChild;
      if (last) {
        if (last.classList.contains("stack")) last = last.lastElementChild;
        last.scrollIntoView({
          block: "end",
          inline: "nearest",
        });
      }
  }
}

/**
 * Add resize event listener to window
 */
function attachWindowEventListeners() {
  window.addEventListener("resize", onWindowResize);
  window.addEventListener("keydown", onWindowKeydown);
  window.dispatchEvent(new Event("resize"));
}

/**
 * Remove resize event listener from window
 */
function detachWindowEventListeners() {
  window.removeEventListener("resize", onWindowResize);
  window.removeEventListener("keydown", onWindowKeydown);
}

/**
 * Initialise all svgs of all slides as if they were first viewed
 * like the whiteboard plugin does
 * @param {*} svg
 */
function makeWhiteboardVisible(svg) {
  const paths = svg.querySelectorAll("path");
  for (const path of paths) {
    path.style.visibility = "visible";
  }
  svg.style.display = "block";
  const bbox = svg.getBBox();
  const scribbleHeight = bbox.y + bbox.height;
  const pageHeight = Reveal.getConfig().height;
  const height =
    pageHeight * Math.max(1, Math.ceil(scribbleHeight / pageHeight));
  svg.style.height = height + "px";
}

/**
 * Toggle Function
 */
function toggleHandoutMode() {
  if (!handoutSlideMode) {
    activateHandoutMode();
    Decker.flash.message(localization.handout_mode_on);
  } else {
    disassembleHandoutMode();
    Decker.flash.message(localization.handout_mode_off);
  }
}

function attachAnimatedIcon(button) {
  const first = document.createElement("div");
  first.className = "top-anim-rect";
  const second = document.createElement("div");
  second.className = "bottom-anim-rect";
  button.appendChild(first);
  button.appendChild(second);
}

/**
 * Add handout mode button to Menu plugin.
 * Add zoom in/out buttons to top right anchor.
 */
function createButtons() {
  // add button to menu plugin
  const menu = Reveal.getPlugin("decker-menu");
  if (menu && !!menu.addPluginButton) {
    pluginButton = menu.addPluginButton(
      "menu-handout-button",
      "animated-button",
      localization.activate_handout_mode,
      toggleHandoutMode
    );
    attachAnimatedIcon(pluginButton);
  }

  // add zoom in/out buttons
  const anchors = Reveal.getPlugin("ui-anchors");
  if (anchors) {
    let buttonMinus = document.createElement("button");
    buttonMinus.id = "handout-minus";
    buttonMinus.ariaLabel =
      navigator.language === "de" ? "Verkleinern" : "Zoom Out";
    buttonMinus.className = "fa-button fa-solid fa-magnifying-glass-minus";
    buttonMinus.onclick = () => {
      userScale /= 1.25;
      updateScaling();
    };
    let buttonPlus = document.createElement("button");
    buttonPlus.id = "handout-plus";
    buttonPlus.ariaLabel =
      navigator.language === "de" ? "Vergrößern" : "Zoom In";
    buttonPlus.className = "fa-button fa-solid fa-magnifying-glass-plus";
    buttonPlus.onclick = () => {
      userScale *= 1.25;
      updateScaling();
    };
    anchors.placeButton(buttonMinus, "TOP_RIGHT");
    anchors.placeButton(buttonPlus, "TOP_RIGHT");
  }
}

const a11y = /a11y/gi.test(window.location.search);
const handout = /handout/gi.test(window.location.search);

const Plugin = {
  id: "handout",
  isActive: () => handoutSlideMode,
  currentSlide: () => centralSlide,
  init: (reveal) => {
    Reveal = reveal;
    createButtons();

    if (localStorage) {
      const initScale = localStorage.getItem("handoutScale");
      if (initScale) {
        userScale = initScale;
      }
    }

    /* Add triple click H to toggle handout mode to reveal keybindings */
    reveal.addKeyBinding(
      {
        keyCode: 72,
        key: "H",
        description: "Toggle Handout Mode (Triple Click)",
      },

      Decker.tripleClick(() => {
        toggleHandoutMode();
      })
    );
    if (a11y || handout) {
      Reveal.addEventListener("ready", () => {
        toggleHandoutMode();
      });
    }
  },
};

export default Plugin;
