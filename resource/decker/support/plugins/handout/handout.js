import { modifyMedia, restoreMedia } from "../../js/media-a11y.js";

let Reveal;

let centralSlide;

let handoutSlideMode = false;
const fakeRevealContainer = document.createElement("div");
const fakeSlideContainer = document.createElement("div");
fakeRevealContainer.appendChild(fakeSlideContainer);

function activateHandoutMode() {
  const currentSlide = Reveal.getCurrentSlide();
  // Store current reveal config and disable everything but keyboard shortcuts
  const currentConfiguration = Reveal.getConfig();
  previousRevealConfiguration.controls = currentConfiguration.controls;
  previousRevealConfiguration.progress = currentConfiguration.progress;
  previousRevealConfiguration.fragments = currentConfiguration.fragments;
  previousRevealConfiguration.slideNumber = currentConfiguration.slideNumber;
  previousRevealConfiguration.disableLayout =
    currentConfiguration.disableLayout;
  Reveal.configure({
    controls: false,
    progress: false,
    fragments: false,
    slideNumber: false,
    disableLayout: true,
  });
  //add class to root to enable special rules from handout.css
  document.documentElement.classList.add("handout");
  handoutSlideMode = true;
  //setup classes for fake containers
  fakeRevealContainer.classList.add("reveal", "handout-container");
  fakeSlideContainer.classList.add("slides");
  const revealElem = Reveal.getRevealElement();
  const slidesElement = Reveal.getSlidesElement();
  makeSlidesVisible(slidesElement);
  const topLevelSections = slidesElement.querySelectorAll(":scope > section");
  storeIndices(topLevelSections);

  /* setup background images */
  slidesElement
    .querySelectorAll("section[data-background-image]")
    .forEach((section) => {
      const url = section.getAttribute("data-background-image");
      section.style.backgroundImage = `url("${url}")`;
    });

  // setup background videos
  slidesElement
    .querySelectorAll("section[data-background-video]")
    .forEach((section) => {
      const video = document.createElement("video");
      const url = section.getAttribute("data-background-video");
      video.src = url;
      video.classList.add("background");
      section.appendChild(video);
    });

  /* Switch controls on and autoplay off */
  const videos = document.getElementsByTagName("VIDEO");
  for (const video of videos) {
    modifyMedia(video);
  }
  const audios = document.getElementsByTagName("AUDIO");
  for (const audio of audios) {
    modifyMedia(audio);
  }

  /* Move slides into the fake container */
  for (const section of topLevelSections) {
    fakeSlideContainer.appendChild(section);
  }

  createVisibleSlideIntersectionObserver(topLevelSections);
  createSRCIntersectionObserver();

  /* Attach fake container to actual DOM and finallize setup*/
  revealElem.parentElement.insertBefore(fakeRevealContainer, revealElem);
  attachWindowEventListeners();

  /* Scroll to the current slide (I like smooth more but it gets cancelled inside some decks) */
  currentSlide.scrollIntoView({ behavior: "instant", start: "top" });
}

function disassembleHandoutMode() {
  // Restore configuration
  Reveal.configure(previousRevealConfiguration);
  // Remove class from root
  document.documentElement.classList.remove("handout");
  handoutSlideMode = false;

  let revealContainer = Reveal.getRevealElement();
  let slides = fakeSlideContainer.childNodes;
  // Create a 2nd list to iterate over because we will be removing elements from the childNodes list
  let iterate = [...slides];
  let revealSlidesElement = Reveal.getSlidesElement();

  // Restore video (if not also locked by a11y-mode)
  const videos = document.getElementsByTagName("VIDEO");
  for (const video of videos) {
    restoreMedia(video);
  }
  const audios = document.getElementsByTagName("AUDIO");
  for (const audio of audios) {
    restoreMedia(audio);
  }

  // remove background images
  fakeRevealContainer
    .querySelectorAll("section[data-background-image]")
    .forEach((section) => {
      section.style.backgroundImage = "";
    });
  // remove background videos
  fakeRevealContainer.querySelectorAll("video.background").forEach((video) => {
    video.remove();
  });

  // Reattach slides to original slides container
  for (const slide of iterate) {
    revealSlidesElement.appendChild(slide);
  }
  fakeRevealContainer.parentElement.insertBefore(
    revealContainer,
    fakeRevealContainer.nextSibling
  );
  detachWindowEventListeners();
  /* Remove the fake container from the DOM */
  fakeRevealContainer.parentElement.removeChild(fakeRevealContainer);
  /* Force reveal to do recalculations on returned slides */
  Reveal.sync();
  Reveal.layout();
  /* If we could determine a current slide from scrolling, move reveal to it */
  if (centralSlide) {
    const indices = Reveal.getIndices(centralSlide);
    Reveal.slide(indices.h);
  }
}

/* Remove inert, hidden and aria-hidden attributes of slides */
function makeSlidesVisible(slideElement) {
  const slides = slideElement.querySelectorAll("section");
  for (const slide of slides) {
    slide.inert = false;
    slide.hidden = false;
    slide.removeAttribute("aria-hidden");
    //if the slide has a whiteboard make it visible
    const whiteboard = slide.getElementsByClassName("whiteboard")[0];
    if (whiteboard) {
      makeWhiteboardVisible(whiteboard);
    }
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

let visibleSlideIntersectionObserver = undefined;
let srcIntersectionObserver = undefined;

let visibleSlides = new Set();

/**
 * Out of all the currently visible slides, pick the one most central
 * @param {*} event
 * @returns
 */
function determineMostVisibleSlide(event) {
  if (!handoutSlideMode) return;
  let mostVisible = undefined;
  let mostVisibleValue = undefined;
  const host = fakeRevealContainer.getBoundingClientRect();
  // This can probably be improved but it works
  for (const slide of visibleSlides) {
    const box = slide.getBoundingClientRect();
    const offset = box.top - host.top;
    let visible = 0;
    if (offset < 0) {
      visible = box.bottom - host.top;
    } else {
      visible = host.bottom - box.top;
    }
    if (!mostVisible || mostVisibleValue < visible) {
      mostVisible = slide;
      mostVisibleValue = visible;
    }
  }
  /* If the current central slide changed */
  if (centralSlide !== mostVisible) {
    centralSlide = mostVisible;
    // Inform feedback plugin (load questions)
    const feedback = Reveal.getPlugin("feedback");
    if (feedback && feedback.getEngine().api) {
      feedback.requestMenuContent(mostVisible);
    }
    // Inform menu plugin (highlight current slide)
    const menu = Reveal.getPlugin("decker-menu");
    if (menu) {
      menu.updateCurrentSlideMark(mostVisible);
    }
    // Inform decker plugin (index page)
    const decker = Reveal.getPlugin("decker");
    if (decker && mostVisible.dataset.hIndex) {
      decker.updateLastVisitedSlide({ h: Number(mostVisible.dataset.hIndex) });
      decker.updatePercentage(Number(mostVisible.dataset.hIndex));
    }
  }
}

/**
 * Create an intersection observer, if it not already exists,
 * that manages the slides that are currently visible so the
 * onscroll callback can be optimised.
 * @param {*} slideElementList
 * @returns
 */
function createVisibleSlideIntersectionObserver(slideElementList) {
  // Do not reinitialise if we already entered handout mode in the past
  if (visibleSlideIntersectionObserver) {
    return;
  }
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
  };
  // Only trigger if a section becomes partly visible or disappears entirely
  const visibilityObserverOptions = {
    root: fakeRevealContainer,
    threshold: [0],
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
  fakeRevealContainer.addEventListener("scroll", determineMostVisibleSlide);
}

/**
 * Create an intersection observer that observes all video, audio and iframe
 * elements to only load their src if they are close to being visible.
 * @returns Nothing
 */
function createSRCIntersectionObserver() {
  if (srcIntersectionObserver) {
    return;
  }
  const observerOptions = {
    root: fakeRevealContainer,
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
        if (!entry.target.src && entry.target.dataset.src) {
          entry.target.src = entry.target.dataset.src;
        }
      } else {
        if (entry.target.src) {
          entry.target.dataset.src = entry.target.src;
        }
        entry.target.removeAttribute("src");
      }
    });
  };

  srcIntersectionObserver = new IntersectionObserver(
    toggleSrc,
    observerOptions
  );
  const iframes = fakeRevealContainer.getElementsByTagName("IFRAME");
  for (const iframe of iframes) {
    srcIntersectionObserver.observe(iframe);
  }
  const audios = fakeRevealContainer.getElementsByTagName("AUDIO");
  for (const audio of audios) {
    srcIntersectionObserver.observe(audio);
  }
  const videos = fakeRevealContainer.getElementsByTagName("VIDEO");
  for (const video of videos) {
    srcIntersectionObserver.observe(video);
  }
}

/**
 * Scale slide container to fit screen width without changing internal slide resolution
 * @param {*} event
 */
function onWindowResize(event) {
  const viewport = document.getElementsByClassName("reveal-viewport")[0];
  const width = Reveal.getConfig().width;
  const ow = viewport.offsetWidth;
  const scale = ow / width;
  fakeSlideContainer.style.transform = "scale(" + scale + ")";
}

let lockScrolling;

/**
 * Keyup listener to handle scrolling in handout mode
 */
function onWindowKeydown(event) {
  if (lockScrolling) return;
  if (event.key === "ArrowUp") {
    lockScrolling = true;
    fakeRevealContainer.scrollBy({ top: -256, left: 0, behavior: "smooth" });
  }
  if (event.key === "ArrowDown") {
    lockScrolling = true;
    fakeRevealContainer.scrollBy({ top: 256, left: 0, behavior: "smooth" });
  }
  if (event.key === "PageUp") {
    if (centralSlide) {
      let previousSlide = centralSlide.previousElementSibling;
      let target = previousSlide;
      // If we are the first child in a container
      if (previousSlide === null) {
        // If we are in a stack select the parent's sibling instead
        const parent = centralSlide.parentElement;
        if (parent.classList.contains("stack")) {
          const parentSibling = parent.previousElementSibling;
          // If the parent itself has a stack select the last child of the parent
          if (parentSibling && parentSibling.classList.contains("stack")) {
            target = parentSibling.lastElementChild;
          } else {
            target = parentSibling;
          }
        }
      }
      if (target) {
        lockScrolling = true;
        target.scrollIntoView({ behavior: "smooth" });
      }
    }
  }
  if (event.key === "PageDown") {
    if (centralSlide) {
      let nextSlide = centralSlide.nextElementSibling;
      let target = nextSlide;
      // If we are the final child in a container
      if (nextSlide === null) {
        // If we are in a stack select the parent's sibling instead
        const parent = centralSlide.parentElement;
        if (parent.classList.contains("stack")) {
          const parentSibling = parent.nextElementSibling;
          // If the parent itself has a stack select the first child of the parent
          if (parentSibling && parentSibling.classList.contains("stack")) {
            target = parentSibling.firstElementChild;
          } else {
            target = parentSibling;
          }
        }
      }
      if (target) {
        lockScrolling = true;
        target.scrollIntoView({ behavior: "smooth" });
      }
    }
  }
  if (event.key === "Home") {
    const first = fakeSlideContainer.firstElementChild;
    if (first) {
      first.scrollIntoView({ behavior: "smooth" });
    }
  }
  if (event.key === "End") {
    const last = fakeSlideContainer.lastElementChild;
    if (last) {
      if (last.classList.contains("stack")) {
        const lastVertical = last.lastElementChild;
        lockScrolling = true;
        lastVertical.scrollIntoView({ behavior: "smooth" });
      } else {
        lockScrolling = true;
        last.scrollIntoView({ behavior: "smooth" });
      }
    }
  }
}

function unlockScroll() {
  lockScrolling = false;
}

/**
 * Add resize event listener to window
 */
function attachWindowEventListeners() {
  window.addEventListener("resize", onWindowResize);
  window.addEventListener("keydown", onWindowKeydown);
  fakeRevealContainer.addEventListener("scrollend", unlockScroll);
  window.dispatchEvent(new Event("resize"));
}

/**
 * Remove resize event listener from window
 */
function detachWindowEventListeners() {
  window.removeEventListener("resize", onWindowResize);
  window.removeEventListener("keydown", onWindowKeydown);
  fakeRevealContainer.removeEventListener("scrollend", unlockScroll);
}

const previousRevealConfiguration = {
  controls: undefined,
  progress: undefined,
  fragments: undefined,
  slideNumber: undefined,
  disableLayout: undefined,
};

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
  } else {
    disassembleHandoutMode();
  }
}

/**
 * Add Handout Mode Button to Menu Plugin, if it exists
 */
function addMenuButton() {
  const menu = Reveal.getPlugin("decker-menu");
  let buttonTitle = "Toggle Handoutmode";
  if (navigator.language === "de") {
    buttonTitle = "Handoutmodus umschalten";
  }
  if (menu) {
    if (menu.addMenuButton) {
      menu.addMenuButton(
        "menu-accessibility-button",
        "fa-align-center",
        buttonTitle,
        toggleHandoutMode
      );
    }
  }
}

const Plugin = {
  id: "handout",
  isActive: () => handoutSlideMode,
  init: (reveal) => {
    Reveal = reveal;
    addMenuButton();
    /* Add triple click H to toggle handout mode to reveal keybindings */
    reveal.addKeyBinding(
      {
        keyCode: 72,
        key: "H",
        description: "Toggle Handout Mode (Triple Click)",
      },

      Decker.tripleClick(() => {
        toggleHandoutMode();

        if (handoutSlideMode) {
          Decker.flash.message(
            `<span>Handout Mode: <strong style="color:var(--accent3);">ON</strong></span>`
          );
        } else {
          Decker.flash.message(
            `<span>Handout Mode: <strong style="color:var(--accent1);">OFF</strong></span>`
          );
        }
      })
    );
  },
};

export default Plugin;
