import {
  setupFlyingFocus,
  hideFlyingFocus,
} from "../../flyingFocus/flying-focus.js";

let Reveal;

let handoutSlideMode = false;
const fakeRevealContainer = document.createElement("div");
const fakeSlideContainer = document.createElement("div");
fakeRevealContainer.appendChild(fakeSlideContainer);

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
let currentFeedbackSlide;

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

function undoAutomaticSlideAdjustments(slideElement) {
  let slides = slideElement.querySelectorAll("section");
  for (const slide of slides) {
    slide.dataset["previousInert"] = slide.inert;
    slide.inert = false;
    slide.dataset["previousTop"] = slide.style.top;
    slide.style.top = null;
    slide.dataset["previousDisplay"] = slide.style.display;
    slide.style.display = null;
    slide.dataset["previousHidden"] = slide.hidden;
    slide.hidden = false;
    slide.dataset["previousAriaHidden"] = slide.getAttribute("aria-hidden");
    slide.removeAttribute("aria-hidden");
    if (!slide.style["min-height"]) {
      //If we have not already assigned a min-height, set it at least as high as the slide height
      slide.style["min-height"] = slide.style.height;
    }
    slide.style.height = null;
  }
}

function findAllRealSlides(slidesElement, slideList) {
  for (const child of slidesElement.childNodes) {
    if (child.tagName !== "SECTION") continue;
    if (child.classList.contains("stack")) {
      for (const subchild of child.childNodes) {
        if (subchild.tagName !== "SECTION") continue;
        subchild.previousParent = child;
        slideList.push(subchild);
      }
      continue;
    }
    slideList.push(child);
  }
}

let feedbackIntersectionObserver = undefined;
let srcIntersectionObserver = undefined;

let visibleSlides = new Set();

function determineMostVisibleSlide(event) {
  if (!handoutSlideMode) return;
  let mostVisible = undefined;
  let mostVisibleValue = undefined;
  const host = fakeRevealContainer.getBoundingClientRect();
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
  if (currentFeedbackSlide !== mostVisible) {
    currentFeedbackSlide = mostVisible;
    const feedback = Reveal.getPlugin("feedback");
    feedback.requestMenuContent(mostVisible);
    const menu = Reveal.getPlugin("decker-menu");
    menu.updateCurrentSlideMark(mostVisible);
  }
}

function createFeedbackIntersectionObserver(slideList) {
  if (feedbackIntersectionObserver) {
    return;
  }
  const feedback = Reveal.getPlugin("feedback");
  if (!!feedback) {
    if (feedback.getEngine().api) {
      const feedbackCallback = function (entries, observer) {
        if (!handoutSlideMode) return;
        for (const entry of entries) {
          if (entry.isIntersecting) {
            visibleSlides.add(entry.target);
          } else {
            visibleSlides.delete(entry.target);
          }
        }
      };
      const feedbackObserverOptions = {
        root: fakeRevealContainer,
        threshold: [0],
      };
      feedbackIntersectionObserver = new IntersectionObserver(
        feedbackCallback,
        feedbackObserverOptions
      );
      for (const child of slideList) {
        feedbackIntersectionObserver.observe(child);
      }
      fakeRevealContainer.addEventListener("scroll", determineMostVisibleSlide);
    }
  }
}

function createSRCIntersectionObserver(slideList) {
  if (srcIntersectionObserver) {
    return;
  }
  const observerOptions = {
    root: fakeRevealContainer,
    rootMargin: "50%",
    threshold: [0],
  };

  const toggleSrc = function (entries, observer) {
    if (!handoutSlideMode) return;
    entries.forEach((entry) => {
      if (entry.isIntersecting) {
        entry.target.src = entry.target.dataset["src"];
      } else {
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
  const videos = fakeRevealContainer.getElementsByTagName("VIDEO");
  for (const video of videos) {
    video.dataset.previousAutoplay = video.dataset.autoplay;
    delete video.dataset.autoplay;
    srcIntersectionObserver.observe(video);
  }
}

function onWindowResize(event) {
  const viewport = document.getElementsByClassName("reveal-viewport")[0];
  const width = Reveal.getConfig().width;
  const ow = viewport.offsetWidth;
  const scale = ow / width;
  fakeSlideContainer.style.transform = "scale(" + scale + ")";
  /*  if (ow < width) {
    const scale = ow / width;
    fakeSlideContainer.style.transform = "scale(" + scale + ")";
  } else {
    fakeSlideContainer.style.transform = "scale(1)";
  }*/
}

function attachResizeEventListener() {
  window.addEventListener("resize", onWindowResize);
  window.dispatchEvent(new Event("resize"));
}

function detachResizeEventListener() {
  window.removeEventListener("resize", onWindowResize);
}

const previousRevealConfiguration = {
  keyboard: undefined,
  controls: undefined,
  progress: undefined,
  fragments: undefined,
  slideNumber: undefined,
  disableLayout: undefined,
};

function prepareWhiteboardSVG(svg) {
  const paths = svg.querySelectorAll("path");
  for (const path of paths) {
    path.style.visibility = "visible";
  }
  svg.dataset["previousDisplay"] = svg.style.display;
  svg.style.display = "block";
  const bbox = svg.getBBox();
  const scribbleHeight = bbox.y + bbox.height;
  const pageHeight = Reveal.getConfig().height;
  const height =
    pageHeight * Math.max(1, Math.ceil(scribbleHeight / pageHeight));
  svg.style.height = height + "px";
}

function activateA11yMode() {
  const currentSlide = Reveal.getCurrentSlide();
  const currentConfiguration = Reveal.getConfig();
  previousRevealConfiguration.keyboard = currentConfiguration.keyboard;
  previousRevealConfiguration.controls = currentConfiguration.controls;
  previousRevealConfiguration.progress = currentConfiguration.progress;
  previousRevealConfiguration.fragments = currentConfiguration.fragments;
  previousRevealConfiguration.slideNumber = currentConfiguration.slideNumber;
  previousRevealConfiguration.disableLayout =
    currentConfiguration.disableLayout;
  Reveal.configure({
    keyboard: false,
    controls: false,
    progress: false,
    fragments: false,
    slideNumber: false,
    disableLayout: true,
  });
  document.documentElement.classList.add("a11y");
  const revealElem = Reveal.getRevealElement();
  fakeRevealContainer.classList.add("reveal", "a11y-container");
  const slidesElement = Reveal.getSlidesElement();
  fakeSlideContainer.classList.add("slides");
  const slideList = [];
  undoAutomaticSlideAdjustments(slidesElement);
  findAllRealSlides(slidesElement, slideList);
  createFeedbackIntersectionObserver(slideList);
  for (const child of slideList) {
    const whiteboard = child.getElementsByClassName("whiteboard")[0];
    if (whiteboard) {
      prepareWhiteboardSVG(whiteboard);
      const imageHeight = whiteboard.style.height;
      const previousHeight = child.style.height;
      if (
        (previousHeight && imageHeight && previousHeight < imageHeight) ||
        imageHeight
      ) {
        child.dataset["previousHeight"] = previousHeight;
        child.style["height"] = imageHeight;
        const container = child.querySelectorAll(".decker")[0];
        container.style.height = "var(--slide-height)";
      }
    }
    fakeSlideContainer.appendChild(child);
  }
  createSRCIntersectionObserver(slideList);
  revealElem.parentElement.insertBefore(fakeRevealContainer, revealElem);
  handoutSlideMode = true;
  attachResizeEventListener();
  currentSlide.scrollIntoView({ behavior: "smooth", start: "top" });
}

function recoverSlideAttributes(slide) {
  const whiteboardsvg = slide.getElementsByClassName("whiteboard")[0];
  if (whiteboardsvg && whiteboardsvg.dataset["previousDisplay"]) {
    whiteboardsvg.style.display = whiteboardsvg.dataset["previousDisplay"];
    whiteboardsvg.dataset["previousDisplay"] = null;
  }
  if (slide.dataset["previousInert"]) {
    slide.inert = slide.dataset["previousInert"];
    slide.dataset["previousInert"] = null;
  }
  if (slide.dataset["previousHeight"]) {
    slide.style.height = slide.dataset["previousHeight"];
    slide.dataset["previousHeight"] = null;
  }
  if (slide.dataset["previousTop"]) {
    slide.style.top = slide.dataset["previousTop"];
    slide.dataset["previousTop"] = null;
  }
  if (slide.dataset["previousDisplay"]) {
    slide.style.display = slide.dataset["previousDisplay"];
    slide.dataset["previousDisplay"] = null;
  }
  if (slide.dataset["previousHidden"]) {
    slide.hidden = slide.dataset["previousHidden"];
    slide.dataset["previousHidden"] = null;
  }
  if (slide.dataset["previousAriaHidden"]) {
    slide.setAttribute("aria-hidden", slide.dataset["previousAriaHidden"]);
    slide.dataset["previousAriaHidden"] = null;
  }
}

function disassembleA11yMode() {
  Reveal.configure(previousRevealConfiguration);
  document.documentElement.classList.remove("a11y");
  let revealContainer = Reveal.getRevealElement();
  let slides = fakeSlideContainer.childNodes;
  let iterate = [...slides];
  let revealSlides = Reveal.getSlidesElement();
  for (const slide of iterate) {
    if (slide.previousParent) {
      slide.previousParent.appendChild(slide);
    } else {
      revealSlides.appendChild(slide);
    }
    recoverSlideAttributes(slide);
    const videos = slide.getElementsByTagName("VIDEO");
    for (const video of videos) {
      video.dataset.autoplay = video.dataset.previousAutoplay;
      delete video.dataset.previousAutoplay;
    }
  }
  handoutSlideMode = false;
  fakeRevealContainer.parentElement.insertBefore(
    revealContainer,
    fakeRevealContainer.nextSibling
  );
  detachResizeEventListener();
  fakeRevealContainer.parentElement.removeChild(fakeRevealContainer);
}

function toggleA11yMode() {
  if (!handoutSlideMode) {
    activateA11yMode();
  } else {
    disassembleA11yMode();
  }
}

function addMenuButton() {
  const menu = Reveal.getPlugin("decker-menu");
  let buttonTitle = "Toggle accessible handoutmode";
  if (navigator.language === "de") {
    buttonTitle = "Barrierefreien Handoutmodus umschalten";
  }
  if (menu) {
    if (menu.addMenuButton) {
      menu.addMenuButton(
        "menu-accessibility-button",
        "fa-universal-access",
        buttonTitle,
        toggleA11yMode
      );
    }
  }
}

const Plugin = {
  id: "a11y",
  init: (reveal) => {
    Reveal = reveal;
    fixTabsByInert();
    addFlyingFocusCallbacks();
    addCustomSpacebarHandler();
    addMenuButton();
  },
};

export default Plugin;
