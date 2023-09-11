let Reveal;

let centralSlide;

let handoutSlideMode = false;
const fakeRevealContainer = document.createElement("div");
const fakeSlideContainer = document.createElement("div");
fakeRevealContainer.appendChild(fakeSlideContainer);

/* Remove inert, hidden and aria-hidden attributes of slides */
function makeSlidesVisible(slideElement) {
  let slides = slideElement.querySelectorAll("section");
  for (const slide of slides) {
    slide.inert = false;
    slide.hidden = false;
    slide.removeAttribute("aria-hidden");
  }
}

let visibleSlideIntersectionObserver = undefined;
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
  if (centralSlide !== mostVisible) {
    centralSlide = mostVisible;
    const feedback = Reveal.getPlugin("feedback");
    if (feedback && feedback.getEngine().api) {
      feedback.requestMenuContent(mostVisible);
    }
    const menu = Reveal.getPlugin("decker-menu");
    if (menu) {
      menu.updateCurrentSlideMark(mostVisible);
    }
    const decker = Reveal.getPlugin("decker");
    if (decker && mostVisible.dataset.hIndex) {
      decker.updateLastVisitedSlide({ h: Number(mostVisible.dataset.hIndex) });
      decker.updatePercentage(Number(mostVisible.dataset.hIndex));
    }
  }
}

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
  // Observe all actual section, not the container sections of vertical stacks
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

function adjustVideos(container) {
  const videos = container.getElementsByTagName("VIDEO");
  for (const video of videos) {
    if (!video.dataset.originalAutoplay) {
      video.dataset.originalAutoplay = video.dataset.autoplay;
    }
    if (!video.dataset.originalControls) {
      video.dataset.originalControls = video.controls ? "true" : "false";
    }
    const modifiedCount = video.dataset.modifiedCount
      ? Number(video.dataset.modifiedCount) + 1
      : 1;
    video.dataset.modifiedCount = modifiedCount;
    // Pause the video if it is playing. A blind call to video.pause() may cause an error because the
    // Promise of video.play() that Reveal caused has not resolved yet.
    video.addEventListener(
      "timeupdate",
      () => {
        video.pause();
      },
      { once: true }
    );
    if (!video.dataset.src) {
      video.dataset.src = video.src;
    }
    video.setAttribute("controls", "");
    delete video.dataset.autoplay;
  }
}

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
    // Do nothing if we enter
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
  const videos = fakeRevealContainer.getElementsByTagName("VIDEO");
  for (const video of videos) {
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
  //  svg.dataset["previousDisplay"] = svg.style.display;
  svg.style.display = "block";
  const bbox = svg.getBBox();
  const scribbleHeight = bbox.y + bbox.height;
  const pageHeight = Reveal.getConfig().height;
  const height =
    pageHeight * Math.max(1, Math.ceil(scribbleHeight / pageHeight));
  svg.style.height = height + "px";
}

function activateHandoutMode() {
  const currentSlide = Reveal.getCurrentSlide();
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
  document.documentElement.classList.add("handout");
  const revealElem = Reveal.getRevealElement();
  fakeRevealContainer.classList.add("reveal", "handout-container");
  const slidesElement = Reveal.getSlidesElement();
  fakeSlideContainer.classList.add("slides");
  const topLevelSections = slidesElement.querySelectorAll(":scope > section");
  makeSlidesVisible(slidesElement);
  createVisibleSlideIntersectionObserver(topLevelSections);
  /* Move slides into the fake container */
  for (const section of topLevelSections) {
    const indices = Reveal.getIndices(section);
    if (indices && indices.h) {
      section.dataset.hIndex = indices.h;
    }
    const whiteboard = section.getElementsByClassName("whiteboard")[0];
    if (whiteboard) {
      makeWhiteboardVisible(whiteboard);
    }
    fakeSlideContainer.appendChild(section);
  }
  adjustVideos(fakeRevealContainer);
  createSRCIntersectionObserver();
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
}

function disassembleHandoutMode() {
  Reveal.configure(previousRevealConfiguration);
  document.documentElement.classList.remove("handout");
  let revealContainer = Reveal.getRevealElement();
  let slides = fakeSlideContainer.childNodes;
  let iterate = [...slides];
  let revealSlides = Reveal.getSlidesElement();
  for (const slide of iterate) {
    const videos = slide.getElementsByTagName("VIDEO");
    for (const video of videos) {
      if (
        video.dataset.modifiedCount &&
        Number(video.dataset.modifiedCount) === 1
      ) {
        video.dataset.autoplay = video.dataset.originalAutoplay;
        if (video.dataset.originalControls === "true") {
          video.setAttribute("controls", "");
        } else {
          video.removeAttribute("controls");
        }
        delete video.dataset.modifiedCount;
      }
    }
    if (slide.previousParent) {
      slide.previousParent.appendChild(slide);
    } else {
      revealSlides.appendChild(slide);
    }
    recoverSlideAttributes(slide);
  }
  handoutSlideMode = false;
  fakeRevealContainer.parentElement.insertBefore(
    revealContainer,
    fakeRevealContainer.nextSibling
  );
  detachResizeEventListener();
  fakeRevealContainer.parentElement.removeChild(fakeRevealContainer);
  Reveal.sync();
  Reveal.layout();
  if (centralSlide) {
    const indices = Reveal.getIndices(centralSlide);
    Reveal.slide(indices.h);
  }
}

function toggleHandoutMode() {
  if (!handoutSlideMode) {
    activateHandoutMode();
  } else {
    disassembleHandoutMode();
  }
}

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
