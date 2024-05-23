// reference to Reveal object
let Reveal;

/* this function is called inbetween just before Reveal's setupPDF.
 * We setup the minimum height of slide elements to Reveal's page height.
 */
function setHeight() {
  if (!Reveal.getConfig().center) {
    const height = Reveal.getConfig().height;
    Reveal.getSlides().forEach(function (slide) {
      slide.style.height = height + "px";
    });
  }
}

// pandoc puts footers into a <p> element, which
// makes positioning w.r.t. slide bottom difficult.
// hence we remove these p-elements and put the
// footers as children of the slide element
function fixFooters() {
  Reveal.getSlides().forEach(function (slide) {
    let footers = slide.getElementsByClassName("footer");
    for (var i = 0; i < footers.length; i++) {
      let footer = footers[i];
      let parent = footer.parentElement;
      if (parent.nodeName == "P") {
        slide.appendChild(footer);
        if (parent.childNodes.length == 0) {
          parent.parentElement.removeChild(parent);
        }
      }
    }
  });
}

/* check whether the current slide is too tall and print error in that case */
function checkHeight() {
  const configHeight = Reveal.getConfig().height;
  const slide = Reveal.getCurrentSlide();
  const decker = slide.querySelector(".decker");
  if (decker) {
    const scrollHeight = decker.scrollHeight;
    if (scrollHeight > configHeight) {
      console.warn(
        "slide " +
          slideNumber() +
          " is " +
          (scrollHeight - configHeight) +
          "px too high"
      );
      slide.style.border = "1px dashed red";
    } else {
      slide.style.border = "";
    }
  } else {
    console.error("[PRINT] slide has no decker container");
    console.error(slide);
  }
}

/* return string with current slide number */
function slideNumber() {
  const idx = Reveal.getIndices();
  return idx.v > 0
    ? "(h:" + (idx.h + 1) + ", v:" + (idx.v + 1) + ")"
    : idx.h + 1;
}

/* load iframes that are marked with class 'pdf'.
 * used to include simple Javascript demos in PDF
 */
function setupIframes() {
  for (let e of document.querySelectorAll(
    ".reveal section .media .print iframe[data-src]"
  )) {
    e.src = e.getAttribute("data-src");
  }
}

/*
 * Remove controls from videos, since they mess up printing.
 * If we have >5 videos and are printing from headless Chrome,
 * this will stall due a Chrome bug. Hence we disable videos
 * from the sixth video on in this configuration.
 */
function setupVideos() {
  let numVideos = 0;
  const maxVideos = 5; // headless Chrome can handle 5 videos

  // go through all slides
  for (let slide of document.getElementsByTagName("section")) {
    // do we have a background video?
    if (slide.hasAttribute("data-background-video")) {
      // handle headless Chrome bug
      if (navigator.webdriver && numVideos >= maxVideos) {
        slide.removeAttribute("data-background-video");
      }

      // play video to 0.5s to get a poster frame
      else {
        var src = slide.getAttribute("data-background-video");
        if (!src.includes("#t=")) {
          src = src + "#t=0.5";
          slide.setAttribute("data-background-video", src);
        }
      }

      numVideos++;
    }

    // do we have videos on this slide?
    for (let video of slide.getElementsByTagName("video")) {
      // handle headless Chrome bug
      if (navigator.webdriver && numVideos >= maxVideos) {
        /* do not load video */
        video.src = "";
        video.removeAttribute("data-src");
        /* hide video, since we cannot determine its size anyway */
        video.style.display = "none";
      }

      // play video to 0.5s to get a poster frame
      else {
        if (video.hasAttribute("data-src")) {
          var src = video.getAttribute("data-src");
          if (!src.includes("#t=")) src = src + "#t=0.5";

          video.src = src;
          video.removeAttribute("data-src");
        }

        video.removeAttribute("controls");
        video.removeAttribute("data-autoplay");
        video.removeAttribute("autoplay");
      }

      numVideos++;
    }
  }
}

// set title, such that the exported PDF has the same filename
function setupTitle() {
  const url = window.location.pathname;
  const filename = url.substring(url.lastIndexOf("/") + 1);
  const basename = filename.substring(0, url.lastIndexOf("."));
  document.title = basename + "pdf";
}

// copied from reveal.js
function injectStyleSheet(value) {
  var tag = document.createElement("style");
  tag.type = "text/css";
  if (tag.styleSheet) {
    tag.styleSheet.cssText = value;
  } else {
    tag.appendChild(document.createTextNode(value));
  }
  document.getElementsByTagName("head")[0].appendChild(tag);
}

// Reveal messes up the page margin when printing multi-page slides.
// In particular this happens for extra whiteboard pages.
// This function set's Reveal's margin to zero and instead
// implements the page margin through CSS.
function setupMargin() {
  const width = Reveal.getConfig().width;
  const height = Reveal.getConfig().height;
  const margin = Reveal.getConfig().margin;
  const marginX = Math.floor(0.5 * width * margin);
  const marginY = Math.floor(0.5 * height * margin);
  // override Reveals page size and setting (hence !important)
  let css = `@page{size: ${width}px ${height}px !important; margin: ${marginY}px ${marginX}px !important;} `;
  // add transparent top border to prevent margin of page and margin of first element
  // on a slide (mostly h1) to collapse
  css += `.reveal .slides section { border-top: 1px solid transparent; }`;
  injectStyleSheet(css);
  Reveal.configure({ margin: 0.0 });
}

// export the plugin
const Plugin = {
  id: "print",

  init: (deck) => {
    Reveal = deck;

    return new Promise(function (resolve) {
      Reveal.addEventListener("ready", fixFooters);
      Reveal.addEventListener("ready", setHeight);
      if (Reveal.getConfig().checkOverflow) {
        Reveal.addEventListener("slidechanged", checkHeight);
      }

      /* are we exporting a PDF? */
      var pdf = !!window.location.search.match(/print-pdf/gi);
      if (pdf) {
        setupIframes();
        setupVideos();
        setupTitle();
        setupMargin();

        // automatically press the print button when not in headless mode
        if (!navigator.webdriver && !Decker.isElectron()) {
          Reveal.addEventListener("pdf-ready", function () {
            setTimeout(window.print, 2000);
          });
        }
      }

      resolve();
    });
  },
};

export default Plugin;
