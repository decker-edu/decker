import { hideFlyingFocus } from "../../flyingFocus/flying-focus.js";

// reference to Reveal object
let Reveal;

// store href *before* reveal modifies it (adds hash of title slide)
const deckPathname = location.pathname;
const deckHash = location.hash;

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);

// Fix some decker-specific things when slides are loaded
function onStart() {
  fixAutoplayWithStart();
  fixLinks();
  currentDate();
  prepareTaskLists();
  prepareFullscreenIframes();
  prepareCodeHighlighting();
  prepareFlyingFocusClearing();
}

// Fix some decker-specific things when Reveal is initilized
function onReady() {
  if (!printMode) {
    setTimeout(continueWhereYouLeftOff, 500);
  }
}

function fixAutoplayWithStart() {
  for (let vid of document.getElementsByTagName("video")) {
    vid.addEventListener("play", (e) => {
      const timeRegex = /#t=(\d+)/;
      const matches = e.target.currentSrc.match(timeRegex);
      if (matches !== null && matches.length > 0) {
        e.target.currentTime = matches[1];
      }
    });
  }
}

function fixLinks() {
  for (let a of document.querySelectorAll("a")) {
    // skip links in SVGs (e.g. MathJax)
    if (a.href.baseVal) continue;

    const url = new URL(a.href);

    // fix bibtex links
    if (url.hash && url.hash.startsWith("#/ref-")) {
      // find linked element
      let e = document.getElementById(url.hash.substring(2));
      if (e) {
        // find enclosing slide/section
        let s = e.closest("section");
        if (s && s.id) {
          // change hash to ID of section
          url.hash = "#" + s.id;
          a.href = url.href;
        }
      }
      continue;
    }

    // open all other links in new window/tab
    if (!a.target) {
      a.target = "_blank";
    }
  }
}

// Replace date string on title slide with current date
// if string provided for date in yaml header is "today"
function currentDate() {
  var date = document.querySelector(".date");
  if (!date) return;
  var dateString = date.textContent.trim();

  var today = new Date().toISOString().substr(0, 10);

  if (dateString === "today") {
    date.textContent = today;
  }
}

function prepareTaskLists() {
  for (let cb of document.querySelectorAll(
    '.reveal ul.task-list>li>input[type="checkbox"]'
  )) {
    var li = cb.parentElement;
    li.classList.add(cb.checked ? "task-yes" : "task-no");
  }
}

// wrap iframe demos in a div that offers a fullscreen button.
// only do this if the browser supports the Fullscreen API.
// don't do this for Safari, since its webkit-prefixed version
// does not work propertly: one cannot put an iframe to fullscreen
// if the slides are in fullscreen already (which is the standard
// presentation setting).
// we wrap the div in any case to make the css simpler.
function prepareFullscreenIframes() {
  for (let iframe of document.querySelectorAll("figure.iframe>iframe")) {
    // wrap div around iframe
    var parent = iframe.parentElement;
    var div = document.createElement("div");
    div.classList.add("fs-container");
    div.style.width = iframe.style.width || "100%";
    div.style.height = iframe.style.height || "100%";
    if (iframe.classList.contains("stretch")) {
      div.classList.add("stretch");
      iframe.classList.remove("stretch");
    }
    parent.insertBefore(div, iframe);
    div.appendChild(iframe);

    // iframe should be full width/height within div
    iframe.style.width = "100%";
    iframe.style.height = "100%";

    // if fullscreen API is not supported then don't add the button
    if (!div.requestFullscreen) continue;

    // add fullscreen button
    var btn = document.createElement("button");
    btn.classList.add("fs-button");
    btn.innerHTML =
      '<i class="fas fa-expand-arrows-alt" style="font-size:20px"></i>';
    div.btn = btn;
    div.appendChild(btn);

    // handle button click: enter/exit fullscreen
    btn.onclick = function () {
      var doc = window.document;
      var container = this.parentElement;
      if (doc.fullscreenElement == container) doc.exitFullscreen();
      else container.requestFullscreen();
    };

    // handle fullscreen change: adjust button icon
    div.onfullscreenchange = function () {
      var doc = window.document;
      this.btn.innerHTML =
        doc.fullscreenElement == this
          ? '<i class="fas fa-compress-arrows-alt"></i>'
          : '<i class="fas fa-expand-arrows-alt"></i>';
    };
  }
}

function prepareCodeHighlighting() {
  for (let code of document.querySelectorAll("pre>code")) {
    var pre = code.parentElement;

    // if line numbers to be highlighted are specified...
    if (pre.hasAttribute("data-line-numbers")) {
      // ...copy them from <pre> to <code>
      code.setAttribute(
        "data-line-numbers",
        pre.getAttribute("data-line-numbers")
      );
    }
    // otherwise, if we specified .line-numbers...
    else if (pre.classList.contains("line-numbers")) {
      // ...set empty attribute data-line-numbers,
      // so reveal adds line numbers w/o highlighting
      code.setAttribute("data-line-numbers", "");
    }

    // construct caption
    if (pre.hasAttribute("data-caption")) {
      var parent = pre.parentElement;
      var figure = document.createElement("figure");
      var caption = document.createElement("figcaption");
      var content = pre.getAttribute("data-caption");

      parent.insertBefore(figure, pre);
      figure.appendChild(pre);
      figure.appendChild(caption);
      caption.innerHTML = content.trim();
    }
  }
}

function createElement({
  type,
  id,
  classes,
  css,
  text,
  parent,
  onclick = null,
}) {
  let e = document.createElement(type);
  if (id) e.id = id;
  if (classes) e.className = classes;
  if (css) e.style = css;
  if (text) e.innerHTML = text;
  if (parent) parent.appendChild(e);
  if (onclick) e.addEventListener("click", onclick);
  return e;
}

function continueWhereYouLeftOff() {
  // if *-deck.html was opened on the title slide,
  // and if user has visited this slide decks before,
  // then ask user whether to jump to slide where he/she left off

  if (localStorage) {
    // if we are on the first slide
    const slideIndex = Reveal.getIndices();
    if (slideIndex && slideIndex.h == 0 && slideIndex.v == 0) {
      // ...and previous slide index is stored (and not title slide)
      const storedIndex = JSON.parse(localStorage.getItem(deckPathname));
      if (storedIndex && storedIndex.h != 0) {
        // ...ask to jump to that slide

        const slideNumber = storedIndex.h + 1;

        // German or non-German?
        const lang = document.documentElement.lang;
        const german = lang == "de";

        let reveal = document.querySelector(".reveal");

        let dialog = createElement({
          type: "div",
          id: "continue-dialog",
          css: "display:flex; justify-content:space-evenly; align-items:center; gap:1em; position:fixed; left:50%; bottom:1em; transform:translate(-50%,0px); padding:1em; border:2px solid #2a9ddf; border-radius: 0.5em; font-size: 1rem; z-index:50;",
          parent: reveal,
          text: german
            ? "Bei Folie " + slideNumber + " weitermachen?"
            : "Continue on slide " + slideNumber + "?",
        });

        let hideDialog = () => {
          dialog.style.display = "none";
        };

        let yes = createElement({
          type: "button",
          id: "continue-yes",
          parent: dialog,
          css: "font:inherit;",
          text: german ? "Ja" : "Yes",
          onclick: () => {
            Reveal.slide(storedIndex.h, storedIndex.v);
            hideDialog();
          },
        });

        let no = createElement({
          type: "button",
          id: "continue-no",
          parent: dialog,
          css: "font:inherit;",
          text: german ? "Nein" : "No",
          onclick: hideDialog,
        });

        // hide dialog after 5sec or on slide change
        setTimeout(hideDialog, 5000);
        Reveal.addEventListener("slidechanged", hideDialog);
      }
    }

    // add hook to store current slide's index
    window.addEventListener("beforeunload", () => {
      // if explain video is playing, stop it to switch to current slide
      if (Reveal.hasPlugin("explain")) {
        const explainPlugin = Reveal.getPlugin("explain");
        if (explainPlugin.isVideoPlaying()) {
          explainPlugin.stopVideo();
        }
      }
      // store current slide index in localStorage
      const slideIndex = Reveal.getIndices();
      if (slideIndex && slideIndex.h != 0) {
        localStorage.setItem(deckPathname, JSON.stringify(slideIndex));
      }
    });
  }
}

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

function prepareFlyingFocusClearing() {
  Reveal.on("slidechanged", event => {
    hideFlyingFocus();
  })
}

const Plugin = {
  id: "decker",
  init: (deck) => {
    Reveal = deck;
    return new Promise(function (resolve) {
      onStart();
      Reveal.addEventListener("ready", onReady);
      resolve();
    });
  },
};

export default Plugin;
