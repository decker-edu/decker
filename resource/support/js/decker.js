"use strict";

// store href *before* reveal modifies it (adds hash of title slide)
const deckPathname = location.pathname;
const deckHash = location.hash;

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);

if (typeof Reveal === "undefined") {
  console.error("decker.js has to be loaded after reveal.js");
} else {
  if (Reveal.isReady()) {
    deckerStart();
  } else {
    Reveal.addEventListener("ready", deckerStart);
  }
}

// Fix some decker-specific things after Reveal
// has been initialized
function deckerStart() {
  fixAutoplayWithStart();
  fixBibtexLinks();
  currentDate();
  addSourceCodeLabels();
  prepareTaskLists();
  prepareFullscreenIframes();
  if (Reveal.getConfig().verticalSlides) {
    setupVerticalSlides();
  }
  if (!printMode) {
    setTimeout(continueWhereYouLeftOff, 500);
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

function fixBibtexLinks() {
  for (let a of document.querySelectorAll("a")) {
    // skip links in SVGs (e.g. MathJax)
    if (a.href.baseVal) continue;

    let url = new URL(a.href);

    // skip external links
    if (url.origin != window.location.origin) continue;

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

function setupVerticalSlides() {
  const subsections = Array.from(document.getElementsByClassName("sub")).filter(
    (s) => s.nodeName === "SECTION"
  );
  const subsection_bundles = [];
  for (let i = 0; i < subsections.length; i++) {
    const subsection = subsections[i];
    const bundle = [subsection];
    var subtemp = subsection;
    while (
      i + 1 < subsections.length &&
      subtemp.nextElementSibling === subsections[i + 1]
    ) {
      i += 1;
      bundle.push(subsections[i]);
      subtemp = subtemp.nextElementSibling;
    }
    subsection_bundles.push(bundle);
  }

  for (let bundle of subsection_bundles) {
    const supersection = document.createElement("section");
    supersection.classList.add("slide");
    supersection.classList.add("level1");
    const section = bundle[0].previousElementSibling;
    section.parentNode.insertBefore(supersection, section);
    supersection.appendChild(section);
    for (let subsection of bundle) {
      supersection.appendChild(subsection);
    }
  }
  Reveal.sync();
  Reveal.setState(Reveal.getState());
}

function addSourceCodeLabels() {
  $("div.sourceCode[label]").each(function () {
    $("<div/>")
      .addClass("language-label")
      .text($(this).attr("label"))
      .prependTo($(this).children("pre"));
  });
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

// wrap iframe demos in a div that offers a fullscreen button.
// only do this if the browser supports the Fullscreen API.
// don't do this for Safari, since its webkit-prefixed version
// does not work propertly: one cannot put an iframe to fullscreen
// if the slides are in fullscreen already (which is the standard
// presentation setting).
// we wrap the div in any case to make the css simpler.
function prepareFullscreenIframes() {
  for (let iframe of document.querySelectorAll("iframe.decker")) {
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

function isElectron() {
  // Renderer process
  if (
    typeof window !== "undefined" &&
    typeof window.process === "object" &&
    window.process.type === "renderer"
  ) {
    return true;
  }

  // Main process
  if (
    typeof process !== "undefined" &&
    typeof process.versions === "object" &&
    !!process.versions.electron
  ) {
    return true;
  }

  // Detect the user agent when the `nodeIntegration` option is set to true
  if (
    typeof navigator === "object" &&
    typeof navigator.userAgent === "string" &&
    navigator.userAgent.indexOf("Electron") >= 0
  ) {
    return true;
  }

  return false;
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

// List of predicates that all must return true for a requested reload to
// actually be performed.
let reloadInhibitors = [];

// Adds a reload inhibitor.
function addReloadInhibitor(predicate) {
  reloadInhibitors.push(predicate);
}

// Removes a reload inhibitor.
function removeReloadInhibitor(predicate) {
  reloadInhibitors.splice(
    reloadInhibitors.find((p) => p === predicate),
    1
  );
}

// Opens a web socket connection and listens to reload requests from the server.
// If all of the registered inhibitors return true, the reload is performed.
function openReloadSocket() {
  if (location.hostname == "localhost" || location.hostname == "0.0.0.0") {
    var socket = new WebSocket("ws://" + location.host + "/reload");
    socket.onmessage = function (event) {
      if (event.data.startsWith("reload!")) {
        console.log("Reload requested.");
        let reload = reloadInhibitors.reduce((a, p) => a && p(), true);
        if (reload) {
          console.log("Reload authorized.");
          window.location.reload();
        } else {
          console.log("Reload inhibited.");
        }
      }
    };
  }
}

window.addEventListener("load", openReloadSocket);
