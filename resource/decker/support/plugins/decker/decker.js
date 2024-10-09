// store href *before* reveal modifies it (adds hash of title slide)
const deckPathname = location.pathname;

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);
const presenterStartup = /presenter/gi.test(window.location.search);

// view menu button
let pluginButton = undefined;

// Fix some decker-specific things when slides are loaded
function onStart(deck) {
  fixAutoplayWithStart();
  fixLinks();
  currentDate();
  prepareTaskLists();
  prepareFullscreenIframes();

  deck.addEventListener("ready", () => {
    if (!printMode) {
      totalSlides = deck.getTotalSlides();
      setTimeout(() => continueWhereYouLeftOff(deck), 500);
    }

    prepareFullscreenIframes();
    prepareFlashPanel(deck);
    preparePresenterMode(deck);

    const menuPlugin = deck.getPlugin("decker-menu");
    if (!!menuPlugin && !!menuPlugin.addPluginButton) {
      pluginButton = menuPlugin.addPluginButton(
        "decker-menu-presenter-button",
        "fa-chalkboard-teacher",
        localization.activate_presenter_mode,
        togglePresenterMode
      );
    }

    Decker.addPresenterModeListener(onPresenterMode);
    if (presenterStartup) {
      togglePresenterMode();
    }
  });
}

// Use Wake Lock API to prevent display from going to sleep, which would cause video recording to break.
// Make sure to re-request WakeLock when it get's accidentially released
let wakeLock = null;
async function requestWakeLock() {
  if ("wakeLock" in navigator) {
    try {
      wakeLock = await navigator.wakeLock.request("screen");
      if (wakeLock) {
        wakeLock.addEventListener("release", wakeLockReleased);
        document.addEventListener("visibilitychange", visibilityChanged);
        console.log("Enabled WakeLock, display will not go to sleep");
      }
    } catch (err) {
      console.error("Failed to request WakeLock, display may go to sleep");
    }
  }
}
async function releaseWakeLock() {
  if (wakeLock) {
    wakeLock.removeEventListener("release", wakeLockReleased);
    document.removeEventListener("visibilitychange", visibilityChanged);
    await wakeLock.release();
    wakeLock = null;
    console.log("Disabled WakeLock, display may go to sleep again");
  }
}
function wakeLockReleased() {
  console.log("WakeLock was released, re-request it");
  requestWakeLock();
}
function visibilityChanged() {
  if (wakeLock && document.visibilityState === "visible") {
    console.log("Document became visible again, re-request WakeLock");
    requestWakeLock();
  }
}

async function onPresenterMode(isActive) {
  if (isActive) {
    // show info message
    Decker.flash.message(localization.presenter_mode_on);

    // request wake lock: display cannot go to sleep
    requestWakeLock();
  } else {
    // show info message
    Decker.flash.message(localization.presenter_mode_off);

    // release wake lock, display may go to sleep again
    releaseWakeLock();
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
    if (!a.href) continue;
    // skip links in SVGs (e.g. MathJax)
    if (a.href.baseVal) continue;

    const url = new URL(a.href);

    // fix bibtex links
    if (url.hash && url.hash.startsWith("#ref-")) {
      // find linked element
      let e = document.getElementById(url.hash.substring(1));
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
    '.reveal ul>li>input[type="checkbox"][disabled]'
  )) {
    const li = cb.parentElement;
    li.classList.add(cb.checked ? "task-yes" : "task-no");
    const ul = li.parentElement;
    ul.classList.add("task-list");
  }
}

// wrap iframe demos in a div that offers a fullscreen button.
// only do this if the browser supports the Fullscreen API.
// don't do this for Safari, since its webkit-prefixed version
// does not work properly: one cannot put an iframe to fullscreen
// if the slides are in fullscreen already (which is the standard
// presentation setting).
// we wrap the div in any case to make the css simpler.
function prepareFullscreenIframes() {
  for (let iframe of document.querySelectorAll(
    ":not(.fs-container)>figure.iframe>iframe"
  )) {
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

let totalSlides;

function updateProgress(deck, event) {
  let slide;
  if (event && event.currentSlide) {
    slide = event.currentSlide;
  }
  // store current slide index in localStorage
  const slideIndex = deck.getIndices(slide);
  if (slideIndex && slideIndex.h != 0) {
    // store current slide index (h- and v-index and fragment)
    updateLastVisitedSlide(slideIndex);
    updatePercentage(slideIndex.h);
  }
}

function updateLastVisitedSlide(slideIndex) {
  localStorage.setItem(deckPathname, JSON.stringify(slideIndex));
}

function updatePercentage(horizontalIndex) {
  // store percentage of slides visited
  const idx = horizontalIndex + 1; // starts at 0
  const percent = Math.round((100.0 * idx) / totalSlides);
  const key = deckPathname + "-percentage";
  const percentBefore = localStorage.getItem(key);
  if (percent > percentBefore) {
    localStorage.setItem(key, percent);
  }
}

function continueWhereYouLeftOff(deck) {
  // if *-deck.html was opened on the title slide,
  // and if user has visited this slide decks before,
  // then ask user whether to jump to slide where he/she left off

  if (localStorage) {
    deck.addEventListener("slidechanged", (event) =>
      updateProgress(deck, event)
    );
    window.addEventListener("beforeunload", () => {
      if (deck.hasPlugin("explain")) {
        const explainPlugin = deck.getPlugin("explain");
        // if explain video is playing, stop it to switch to current slide
        if (explainPlugin.isVideoPlaying()) {
          explainPlugin.stopVideo();
        }
      }
    });
    // if we are on the first slide
    const slideIndex = deck.getIndices();
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
            deck.slide(storedIndex.h, storedIndex.v);
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
        deck.addEventListener("slidechanged", hideDialog);
      }
    }
  }
}

function prepareFlashPanel(deck) {
  let pending = [];
  let interval = null;

  // This is why this needs to run after Reveal is ready.
  let revealElement = deck.getRevealElement();
  let viewport = revealElement.parentElement;
  if (viewport) {
    let panelHtml = `
  <div class="decker-flash-panel">
    <div class="content" role="alert" aria-live="assertive"></div>
  </div>
  `;
    viewport.insertAdjacentHTML("beforeend", panelHtml);

    let panel = viewport.querySelector("div.decker-flash-panel");
    let content = viewport.querySelector("div.decker-flash-panel div.content");

    let update = (msg) => {
      if (msg) {
        // One more message.
        if (interval) {
          pending.push(msg);
        } else {
          interval = setInterval(update, 1000);
          content.innerHTML = msg;
          panel.classList.add("flashing");
        }
      } else {
        // Called by interval timer. No new message.
        if (pending.length != 0) {
          content.innerHTML = pending.shift();
        } else {
          clearInterval(interval);
          interval = null;
          content.innerHTML = "";
          panel.classList.remove("flashing");
        }
      }
    };

    Decker.flash = {
      message: update,
    };
  } else {
    console.error(
      "Element is missing: getRevealElement (This is seriously wrong)"
    );
    Decker.flash = {
      message: console.log,
    };
  }
}

let presenterMode = false;
let listeners = [];
let viewportElement = undefined;

function togglePresenterMode() {
  presenterMode = !presenterMode;

  if (presenterMode) {
    viewportElement.classList.add("presenter-mode");
    pluginButton.ariaPressed = true;
    pluginButton.setLabel(localization.deactivate_presenter_mode);
  } else {
    viewportElement.classList.remove("presenter-mode");
    pluginButton.ariaPressed = false;
    pluginButton.setLabel(localization.activate_presenter_mode);
  }

  for (let callback of listeners) {
    callback(presenterMode);
  }
}

// Setup the presenter mode toggle key binding and notification machinery.
function preparePresenterMode(deck) {
  if (!Decker)
    throw "Global Decker object is missing. This is seriously wrong.";

  // This is why this needs to run after Reveal is ready.
  viewportElement = deck.getViewportElement();
  let revealElement = deck.getRevealElement();
  if (!revealElement)
    throw "Reveal slide element is missing. This is seriously wrong.";

  Decker.addPresenterModeListener = (callback) => {
    listeners.push(callback);
  };

  Decker.removePresenterModeListener = (callback) => {
    listeners = listeners.filter((cb) => cb !== callback);
  };

  Decker.isPresenterMode = () => {
    return presenterMode;
  };

  /* prevent reload when in presenter mode */
  Decker.addReloadInhibitor(() => !Decker.isPresenterMode());

  deck.addKeyBinding(
    {
      keyCode: 80,
      key: "P",
      description: "Toggle Decker Presenter Mode (Triple Click)",
    },

    Decker.tripleClick(() => {
      if (deck.hasPlugin("handout")) {
        const handoutPlugin = deck.getPlugin("handout");
        if (handoutPlugin.isActive()) {
          return;
        }
      }
      togglePresenterMode();
    })
  );
}

const localization = {
  activate_presenter_mode: "Activate Presenter Mode (P,P,P)",
  deactivate_presenter_mode: "Deactivate Presenter Mode (P,P,P)",
  presenter_mode_on: `<span>Presenter Mode: <strong style="color:var(--accent3);">ON</strong></span>`,
  presenter_mode_off: `<span>Presenter Mode: <strong style="color:var(--accent1);">OFF</strong></span>`,
};

if (navigator.language === "de") {
  localization.activate_presenter_mode =
    "Präsentationsmodus anschalten (P,P,P)";
  localization.deactivate_presenter_mode =
    "Präsentationsmodus abschalten (P,P,P)";
  localization.presenter_mode_on = `<span>Präsentationsmodus: <strong style="color:var(--accent3);">AN</strong></span>`;
  localization.presenter_mode_off = `<span>Präsentationsmodus: <strong style="color:var(--accent1);">AUS</strong></span>`;
}

const Plugin = {
  id: "decker",
  init: (deck) => {
    return new Promise((resolve) => {
      onStart(deck);
      resolve();
    });
  },
  updatePercentage: updatePercentage,
  updateLastVisitedSlide: updateLastVisitedSlide,
};

export default Plugin;
