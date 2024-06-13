/*****************************************************************
 *
 *  A plugin for reveal.js to add slide annotations and a whiteboard.
 *
 *  Original version by Asvin Goel, goel@telematique.eu (based on v 0.6)
 *  Modified version by Mario Botsch, TU Dortmund
 *  Further contributions by Martin Heistermann, Bern University
 *
 *  License: MIT license
 *
 ******************************************************************/

// reference to Reveal object
let Reveal;

const STROKE_STEP_THRESHOLD = 2;

/************************************************************************
 ** Configuration options, global variables
 ************************************************************************/

// global config object from Reveal
let config;

// pen options
let penColors;
let penColor;
let penWidth;

// page dimensions
let pageHeight;
let pageWidth;

// Reveal HTML elements
let slides;
let slideZoom;

// different cursors used by whiteboard
let eraserCursor;
let eraserRadius = 10;
let laserCursor;
let penCursor;
let currentCursor;

// whether stroke is a laser-point-stroke
let isLaserStroke = false;

// canvas for dynamic cursor generation
let cursorCanvas = document.createElement("canvas");
cursorCanvas.id = "CursorCanvas";
cursorCanvas.width = 20;
cursorCanvas.height = 20;

// store which tools are active
const NOTHING = 0;
const PEN = 1;
const ERASER = 2;
const LASER = 3;
const MOVE = 4;
let tool = PEN;

// variable used to block leaving HTML page
let unsavedAnnotations = false;
let autosave = true;

// whether to automatically turn on whiteboard when pencil hovers
let autoToggle = false;

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);

// whiteboard canvas
let svg = null;
let stroke = null;
let points = null;

// global whiteboard status
let whiteboardActive = false;

// currently active fragment
let currentFragmentIndex = 0;

// here we save SVG snapshots for undo/redo
let undoHistory = [];
const undoBufferSize = 10;

// get configuration options from Reveal deck
function readConfig() {
  config = Reveal.getConfig().whiteboard || {};

  penColors = config.colors || [
    "var(--base07)",
    "var(--base06)",
    "var(--base05)",
    "var(--base04)",
    "var(--base03)",
    "var(--base02)",
    "var(--base01)",
    "var(--base00)",
    "var(--base08)",
    "var(--base09)",
    "var(--base0A)",
    "var(--base0B)",
    "var(--base0C)",
    "var(--base0D)",
    "var(--base0E)",
    "var(--base0F)",
  ];

  // reveal setting wrt slide dimension
  pageHeight = Reveal.getConfig().height;
  pageWidth = Reveal.getConfig().width;

  // reveal elements
  slides = document.querySelector(".reveal .slides");
  slideZoom = slides.style.zoom || 1;

  // autosave is on by default
  autosave = config.autosave || true;

  // autotoggle is off by default
  autoToggle = config.autotoggle || false;
}

/************************************************************************
 * Check browser features
 ************************************************************************/

const weHavePointerEvents = !!window.PointerEvent;
const weHaveCoalescedEvents = !!(
  window.PointerEvent && new PointerEvent("pointermove").getCoalescedEvents
);
let userShouldBeWarned = !weHavePointerEvents || !weHaveCoalescedEvents;
let userHasBeenWarned = false;

function warnUser() {
  if (!weHavePointerEvents) {
    alert(
      "Your browser does not support pointer events.\nWhiteboard will not work.\nBetter use Chrome/Chromium or Firefox."
    );
  } else if (!weHaveCoalescedEvents) {
    alert(
      "Your browser does not support coalesced pointer events.\nWhiteboard drawing might be laggy.\nBetter use Chrome/Chromium or Firefox."
    );
  }
  userHasBeenWarned = true;
}

/************************************************************************
 * Setup GUI
 ************************************************************************/

let buttons;
let buttonWhiteboard;
let buttonSave;
let buttonDownload;
let buttonGrid;
let buttonAdd;
let buttonUndo;
let buttonPen;
let buttonEraser;
let buttonLaser;
let colorPicker;
let hoverTimer;

function hidePanel() {
  buttons.classList.remove("visible");
  hideColorPicker();
}

function showPanel() {
  buttons.classList.add("visible");
  clearInterval(hoverTimer);
}

// function to generate a button
function createButton(classes, callback, active = false, tooltip) {
  let b = document.createElement("button");
  b.className = "fa-button whiteboard " + classes;
  b.onclick = callback;
  b.dataset.active = active;
  if (tooltip) b.title = tooltip;
  if (tooltip) b.setAttribute("aria-label", tooltip);
  buttons.appendChild(b);
  return b;
}

// setup all GUI elements
function createGUI() {
  buttons = document.createElement("div");
  buttons.id = "whiteboardButtons";
  buttons.classList.add("presenter-only");

  // handle hover visibility of panel
  buttons.onmouseenter = (evt) => {
    clearInterval(hoverTimer);
  };
  buttons.onmouseleave = (evt) => {
    hoverTimer = setInterval(hidePanel, 3000);
  };

  buttonDownload = createButton(
    "fas fa-download",
    saveAnnotations,
    false,
    "Manual Save"
  );

  buttonSave = createButton(
    "fas fa-save checkbox",
    toggleAutoSave,
    autosave,
    "Toggle Auto-Save"
  );
  buttonSave.setAttribute("role", "switch");

  buttonGrid = createButton(
    "fas fa-border-all checkbox",
    toggleGrid,
    false,
    "Toggle Grid"
  );
  buttonGrid.setAttribute("role", "switch");

  buttonAdd = createButton(
    "fas fa-plus",
    addWhiteboardPage,
    true,
    "Add Whiteboard Page"
  );

  buttonUndo = createButton("fas fa-undo", undo, false, "Undo");

  buttonPen = createButton(
    "fas fa-pen radiobutton",
    () => {
      if (!buttons.classList.contains("visible")) {
        showPanel();
        buttonPen.focus();
        return;
      }
      if (tool != PEN) {
        selectTool(PEN);
      } else {
        toggleColorPicker();
      }
    },
    false,
    "Select or change Pen"
  );
  buttonPen.setAttribute("role", "switch");
  buttonPen.style.position = "relative";

  buttonEraser = createButton(
    "fas fa-eraser radiobutton",
    () => {
      if (!buttons.classList.contains("visible")) {
        showPanel();
        buttonEraser.focus();
        return;
      }
      selectTool(ERASER);
    },
    false,
    "Pick Eraser"
  );
  buttonEraser.setAttribute("role", "switch");

  buttonLaser = createButton(
    "fas fa-magic radiobutton",
    () => {
      if (!buttons.classList.contains("visible")) {
        showPanel();
        buttonLaser.focus();
        return;
      }
      selectTool(LASER);
    },
    false,
    "Pick Laserpointer"
  );
  buttonLaser.setAttribute("role", "switch");

  buttonWhiteboard = createButton(
    "fas fa-edit checkbox",
    toggleWhiteboard,
    false,
    "Toggle Whiteboard Mode"
  );
  buttonWhiteboard.id = "whiteboardButton";

  if (Reveal.hasPlugin("ui-anchors")) {
    let anchors = Reveal.getPlugin("ui-anchors");
    anchors.placeButton(buttons, "BOTTOM_LEFT");
  }

  // generate color picker container
  colorPicker = document.createElement("div");
  colorPicker.id = "whiteboardColorPicker";
  buttonPen.appendChild(colorPicker);

  // color buttons
  penColors.forEach((color) => {
    let b = document.createElement("button");
    b.className = "fa-button whiteboard fas fa-circle";
    b.onclick = (evt) => {
      selectPenColor(color);
      evt.stopImmediatePropagation();
    };
    b.tooltip = color;
    b.setAttribute("aria-label", color);
    b.style.color = color;
    colorPicker.appendChild(b);
  });
  // pen radius buttons
  for (let r = 2; r < 17; r += 2) {
    const slideScale = Reveal.getScale();
    const radius = r * slideScale + "px";
    let b = document.createElement("button");
    b.className = "fa-button whiteboard fas fa-circle";
    b.style.fontSize = radius;
    b.onclick = (evt) => {
      selectPenRadius(r);
      evt.stopImmediatePropagation();
    };
    b.tooltip = radius;
    b.setAttribute("aria-label", radius);
    colorPicker.appendChild(b);
  }
}

/* Set slides to full height, such that they contain the full-height whiteboard.
 * For centered slides, also enforce full height, and wrap the slide content
 * in a flex-box to achieve vertical centering. This is necessary, since
 * Reveal's slide centering leads to slides that do not have full height,
 * which in turn do not allow for a full-height whiteboard.
 */
function setupSlides() {
  Reveal.getSlides().forEach(function (slide) {
    slide.style.height = pageHeight + "px";

    if (Reveal.getConfig().center || slide.classList.contains("center")) {
      // Reveal implements centering by adjusting css:top. Remove this.
      slide.style.top = "";
      // Make sure centered slides have class center.
      slide.classList.add("center");
    }
  });
}

// create whiteboard SVG for current slide
// is currently called for each slide, even if we don't have strokes yet
function setupSVG(slide, height) {
  // get slide
  if (!slide) slide = Reveal.getCurrentSlide();
  if (!slide) return;

  // does SVG exist already?
  svg = slide.querySelector("svg.whiteboard");
  if (svg) return svg;

  // otherwise, let's create the SVG
  svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.classList.add("whiteboard");
  svg.setAttribute("data-prevent-swipe", "");
  svg.setAttribute("preserveAspectRatio", "none");
  svg.style.pointerEvents = "none";
  svg.style.border = "1px solid transparent";

  // SVG dimensions
  svg.style.width = "100%";
  if (!height) height = pageHeight;
  svg.style.height = height + "px";

  // prevent accidential click, double-click, and context menu
  svg.oncontextmenu = killEvent;
  svg.onclick = killEvent;
  svg.ondblclick = killEvent;

  // add SVG to slide
  slide.appendChild(svg);

  // slide indices (used for saving)
  let idx = Reveal.getIndices(slide);
  svg.h = idx.h;
  svg.v = idx.v ? idx.v : 0;

  return svg;
}

/*
 * setup hidden SVG for defining grid pattern
 */
function createGridPattern() {
  let svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  svg.style.position = "absolute";
  svg.style.left = "0px";
  svg.style.top = "0px";
  svg.style.width = "10px";
  svg.style.height = "10px";
  svg.style.pointerEvents = "none";
  slides.insertBefore(svg, slides.firstChild);

  const h = Math.floor(Math.min(pageWidth, pageHeight) / 25);
  const rectWidth = pageWidth - 2;
  const rectHeight = pageHeight - 1;

  svg.innerHTML = `<defs>
      <pattern id="smallPattern" width="${h}" height="${h}" patternUnits="userSpaceOnUse">
          <path d="M ${h} 0 L 0 0 0 ${h}" fill="none" style="stroke:var(--base01); stroke-width:2px;"/>
      </pattern>
      <pattern id="gridPattern" width="${pageWidth}" height="${pageHeight}" patternUnits="userSpaceOnUse">
          <rect width="${rectWidth}" height="${rectHeight}" fill="url(#smallPattern)" style="stroke:var(--base02); stroke-width:3px;"/>
      </pattern>
  </defs>`;
}

/*
 * ignore this event, and don't propagate it further
 */
function killEvent(evt) {
  evt.preventDefault();
  evt.stopPropagation();
  return false;
}

/*****************************************************************
 * Internal GUI functions related to mouse cursor
 ******************************************************************/

function createLaserCursor() {
  cursorCanvas.width = 20;
  cursorCanvas.height = 20;
  let ctx = cursorCanvas.getContext("2d");

  // setup color gradient
  let col1 = "rgba(255, 100, 100, 1.0)";
  let col2 = "rgba(255, 0, 0, 0.9)";
  let col3 = "rgba(255, 0, 0, 0.7)";
  let col4 = "rgba(255, 0, 0, 0.0)";
  let grdLaser = ctx.createRadialGradient(10, 10, 1, 10, 10, 10);
  grdLaser.addColorStop(0.0, col1);
  grdLaser.addColorStop(0.3, col2);
  grdLaser.addColorStop(0.6, col3);
  grdLaser.addColorStop(1.0, col4);

  // render laser cursor
  ctx.clearRect(0, 0, 20, 20);
  ctx.fillStyle = grdLaser;
  ctx.fillRect(0, 0, 20, 20);
  laserCursor = "url(" + cursorCanvas.toDataURL() + ") 10 10, auto";
}

function createPenCursor() {
  let ctx = cursorCanvas.getContext("2d");

  // adjust canvas size and cursor radius using Reveal scale
  const slideScale = Reveal.getScale();
  const radius = Math.max(2, 0.5 * penWidth * slideScale);
  const width = radius * 2;
  cursorCanvas.width = width + 1;
  cursorCanvas.height = width + 1;

  // we cannot use penColor, since this might be contain a CSS variable
  // instead we have to evalute it
  let color = getComputedStyle(buttonPen).color;

  ctx.clearRect(0, 0, width, width);
  ctx.fillStyle = ctx.strokeStyle = color;
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.arc(radius, radius, radius, 0, 2 * Math.PI);
  ctx.fill();

  penCursor =
    "url(" + cursorCanvas.toDataURL() + ") " + radius + " " + radius + ", auto";
}

function createEraserCursor() {
  let ctx = cursorCanvas.getContext("2d");

  // (adjust canvas size and eraser radius using Reveal scale)
  const slideScale = Reveal.getScale();
  const radius = eraserRadius * slideScale;
  const width = 2 * radius;
  cursorCanvas.width = width;
  cursorCanvas.height = width;

  ctx.clearRect(0, 0, width, width);
  ctx.fillStyle = "rgba(255,255,255,0)";
  ctx.fillRect(0, 0, width, width);
  ctx.strokeStyle = "rgba(128, 128, 128, 0.8)";
  ctx.lineWidth = 2;
  ctx.beginPath();
  ctx.arc(radius, radius, radius - 2, 0, 2 * Math.PI);
  ctx.stroke();
  eraserCursor =
    "url(" + cursorCanvas.toDataURL() + ") " + radius + " " + radius + ", auto";
}

// select a cursor
function selectCursor(cur) {
  currentCursor = cur;
}

// show currently selected cursor
function showCursor(cur) {
  if (cur != undefined) selectCursor(cur);
  slides.style.cursor = currentCursor;
}

// hide cursor
function hideCursor() {
  slides.style.cursor = "none";
}

// hide cursor after 1 sec
let hideCursorTimeout;
function triggerHideCursor() {
  clearTimeout(hideCursorTimeout);
  hideCursorTimeout = setTimeout(hideCursor, 1000);
}

/*****************************************************************
 * Internal GUI functions (not called by user)
 ******************************************************************/

/*
 * select active tool and update buttons & cursor
 */
function selectTool(newTool) {
  tool = newTool;

  // update tool icons, update cursor
  buttonLaser.dataset.active =
    buttonEraser.dataset.active =
    buttonPen.dataset.active =
      false;
  buttonLaser.setAttribute("aria-checked", "false");
  buttonEraser.setAttribute("aria-checked", "false");
  buttonPen.setAttribute("aria-checked", "false");

  switch (tool) {
    case PEN:
      buttonPen.dataset.active = true;
      buttonPen.setAttribute("aria-checked", "true");
      buttonPen.style.color = penColor;
      selectCursor(penCursor);
      break;

    case ERASER:
      buttonEraser.dataset.active = true;
      buttonEraser.setAttribute("aria-checked", "true");
      selectCursor(eraserCursor);
      break;

    case LASER:
      buttonLaser.dataset.active = true;
      buttonLaser.setAttribute("aria-checked", "true");
      selectCursor(laserCursor);
      break;
  }

  hideColorPicker();
}

/*
 * switch between laser and pen
 */
function toggleLaser() {
  if (tool == LASER) selectTool(PEN);
  else selectTool(LASER);
}

/*
 * switch between eraser and pen
 */
function toggleEraser() {
  if (tool == ERASER) selectTool(PEN);
  else selectTool(ERASER);
}

function toggleColorPicker() {
  colorPicker.classList.toggle("active");
}

function hideColorPicker() {
  colorPicker.classList.remove("active");
}

function selectPenColor(col) {
  penColor = col;
  buttonPen.style.color = penColor;
  createPenCursor();
  selectCursor(penCursor);
  hideColorPicker();
}

function selectPenRadius(radius) {
  penWidth = radius;
  createPenCursor();
  selectCursor(penCursor);
  hideColorPicker();
}

function enableWhiteboard() {
  // do only prevent activation of whiteboard mode - deactivation should always be possible
  if (!Decker?.isPresenterMode?.()) return;
  if (userShouldBeWarned && !userHasBeenWarned) warnUser();

  whiteboardActive = true;
  clearTimeout(autoToggleTimer);

  // show scrollbar
  slides.classList.add("whiteboard-active");

  // show buttons
  buttons.classList.add("active");

  // activate SVG
  if (svg) {
    svg.style.border = "1px dashed lightgrey";
    svg.style.pointerEvents = "auto";
  }
}

function disableWhiteboard() {
  whiteboardActive = false;
  clearTimeout(autoToggleTimer);

  // hide scrollbar
  slides.classList.remove("whiteboard-active");

  // hide buttons
  buttons.classList.remove("active");
  hideColorPicker();

  // reset SVG
  if (svg) {
    svg.style.border = "1px solid transparent";
    svg.style.pointerEvents = "none";
  }

  // reset cursor
  clearTimeout(hideCursorTimeout);
  slides.style.cursor = "";
}

function toggleWhiteboard(state) {
  const activate = typeof state === "boolean" ? state : !whiteboardActive;
  if (activate) {
    enableWhiteboard();
  } else {
    disableWhiteboard();
  }
}

let autoToggleTimer;
function autoToggleOff(evt) {
  if (evt.pointerType == "pen") {
    if (whiteboardActive) {
      clearTimeout(autoToggleTimer);
      autoToggleTimer = setTimeout(disableWhiteboard, 2000);
    }
  }
}
function autoToggleOn(evt) {
  if (evt.pointerType == "pen") {
    clearTimeout(autoToggleTimer);
    if (!whiteboardActive) {
      enableWhiteboard();
    }
  }
}

// set unsavedAnnotations and update save icon
function needToSave(b) {
  unsavedAnnotations = b;
}

/*
 * add one page to whiteboard (only when drawing on back-board!)
 */
function addWhiteboardPage() {
  if (!svg) return;
  let boardHeight = svg.clientHeight;
  setWhiteboardHeight(boardHeight + pageHeight);
  slides.classList.add("animateScroll");
  slides.scrollTop = boardHeight;
}

function adjustWhiteboardHeight() {
  // hide grid, so that height computation only depends on strokes
  let rect = getGridRect();
  let display;
  if (rect) {
    display = rect.style.display;
    rect.style.display = "none";
  }

  // both SVG and slide (its parent) have to be visible
  // for the following svg.getBBox() to work.
  const slide = svg.parentElement;
  const slideDisplay = slide.style.display;
  const svgDisplay = svg.style.display;
  svg.style.display = "block";
  slide.style.display = "block";

  // get height of current annotations (w/o grid).
  const bbox = svg.getBBox();
  const scribbleHeight = bbox.y + bbox.height;
  // console.log("scribble height " + scribbleHeight);

  // restore previous visibility
  if (rect) rect.style.display = display;
  svg.style.display = svgDisplay;
  slide.style.display = slideDisplay;

  // rounding
  var height = pageHeight * Math.max(1, Math.ceil(scribbleHeight / pageHeight));
  setWhiteboardHeight(height);
}

function setWhiteboardHeight(svgHeight) {
  const needScrollbar = svgHeight > pageHeight;

  // set height of SVG
  svg.style.height = svgHeight + "px";

  // if grid exists, adjust its height
  let rect = getGridRect();
  if (rect) rect.setAttribute("height", svgHeight - pageHeight);

  // update scrollbar of slides container
  if (needScrollbar) slides.classList.add("needScrollbar");
  else slides.classList.remove("needScrollbar");

  // adjust with of slides container to accomodate scrollbar
  let currentWidth = slides.clientWidth;
  if (currentWidth != pageWidth) {
    const width = 2 * pageWidth - currentWidth;
    slides.style.width = width + "px";
  }

  // activate/deactivate pulsing border indicator
  if (needScrollbar) {
    if (!printMode) {
      // (re-)start border pulsing
      // (taken from https://css-tricks.com/restart-css-animation/)
      slides.classList.remove("pulseBorder");
      void slides.offsetWidth; // this does the magic!
      slides.classList.add("pulseBorder");
    }
  } else {
    slides.classList.remove("pulseBorder");
  }
}

/*****************************************************************
 * Public GUI functions that can be triggered by user
 ******************************************************************/

/*
 * User wants to clear current slide (mapped to key Delete)
 */
function clearSlide() {
  if (!whiteboardActive) return;
  if (confirm("Delete notes and board on this slide?")) {
    let strokes = svg.querySelectorAll("svg>path");
    if (strokes) {
      strokes.forEach((stroke) => {
        stroke.remove();
      });
      needToSave(true);
    }

    let grid = svg.querySelector("svg>rect");
    if (grid) {
      grid.remove();
      buttonGrid.dataset.active = false;
      needToSave(true);
    }

    setWhiteboardHeight(pageHeight);
  }
}

/*
 * Remove all laser strokes (called before saving annotations)
 */
function clearLaserStrokes() {
  document.querySelectorAll("svg.whiteboard>path.laser").forEach((stroke) => {
    stroke.remove();
  });
}

/*
 * return grid rect
 */
function getGridRect() {
  if (svg) return svg.querySelector("svg>rect");
}

/*
 * add/remove background rectangle with grid pattern
 */
function toggleGrid() {
  if (!svg) return;

  // if grid exists, remove it
  let rect = getGridRect();
  if (rect) {
    rect.remove();
    buttonGrid.dataset.active = false;
    buttonGrid.setAttribute("aria-checked", false);
  }

  // otherwise, add it
  else {
    const boardHeight = svg.clientHeight;

    // add large rect with this pattern
    let rect = document.createElementNS("http://www.w3.org/2000/svg", "rect");
    svg.insertBefore(rect, svg.firstChild);

    rect.setAttribute("x", 0);
    rect.setAttribute("y", pageHeight);
    rect.setAttribute("width", "100%");
    rect.setAttribute("height", Math.max(0, boardHeight - pageHeight));

    rect.style.fill = "url(#gridPattern)";
    rect.style.stroke = "none";
    rect.style.pointerEvents = "none";

    buttonGrid.dataset.active = true;
    buttonGrid.setAttribute("aria-checked", "true");
  }

  needToSave(true);
}

/*
 * enable/disable auto-saving annotations
 */
function toggleAutoSave() {
  autosave = !autosave;
  buttonSave.dataset.active = autosave;
  buttonSave.setAttribute("aria-checked", autosave);
  console.log("autosave: " + autosave);
}

/*****************************************************************
 ** Undo and re-do
 ******************************************************************/

function clearUndoHistory() {
  undoHistory = [];
  buttonUndo.dataset.active = false;
  buttonUndo.title = "undo";
}

function pushUndoHistory(action) {
  undoHistory.push({ action: action, svg: svg.innerHTML });
  buttonUndo.dataset.active = true;
  buttonUndo.title = "undo: " + action;
  if (undoHistory.length > undoBufferSize) undoHistory.shift();
}

function undo() {
  if (undoHistory.length) {
    svg.innerHTML = undoHistory.pop().svg;

    if (undoHistory.length) {
      let action = undoHistory[undoHistory.length - 1].action;
      buttonUndo.title = "undo: " + action;
      buttonUndo.dataset.active = true;
    } else {
      buttonUndo.dataset.active = false;
      buttonUndo.title = "undo";
    }

    needToSave(true);
  }
}

/*****************************************************************
 ** Load and save
 ******************************************************************/

/*
 * load scribbles from default URL
 * use Promise to ensure loading in init()
 */
function loadAnnotationsFromURL() {
  return new Promise(function (resolve) {
    // electron? try to load annotation from local file
    if (window.electronApp) {
      window.electronApp.loadAnnotation(annotationURL()).then((storage) => {
        if (storage) {
          parseAnnotations(storage);
          resolve();
          return;
        }
      });
    }

    // determine scribble filename
    let filename = annotationURL();

    // console.log("whiteboard load " + filename);
    let xhr = new XMLHttpRequest();

    xhr.onloadend = function () {
      if (xhr.status == 200 || xhr.status == 0) {
        try {
          const storage = JSON.parse(xhr.responseText);
          parseAnnotations(storage);
        } catch (err) {
          console.error("Cannot parse " + filename + ": " + err);
        }
      }
      // Don't dwell on it. 'Failed to load resource' is always shown anyways.
      // } else {
      //   console.warn("Failed to get file " + filename);
      // }

      resolve();
    };

    xhr.open("GET", filename, true);
    xhr.send();
  });
}

/*
 * load scribbles from local file
 * use Promise to ensure loading in init()
 */
function loadAnnotationsFromFile(filename) {
  if (window.File && window.FileReader && window.FileList) {
    var reader = new FileReader();

    reader.onload = function (evt) {
      try {
        const storage = JSON.parse(evt.target.result);
        parseAnnotations(storage);

        // re-setup current slide
        slideChanged();
      } catch (err) {
        console.error("Cannot parse " + filename + ": " + err);
      }
    };

    reader.readAsText(filename);
  } else {
    console.error("Your browser does not support the File API");
  }
}

/*
 * parse the annnotations blob loaded from URL or file
 */
function parseAnnotations(storage) {
  // create SVGs
  if (storage.whiteboardVersion && storage.whiteboardVersion >= 2) {
    storage.annotations.forEach((page) => {
      let slide = document.getElementById(page.slide);
      if (slide) {
        // use global SVG
        svg = setupSVG(slide);
        if (svg) {
          svg.innerHTML = page.svg;
          svg.style.display = "none";
        }
      }
    });
    // console.log("whiteboard loaded");
  }

  // fix inconsistency for centered slides
  if (storage.whiteboardVersion == 2 && Reveal.getConfig().center) {
    document.querySelectorAll("svg.whiteboard path").forEach((path) => {
      path.setAttribute("transform", "translate(0 40)");
    });
  }

  // adjust height for PDF export
  if (printMode) {
    slides.querySelectorAll("svg.whiteboard").forEach((mysvg) => {
      svg = mysvg;
      svg.style.display = "block";
      adjustWhiteboardHeight();
    });
  }
}

/*
 * filename for loading/saving annotations
 */
function annotationFilename() {
  let url = location.pathname;
  let basename = url.split("\\").pop().split("/").pop().split(".")[0];

  // decker filenames vs. Mario filenames
  let filename;
  if (basename.endsWith("-deck"))
    filename = basename.substring(0, basename.length - 5) + "-annot.json";
  else filename = basename + ".json";

  return filename;
}

/*
 * URL for loading/saving annotations
 */
function annotationURL() {
  let url = location.origin + location.pathname;
  let basename = url.substring(0, url.lastIndexOf("."));

  // decker filenames vs. Mario filenames
  let filename;
  if (basename.endsWith("-deck"))
    filename = basename.substring(0, basename.length - 5) + "-annot.json";
  else filename = basename + ".json";

  return filename;
}

/*
 * return annotations as JSON object
 */
function annotationData() {
  let storage = { whiteboardVersion: 3.0, annotations: [] };

  slides.querySelectorAll("svg.whiteboard").forEach((svg) => {
    if (svg.children.length) {
      storage.annotations.push({
        slide: svg.parentElement.id,
        svg: svg.innerHTML,
      });
    }
  });

  return storage;
}

/*
 * return annotations as Blob
 */
function annotationBlob() {
  return new Blob([JSON.stringify(annotationData())], {
    type: "application/json",
  });
}

/*
 * save annotations to decker server
 */
function saveAnnotations() {
  // clear remaining laser strokes
  clearLaserStrokes();

  // electron app? then save to file and return
  if (window.electronApp) {
    if (window.electronApp.saveAnnotation(annotationData(), annotationURL())) {
      console.log("whiteboard annotations saved to local file");
      needToSave(false);
    }
    return;
  }

  // also save to downloads folder (just to be save(r))
  let a = document.createElement("a");
  a.classList.add("whiteboard"); // otherwise a.click() is prevented/cancelled by global listener
  document.body.appendChild(a);
  try {
    a.download = annotationFilename();
    a.href = window.URL.createObjectURL(annotationBlob());
    a.click();
    needToSave(false);
  } catch (error) {
    console.error("whiteboard annotations could not be downloaded: " + error);
  }
  document.body.removeChild(a);

  // save to decker server (and do not return)
  let xhr = new XMLHttpRequest();
  xhr.open("put", annotationURL(), true);
  xhr.onloadend = function () {
    if (xhr.status == 200) {
      console.log("whiteboard annotations saved to deck directory");
      needToSave(false);
    } else {
      console.warn(
        "whiteboard annotation could not be save to decker, trying to download the file instead."
      );
    }
  };
  xhr.send(annotationBlob());
}

/*****************************************************************
 ** Geometry helper functions
 ******************************************************************/

// Euclidean distance between points p and q
function distance(p, q) {
  const dx = p[0] - q[0];
  const dy = p[1] - q[1];
  return Math.sqrt(dx * dx + dy * dy);
}

// compute midpoint between p0 and p1
function center(p0, p1) {
  return [0.5 * (p0[0] + p1[0]), 0.5 * (p0[1] + p1[1])];
}

// return string representation of point p (two decimal digits)
function printPoint(p) {
  return p[0].toFixed(1) + " " + p[1].toFixed(1);
}

// convert points to quadratic Bezier spline
function renderStroke(points, stroke) {
  const n = points.length;
  if (n < 2) {
    alert("fuck");
    return;
  }

  let path = "";
  let c;

  path += "M " + printPoint(points[0]);
  path += " L " + printPoint(center(points[0], points[1]));

  if (n > 2) {
    path += " Q ";
    for (let i = 1; i < points.length - 1; ++i) {
      c = center(points[i], points[i + 1]);
      path += " " + printPoint(points[i]) + " " + printPoint(c);
    }
  }

  path += " L " + printPoint(points[n - 1]);

  stroke.setAttribute("d", path);
}

// is point close enough to stroke to be counted as intersection?
function isPointInStroke(path, point) {
  if (!path.hasAttribute("d")) return;

  const length = path.getTotalLength();
  const precision = 10;
  let p;
  let d;

  for (let s = 0; s <= length; s += precision) {
    p = path.getPointAtLength(s);
    d = distance(point, [p.x, p.y]);
    if (d < precision) return true;
  }

  return false;
}

/*****************************************************************
 * GUI methods to start, continue, and stop a stroke
 * Are called from pointer/mouse callback
 * Call low-level drawing routines
 ******************************************************************/

/*
 * start a stroke:
 * compute mouse position, setup new stroke
 */
function startStroke(evt) {
  // console.log("start stroke");

  // don't start stroke if one is still active
  if (stroke) return;

  // mouse position
  const mouseX = evt.offsetX / slideZoom;
  const mouseY = evt.offsetY / slideZoom;

  // add stroke to SVG
  stroke = document.createElementNS("http://www.w3.org/2000/svg", "path");
  svg.appendChild(stroke);
  if (isLaserStroke) {
    stroke.classList.add("laser");
  } else {
    pushUndoHistory("paint stroke");
    stroke.style.stroke = penColor;
    stroke.style.strokeWidth = penWidth + "px";
  }

  // add point, convert to Bezier spline
  points = [
    [mouseX, mouseY],
    [mouseX, mouseY],
  ];
  renderStroke(points, stroke);

  // add fragment index to stroke
  if (currentFragmentIndex != undefined) {
    stroke.setAttribute("data-frag", currentFragmentIndex);
  }
}

/*
 * continue the active stroke:
 * append mouse point to active stroke
 */
function continueStroke(evt) {
  // we need an active stroke
  if (!stroke) return;

  // laser stroke?
  if (isLaserStroke) {
    if (!stroke.classList.contains("laser")) {
      stroke.classList.add("laser");
    }
  }

  // collect coalesced events
  let events = [evt];
  if (evt.getCoalescedEvents) events = evt.getCoalescedEvents() || events;

  // process events
  for (let evt of events) {
    if (evt.buttons > 0) {
      // mouse position
      const mouseX = evt.offsetX / slideZoom;
      const mouseY = evt.offsetY / slideZoom;

      const newPoint = [mouseX, mouseY];
      const oldPoint = points[points.length - 1];

      // only do something if mouse position changed and we are within bounds
      if (distance(newPoint, oldPoint) > STROKE_STEP_THRESHOLD)
        points.push(newPoint);
    }
  }

  // update svg stroke
  renderStroke(points, stroke);
}

/*
 * stop current stroke:
 */
function stopStroke(evt) {
  // we need an active stroke
  if (!stroke) return;

  // add timer to laser stroke
  if (isLaserStroke) {
    stroke.style.opacity = "0";
  }

  // reset stroke
  stroke = null;

  // new stroke -> we have to save
  needToSave(true);
}

/*
 * erase a stroke:
 * compute mouse position, compute "collision" with each stroke
 */
function eraseStroke(evt) {
  // mouse position
  const mouseX = evt.offsetX / slideZoom;
  const mouseY = evt.offsetY / slideZoom;
  const point = [mouseX, mouseY];

  svg.querySelectorAll("path").forEach((stroke) => {
    if (isPointInStroke(stroke, point)) {
      pushUndoHistory("erase stroke");
      stroke.remove();
      needToSave(true);
    }
  });
}

/*****************************************************************
 * pointer and mouse callbacks
 ******************************************************************/

function pointerMode(evt) {
  // target has to be SVG
  if (evt.target != svg) return NOTHING;

  // no buttons -> mouse move
  if (!evt.buttons) return MOVE;

  switch (evt.pointerType) {
    case "pen": {
      // eraser selected && pen on wacom (button 1) -> eraser
      if (tool == ERASER && evt.buttons == 1) return ERASER;
      // eraser tip on wacom -> eraser
      if (evt.buttons >= 4) return ERASER;
      // laser selected && pen on wacom (button 1) -> laser
      if (tool == LASER && evt.buttons == 1) return LASER;
      // pen selected && pen on wacom (button 1) && button 2 -> laser
      if (tool == PEN && evt.buttons == 3) return LASER;
      // pen selected && pen on wacom (button 1) -> pen
      if (tool == PEN && evt.buttons == 1) return PEN;
      break;
    }
    case "mouse": {
      // eraser selected && left mouse -> eraser
      if (tool == ERASER && evt.buttons == 1) return ERASER;
      // pen selected && middle mouse -> eraser
      if (tool == PEN && evt.buttons == 4) return ERASER;
      // laser selected && left mouse -> laser
      if (tool == LASER && evt.buttons == 1) return LASER;
      // pen selected && right mouse -> laser
      if (tool == PEN && evt.buttons == 2) return LASER;
      // pen selected && left mouse -> pen
      if (tool == PEN && evt.buttons == 1) return PEN;
      break;
    }
  }
  return NOTHING;
}

function pointerdown(evt) {
  // console.log("pointerdown");

  // only when whiteboard is active
  if (!whiteboardActive) return;

  switch (pointerMode(evt)) {
    case ERASER:
      if (evt.pointerType == "pen") {
        clearTimeout(hideCursorTimeout);
        hideCursor();
      } else {
        clearTimeout(hideCursorTimeout);
        showCursor(eraserCursor);
      }
      eraseStroke(evt);
      return killEvent(evt);

    case LASER:
      clearTimeout(hideCursorTimeout);
      hideCursor();
      isLaserStroke = true;
      startStroke(evt);
      return killEvent(evt);

    case PEN:
      clearTimeout(hideCursorTimeout);
      hideCursor();
      startStroke(evt);
      return killEvent(evt);
  }
}

function pointermove(evt) {
  // console.log("pointermove");

  // only when whiteboard is active
  if (!whiteboardActive) return;

  switch (pointerMode(evt)) {
    case MOVE:
      if (evt.pointerType == "pen") {
        clearTimeout(hideCursorTimeout);
        hideCursor();
      } else {
        showCursor();
        triggerHideCursor();
      }
      return;

    case ERASER:
      eraseStroke(evt);
      return killEvent(evt);

    case LASER:
      isLaserStroke = true;
      // user pressed pen button (pointerdown, but only mouse 2),
      // then pen touches wacom (no additional pointerdown)
      if (!stroke) startStroke(evt);
      // normal stroke continuation
      else continueStroke(evt);
      return killEvent(evt);

    case PEN:
      continueStroke(evt);
      return killEvent(evt);

    default:
      // user stopped laser stroke by lifting pen
      // while keeping pen button down (hence no pointerup)
      if (stroke && isLaserStroke) {
        pointerup(evt);
        return;
      }
  }
}

function pointerup(evt) {
  // console.log("pointerup");

  // only when whiteboard is active
  if (!whiteboardActive) return;
  // event has to happen for SVG
  if (evt.target != svg) return;
  // only pen and mouse events
  if (evt.pointerType != "pen" && evt.pointerType != "mouse") return;

  // finish pen stroke
  if (stroke) stopStroke(evt);

  // select cursor based on tool (NOT pointerMode!)
  switch (tool) {
    case PEN:
      selectCursor(penCursor);
      break;

    case LASER:
      selectCursor(laserCursor);
      break;

    case ERASER:
      selectCursor(eraserCursor);
      break;
  }

  // re-activate cursor hiding
  triggerHideCursor();

  // deactive laser stroke
  isLaserStroke = false;

  // don't propagate event any further
  return killEvent(evt);
}

/*****************************************************************
 * Setup event listeners
 ******************************************************************/

// when drawing, prevent touch events triggering clicks
// (e.g. menu icon, control arrows)
// only allow clicks for our (.whiteboard) buttons
function preventTouchClick(evt) {
  if (whiteboardActive && !evt.target.classList.contains("whiteboard")) {
    return killEvent(evt);
  }
}

// handle fragments
function fragmentChanged() {
  // hide pen dialog
  hideColorPicker();

  // determine current fragment index
  currentFragmentIndex = Reveal.getIndices().f;

  if (svg && currentFragmentIndex != undefined) {
    // adjust fragment visibility
    svg.querySelectorAll("svg>path[data-frag]").forEach((stroke) => {
      stroke.style.visibility =
        stroke.getAttribute("data-frag") > currentFragmentIndex &&
        stroke.getBBox().y < pageHeight
          ? "hidden"
          : "visible";
    });
  }
}

// prevent iPad pen to trigger scrolling (by killing touchstart
// whenever force is detected
function preventPenScroll(evt) {
  if (whiteboardActive && evt.targetTouches[0].force) {
    return killEvent(evt);
  }
}

// what to do when the slide changes
function slideChanged(evt) {
  if (!printMode) {
    // hide pen dialog
    hideColorPicker();

    // determine current fragment index
    currentFragmentIndex = Reveal.getIndices().f;

    // hide all SVG's
    slides.querySelectorAll("svg.whiteboard").forEach((svg) => {
      svg.style.display = "none";
    });

    // clear laser strokes from SVGs
    clearLaserStrokes();

    // setup and show current slide's SVG (adjust slide height before!)
    setupSVG();
    svg.style.display = "block";

    // activate/deactivate SVG
    toggleWhiteboard(whiteboardActive);

    // set height based on annotations
    adjustWhiteboardHeight();

    // setup slides container
    slides.classList.remove("animateScroll");
    slides.scrollTop = 0;
    if (svg.clientHeight > slides.clientHeight)
      slides.classList.add("needScrollbar");
    else slides.classList.remove("needScrollbar");

    // adjust fragment visibility
    fragmentChanged();

    // update SVG grid icon
    buttonGrid.dataset.active = !!svg && !!getGridRect();

    // clear undo history (updates icon)
    clearUndoHistory();

    // just to be sure, update slide zoom
    slideZoom = slides.style.zoom || 1;
  }
}

// register all event listeners to window and Reveal deck
function setupCallbacks() {
  // setup pointer events
  slides.addEventListener("pointerdown", pointerdown, true);
  slides.addEventListener("pointermove", pointermove);
  slides.addEventListener("pointerup", pointerup);
  slides.addEventListener("pointerout", pointerup);

  // autotoggle: turn on at pen-hover, turn off when stroke it stopped or pointer is lost
  if (autoToggle) {
    slides.addEventListener("pointerover", autoToggleOn);
    slides.addEventListener("pointerup", autoToggleOff);
    slides.addEventListener("pointerout", autoToggleOff);
  }

  // Intercept page leave when data is not saved
  window.addEventListener("beforeunload", function (evt) {
    if (unsavedAnnotations) {
      if (autosave) {
        console.log("whiteboard: auto-saving annotations");
        saveAnnotations();
      } else {
        evt.preventDefault();
        evt.returnValue = "You have unsaved annotations";
        return evt.returnValue;
      }
    }
  });

  // when drawing, stop ANY context menu from being opened
  window.addEventListener(
    "contextmenu",
    function (evt) {
      if (whiteboardActive) {
        return killEvent(evt);
      }
    },
    true
  );

  window.addEventListener("touchstart", preventTouchClick, true);
  window.addEventListener("touchend", preventTouchClick, true);
  slides.addEventListener("touchstart", preventPenScroll);

  // bind to undo event (CTRL-Z or CMD-Z).
  // doesn't work with Reveal's key bindings,
  // due to CTRL/CMD modifier.
  window.addEventListener("keydown", function (evt) {
    if (
      (evt.ctrlKey || evt.metaKey) &&
      !evt.shiftKey &&
      String.fromCharCode(evt.which).toLowerCase() == "z"
    ) {
      undo();
      return killEvent(evt);
    }
  });

  // whenever slide changes, update slideIndices and redraw
  Reveal.addEventListener("ready", setupSlides);
  Reveal.addEventListener("ready", slideChanged);
  Reveal.addEventListener("slidechanged", slideChanged);

  // whenever fragment changes, update stroke visibility
  Reveal.addEventListener("fragmentshown", fragmentChanged);
  Reveal.addEventListener("fragmenthidden", fragmentChanged);

  // eraser cursor has to be updated on resize (i.e. scale change)
  Reveal.addEventListener("resize", () => {
    // hide pen dialog
    hideColorPicker();
    // size of eraser cursor has to be adjusted
    createEraserCursor();
    // slide zoom might change
    slideZoom = slides.style.zoom || 1;
  });
}

/*****************************************************************
 * Setup key bindings
 ******************************************************************/

function setupKeyBindings() {
  Reveal.addKeyBinding(
    { keyCode: 46, key: "Delete", description: "Whiteboard: Clear Slide" },
    clearSlide
  );

  Reveal.addKeyBinding(
    { keyCode: 76, key: "L", description: "Whiteboard: Toggle laser pointer" },
    toggleLaser
  );

  Reveal.addKeyBinding(
    { keyCode: 69, key: "E", description: "Whiteboard: Toggle eraser" },
    toggleEraser
  );

  Reveal.addKeyBinding(
    { keyCode: 87, key: "W", description: "Whiteboard: Toggle on/off" },
    toggleWhiteboard
  );

  // Bind colors to 7 keys, first one is foreground color
  for (let i = 0; i < 7; i++) {
    Reveal.addKeyBinding(
      {
        keyCode: 49 + i,
        key: String.fromCharCode(49 + i),
        description: `Whiteboard: Color ${i}`,
      },
      () => {
        selectPenColor(penColors[i === 0 ? 0 : i + 8]);
      }
    );
  }

  Reveal.addKeyBinding(
    { keyCode: 56, key: "8", description: "Whiteboard: Pen radius 2" },
    () => {
      selectPenRadius(2);
    }
  );

  Reveal.addKeyBinding(
    { keyCode: 57, key: "9", description: "Whiteboard: Pen radius 4" },
    () => {
      selectPenRadius(4);
    }
  );

  Reveal.addKeyBinding(
    { keyCode: 48, key: "0", description: "Whiteboard: Pen radius 6" },
    () => {
      selectPenRadius(6);
    }
  );
}

/*****************************************************************
 * Export the plugin
 ******************************************************************/

const Plugin = {
  id: "whiteboard",

  init: (deck) => {
    // console.log("initialize whiteboard");

    // store reference to Reveal deck
    Reveal = deck;

    // read configuration options
    readConfig();

    // generate GUI
    createGUI();
    createGridPattern();
    setupCallbacks();
    setupKeyBindings();

    // generate cursors
    createLaserCursor();
    createEraserCursor();
    createPenCursor();

    // set default state
    toggleWhiteboard(false);
    selectTool(PEN);
    selectPenColor(penColors[0]);
    selectPenRadius(2);

    // load annotations
    return new Promise((resolve) => loadAnnotationsFromURL().then(resolve));
  },

  saveAnnotations: saveAnnotations,
};

export default Plugin;
