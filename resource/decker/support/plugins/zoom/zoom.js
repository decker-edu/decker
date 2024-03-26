//
// Based on zoom.js plugin of reveal.js
// modified to correctly handle reveal's scaling
//          to react on dblclick
//          to remove (unused) panning code
//

/*!
 * zoom.js 0.3 (modified for use with reveal.js)
 * http://lab.hakim.se/zoom-js
 * MIT licensed
 *
 * Copyright (C) 2011-2014 Hakim El Hattab, http://hakim.se
 */

// The current zoom level (scale)
let zoomLevel = 1;

// maximum allowed zoom factor
let maxZoomLevel = 2;

/**
 * Applies the CSS required to zoom in
 *
 * @param {Object} rect
 * @param {Number} scale
 */
function magnify(rect, scale) {
  // Reset
  if (scale === 1) {
    document.body.style.transform = "";
  }

  // Scale
  else {
    // Ensure a width/height is set
    rect.width = rect.width || 1;
    rect.height = rect.height || 1;

    // Center the rect within the zoomed viewport
    rect.x *= scale;
    rect.y *= scale;
    rect.x -= (window.innerWidth - rect.width * scale) / 2;
    rect.y -= (window.innerHeight - rect.height * scale) / 2;

    document.body.style.transformOrigin = "0px 0px";
    document.body.style.transform =
      "translate(" + -rect.x + "px," + -rect.y + "px) scale(" + scale + ")";
  }

  zoomLevel = scale;
}

/**
 * Zooms in on an HTML element.
 *
 * @param element: HTML element to zoom in on
 */
function zoomTo(element) {
  // Due to an implementation limitation we can't zoom in
  // to another element without zooming out first
  if (zoomLevel !== 1) {
    zoomOut();
  } else {
    // Space around the zoomed in element to leave on screen
    const padding = 5;
    const bounds = element.getBoundingClientRect();

    // are slides zoomed up, and is this done using CSS zoom?
    // then incorporate this zoom!
    const currentZoom = document.querySelector(".reveal .slides").style.zoom;
    const currentScale = currentZoom < 1 ? 1 : currentZoom;

    const rect = {
      x: Math.round(bounds.left * currentScale - padding),
      y: Math.round(bounds.top * currentScale - padding),
      width: Math.round(bounds.width * currentScale + padding * 2),
      height: Math.round(bounds.height * currentScale + padding * 2),
    };

    const scale = Math.max(
      Math.min(
        window.innerWidth / rect.width,
        window.innerHeight / rect.height,
        maxZoomLevel
      ),
      1
    );

    if (scale > 1) {
      magnify(rect, scale);
    }
  }
}

// zoom out to normal scale
function zoomOut() {
  magnify({ x: 0, y: 0 }, 1);
}

const Plugin = {
  id: "zoom",

  init: function (reveal) {
    // The easing that will be applied when we zoom in/out
    document.body.style.transition = "transform 0.8s ease";

    // read config
    const config = Decker.meta.zoom;
    const trigger = config?.trigger || "doubleClick";
    maxZoomLevel = config?.max || 100;

    const triggerZoom = (event) => {
      // which element to zoom to
      let element = event.target;

      // is it a part of an SVG (or MathJax formula)? then zoom to the SVG
      const svg = element.closest("svg");
      if (svg) element = svg;

      zoomTo(element);

      // stop default behavior (select line/paragraph)
      event.preventDefault();
      event.stopPropagation();
      return false;
    };

    if (trigger == "tripleClick") {
      reveal.getSlidesElement().addEventListener("mousedown", function (event) {
        if (event.detail === 3) triggerZoom(event);
      });
    } else if (trigger == "altClick") {
      reveal.getSlidesElement().addEventListener("mousedown", function (event) {
        if (event.altKey) triggerZoom(event);
      });
    } else if (trigger == "doubleClick") {
      reveal.getSlidesElement().addEventListener("dblclick", function (event) {
        triggerZoom(event);
      });
    } else {
      console.error("Zoom plugin: wrong options");
    }

    reveal.addEventListener("slidechanged", function () {
      if (zoomLevel !== 1) {
        zoomOut();
      }
    });
  },
};

export default Plugin;
