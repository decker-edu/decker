/*****************************************************************
 ** Author: Asvin Goel, goel@telematique.eu
 **
 ** csv2chart.js is a plugin for reveal.js allowing to integrate
 ** Chart.js in reveal.js
 **
 ** Version: 0.2
 **
 ** License: MIT license (see LICENSE.md)
 **
 ******************************************************************/

// let chartjs properly handle Reveals zoom
import "./plugin-csszoom.js";

// reference to Reveal deck
let Reveal;

// chart config options
const printMode = /print-pdf/gi.test(window.location.search);
let chartConfig;
let pixelRatio = 1;

function parseJSON(str) {
  let json;
  try {
    json = JSON.parse(str);
  } catch (e) {
    let idx = Reveal.getIndices();
    console.log("chart error on slide " + idx.h + "." + idx.v + ":\n" + e);
    console.log(str);
    return null;
  }
  return json;
}

/*
 * Recursively merge properties of two objects
 */
function mergeRecursive(obj1, obj2) {
  for (var p in obj2) {
    try {
      // Property in destination object set; update its value.
      if (
        obj1[p] !== null &&
        typeof obj1[p] === "object" &&
        typeof obj2[p] === "object"
      ) {
        obj1[p] = mergeRecursive(obj1[p], obj2[p]);
      } else {
        obj1[p] = obj2[p];
      }
    } catch (e) {
      // Property in destination object not set; create it and set its value.
      obj1[p] = obj2[p];
    }
  }

  return obj1;
}

function createChart(canvas, CSV, comments) {
  canvas.chart = null;
  let ctx = canvas.getContext("2d");
  let chartOptions = { responsive: true };
  // let chartOptions = { responsive: false };
  let chartData = { labels: null, datasets: [] };
  if (comments !== null)
    for (let j = 0; j < comments.length; j++) {
      comments[j] = comments[j].replace(/<!--/, "");
      comments[j] = comments[j].replace(/-->/, "");
      let config = parseJSON(comments[j]);
      if (config) {
        if (config.data) {
          mergeRecursive(chartData, config.data);
        }
        if (config.options) {
          mergeRecursive(chartOptions, config.options);
        }
      }
    }

  // MARIO: set title
  if (canvas.hasAttribute("data-title")) {
    chartOptions.plugins = {
      title: {
        display: true,
        text: canvas.getAttribute("data-title"),
      },
    };
  }

  // MARIO: set pixel ratio
  chartOptions.devicePixelRatio = pixelRatio;

  // MARIO: set width & height -> maintainAspectRatio=false
  if (canvas.parentElement.style.width && canvas.parentElement.style.height) {
    chartOptions.maintainAspectRatio = false;
  }

  let lines = CSV.split("\n").filter(function (v) {
    return v !== "";
  });

  // if labels are not defined, get them from first line
  if (chartData.labels === null && lines.length > 0) {
    chartData.labels = lines[0].split(",");

    // MARIO: remove whitespace around labels
    for (let i = 0; i < chartData.labels.length; i++)
      chartData.labels[i] = chartData.labels[i].trim();

    // MARIO: do NOT remove first element
    // chartData.labels.shift();
    lines.shift();
  }

  // get data values
  for (let j = 0; j < lines.length; j++) {
    if (chartData.datasets.length <= j) chartData.datasets[j] = {};
    chartData.datasets[j].data = lines[j].split(","); //.filter(function(v){return v!==''});
    chartData.datasets[j].label = chartData.datasets[j].data[0];
    chartData.datasets[j].data.shift();
    for (let k = 0; k < chartData.datasets[j].data.length; k++) {
      chartData.datasets[j].data[k] = Number(chartData.datasets[j].data[k]);
    }
  }

  // add chart options
  let config = chartConfig[canvas.getAttribute("data-chart")];
  if (config) {
    for (let j = 0; j < chartData.datasets.length; j++) {
      for (let attrname in config) {
        if (!chartData.datasets[j][attrname]) {
          chartData.datasets[j][attrname] =
            config[attrname][j % config[attrname].length];
        }
      }
    }
  }

  // non-filled chart?
  if (canvas.hasAttribute("data-nofill")) {
    for (let j = 0; j < chartData.datasets.length; j++) {
      chartData.datasets[j].fill = false;
    }
  }
  // filled chart?
  if (canvas.hasAttribute("data-fill")) {
    for (let j = 0; j < chartData.datasets.length; j++) {
      chartData.datasets[j].fill = true;
    }
  }

  // horizontal bar charts?
  if (canvas.getAttribute("data-chart") == "horizontalBar") {
    canvas.setAttribute("data-chart", "bar");
    chartOptions.indexAxis = "y";
  }

  // log-scale on y-axis? (note: log-log doesn't work, since entries are always equally spaced on x-axis)
  if (canvas.hasAttribute("data-log")) {
    chartOptions.scales = {
      y: { type: "logarithmic" },
    };
  }

  canvas.chart = new Chart(ctx, {
    type: canvas.getAttribute("data-chart"),
    data: chartData,
    options: chartOptions,
  });
}

let initializeCharts = function () {
  // MARIO: extract chart data from pre.code elements and convert to canvas
  let types = [
    "bar",
    "horizontalBar",
    "line",
    "radar",
    "doughnut",
    "pie",
    "polarArea",
  ];
  // let classes = types.map((s) => (s + "-chart").toLowerCase());
  let classes = types.map((s) => s + "-chart");
  let selectors = classes.map((s) => "pre." + s).join();

  let charts = document.querySelectorAll(selectors);
  for (let i = 0; i < charts.length; i++) {
    let pre = charts[i];
    let fig = pre.parentElement;

    // MARIO: create enclosing parent (for setting size)
    let container = document.createElement("div");
    container.classList.add("chart-container");
    container.style.position = "relative";
    container.style.margin = "auto";

    // create canvas element
    let canvas = document.createElement("canvas");

    // determine chart type
    let type;
    for (let j = 0; j < classes.length; j++) {
      if (pre.classList.contains(classes[j])) {
        type = types[j];
        break;
      }
    }
    canvas.setAttribute("data-chart", type);

    // MARIO: title
    if (pre.hasAttribute("title")) {
      canvas.setAttribute("data-title", pre.getAttribute("title"));
    }

    // MARIO: width is defined on enclosing figure element
    if (fig.style.width && fig.style.width != "auto") {
      container.style.width = fig.style.width;
    }
    // MARIO: height is defined on pre element
    if (pre.style.height && pre.style.height != "auto") {
      container.style.height = pre.style.height;
    }

    // MARIO: empty (no-fill) chart
    if (pre.classList.contains("nofill")) {
      canvas.setAttribute("data-nofill", true);
    }

    // MARIO: filled chart
    if (pre.classList.contains("fill")) {
      canvas.setAttribute("data-fill", true);
    }

    // MARIO: log scale?
    if (pre.classList.contains("log")) {
      canvas.setAttribute("data-log", true);
    }

    // copy chart definition to canvas
    let content = pre.firstChild.innerText; // don't use innerHTML, it's escaped
    canvas.innerHTML = content;

    // replace pre element by canvas element
    let parent = pre.parentElement;
    parent.insertBefore(container, pre);
    container.appendChild(canvas);
    parent.removeChild(pre);
  }

  // Get all canvases
  let canvases = document.querySelectorAll("canvas");
  for (let i = 0; i < canvases.length; i++) {
    // check if canvas has data-chart attribute
    if (canvases[i].hasAttribute("data-chart")) {
      let CSV = canvases[i].innerHTML.trim();
      let comments = CSV.match(/<!--[\s\S]*?-->/g);
      CSV = CSV.replace(/<!--[\s\S]*?-->/g, "").replace(/^\s*\n/gm, "");
      if (!canvases[i].hasAttribute("data-chart-src")) {
        createChart(canvases[i], CSV, comments);
      } else {
        let canvas = canvases[i];
        let xhr = new XMLHttpRequest();
        xhr.onload = function () {
          if (xhr.readyState === 4) {
            createChart(canvas, xhr.responseText, comments);
          } else {
            console.warn(
              "Failed to get file " +
                canvas.getAttribute("data-chart-src") +
                ". ReadyState: " +
                xhr.readyState +
                ", Status: " +
                xhr.status
            );
          }
        };

        xhr.open("GET", canvas.getAttribute("data-chart-src"), false);
        try {
          xhr.send();
        } catch (error) {
          console.warn(
            "Failed to get file " +
              canvas.getAttribute("data-chart-src") +
              ". Make sure that the presentation and the file are served by a HTTP server and the file can be found there. " +
              error
          );
        }
      }
    }
  }
};

function recreateChart(canvas) {
  let config = canvas.chart.config;

  // MARIO: inject correct pixel ratio, since global setting will not be used on *recreation*
  config.options.devicePixelRatio = pixelRatio;

  canvas.chart.destroy();
  // setTimeout(function () {
  //   canvas.chart = new Chart(canvas, config);
  // }, 500); // wait for slide transition
  canvas.chart = new Chart(canvas, config);
}

// MARIO: when Reveal's scale is >1, i.e., when it is enlarging the slides
// increase the pixelRatio for canvas elements to get crisper results.
function adjustPixelRatio() {
  if (printMode) pixelRatio = 2;
  else if (Reveal.getScale() > 1)
    pixelRatio = Math.max(window.devicePixelRatio, 2);
  else pixelRatio = window.devicePixelRatio;

  // set default pixel ratio. this one is used for *initially* creating
  // charts, *not* for recreateChart.
  Chart.defaults.devicePixelRatio = pixelRatio;
}

const Plugin = {
  id: "charts",
  init: (deck) => {
    Reveal = deck;

    // check if chart option is given or not
    chartConfig = Reveal.getConfig().chart || {};

    // set global chart options
    let config = chartConfig["defaults"];
    if (config) {
      mergeRecursive(Chart.defaults, config);
    }

    // MARIO: disabled this, since it's more robust to
    // add a light background to charts in CSS
    // const colors =
    //   window.matchMedia &&
    //   window.matchMedia("(prefers-color-scheme: dark)").matches
    //     ? window.Decker.meta.palette.colors.dark
    //     : window.Decker.meta.palette.colors.light;
    // Chart.defaults.color = colors[7];
    // Chart.defaults.borderColor = colors[2];

    Reveal.addEventListener("ready", function () {
      // MARIO: when in print mode, set animation duration to zero
      // otherwise we might get half-ready charts in exported PDF
      if (printMode) Chart.defaults.animation = false;

      adjustPixelRatio();
      initializeCharts();

      Reveal.addEventListener("slidechanged", function () {
        let canvases =
          Reveal.getCurrentSlide().querySelectorAll("canvas[data-chart]");
        for (let i = 0; i < canvases.length; i++) {
          if (canvases[i].chart && canvases[i].chart.options.animation)
            recreateChart(canvases[i]);
        }
      });

      Reveal.addEventListener("resize", adjustPixelRatio);
    });
  },
};

export default Plugin;
