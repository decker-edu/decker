// Reveal.js uses CSS zoom when available (e.g. on Chrome).
// This messes up mouse coordinates for Chart.js.
// They don't want to include the fix in their core, but
// propose to implement it as a plugin:
// https://github.com/chartjs/Chart.js/issues/7178#issuecomment-770432120
// This here is the plugin.

const CSSZoomPlugin = {
  id: "csszoom",

  beforeEvent: function (chart, args) {
    let zoom = 1;
    for (let elem = chart.canvas; elem; elem = elem.parentElement) {
      zoom *= elem.style.zoom || 1;
    }
    // console.log("zoom = " + zoom);
    if (zoom != 1) {
      let event = args.event;
      event.x = event.x / zoom;
      event.y = event.y / zoom;
    }
  },
};

// export default CSSZoomPlugin;
Chart.register(CSSZoomPlugin);
