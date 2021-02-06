'use strict';

// Reveal.js uses CSS zoom when available (e.g. on Chrome).
// This messes up mouse coordinates for Chart.js.
// They don't want to include the fix in their core, but
// propose to implement it as a plugin:
// https://github.com/chartjs/Chart.js/issues/7178#issuecomment-770432120
// This here is the plugin.

let CSSZoomPlugin = {
	id: 'csszoom',

    beforeEvent: function(chart, ctx) {
        let zoom = 1;
        for (var elem=chart.canvas; elem; elem=elem.parentElement) {
            zoom *= elem.style.zoom || 1;
        }
        if (zoom != 1) {
            const offsetX = ctx.native.x - ctx.x;
            const offsetY = ctx.native.y - ctx.y;
            ctx.x = ctx.native.x / zoom - offsetX;
            ctx.y = ctx.native.y / zoom - offsetY;
        }
    }
};

Chart.plugins.register(CSSZoomPlugin);