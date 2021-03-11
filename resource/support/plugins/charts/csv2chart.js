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

var RevealChart = window.RevealChart || (function(){
    function parseJSON(str) {
        var json;
        try {
            json = JSON.parse(str);
        } catch (e) {
            var idx = Reveal.getIndices();
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
	      if ( obj1[p].constructor==Object && obj2[p].constructor==Object ) {
	        obj1[p] = mergeRecursive(obj1[p], obj2[p]);
	
	      } else {
	        obj1[p] = obj2[p];
	
	      }
	
	    } catch(e) {
	      // Property in destination object not set; create it and set its value.
	      obj1[p] = obj2[p];
	
	    }
	  }
	
	  return obj1;
	}


	function createChart(canvas, CSV, comments) {
		canvas.chart = null;
		var ctx = canvas.getContext("2d");
		var chartOptions = { responsive: true };
		var chartData = { labels: null, datasets: []};
		if ( comments !== null ) for (var j = 0; j < comments.length; j++ ){
			comments[j] = comments[j].replace(/<!--/,'');
			comments[j] = comments[j].replace(/-->/,'');
			var config = parseJSON(comments[j]);
			if ( config ) {
				if ( config.data ) {
					mergeRecursive( chartData, config.data);
				}
				if ( config.options ) {
					mergeRecursive( chartOptions, config.options);
				}
			}
		}

        // MARIO: set title
        if (canvas.hasAttribute("data-title")) {
            chartOptions.title = { display: true, text: canvas.getAttribute("data-title") };
        }
		
		// MARIO: set width & height -> maintainAspectRatio=false
		if (canvas.parentElement.style.width && canvas.parentElement.style.height) {
			chartOptions.maintainAspectRatio=false;
		}

		var lines = CSV.split('\n').filter(function(v){return v!==''});
		// if labels are not defined, get them from first line
		if ( chartData.labels === null && lines.length > 0 ) {
			chartData.labels = lines[0].split(',');

            // MARIO: remove whitespace around labels
            for (var i=0; i<chartData.labels.length; i++)
                chartData.labels[i] = chartData.labels[i].trim();

			// MARIO: do NOT remove first element
            // chartData.labels.shift();
			lines.shift();
		} 
		// get data values
		for (var j = 0; j < lines.length; j++ ){
			if (chartData.datasets.length <= j) chartData.datasets[j] = {};
			chartData.datasets[j].data =  lines[j].split(','); //.filter(function(v){return v!==''});
			chartData.datasets[j].label = chartData.datasets[j].data[0];
			chartData.datasets[j].data.shift();
			for (var k = 0; k < chartData.datasets[j].data.length; k++ ){
				chartData.datasets[j].data[k] = Number(chartData.datasets[j].data[k]);
			}
		}

		// add chart options
		var config = chartConfig[canvas.getAttribute("data-chart")];
		if ( config ) {
			for (var j = 0; j < chartData.datasets.length; j++ ){
				for (var attrname in config) {
					if ( !chartData.datasets[j][attrname]  ) {
						chartData.datasets[j][attrname] = config[attrname][j%config[attrname].length];  
					}
				}
			}
		}		

		canvas.chart = new Chart(ctx, { type: canvas.getAttribute("data-chart"), data: chartData, options: chartOptions }); 
	}

	var initializeCharts = function(){

        // MARIO: extract chart data from pre.code elements and convert to canvas
		var types     = [ "bar", "horizontalBar", "line", "radar", "doughnut", "pie", "polarArea" ];
        var classes   = types.map( s => (s+"-chart").toLowerCase() );
        var selectors = classes.map( s => "pre."+s ).join();
    
		var charts = document.querySelectorAll(selectors);
		for (var i = 0; i < charts.length; i++ ){
            var chart = charts[i];

            // MARIO: create enclosing parent (for setting size)
            var container = document.createElement('div');
			container.classList.add("chart-container");
			container.style.position = "relative";
			container.style.margin = "auto";

            // create canvas element
            var canvas = document.createElement('canvas');

            // determine chart type
            var type;
            for (var j=0; j<classes.length; j++) {
                if (chart.classList.contains(classes[j])) {
                    type = types[j];
                    break;
                }
            }
            canvas.setAttribute("data-chart", type);

            // MARIO: title
            if (chart.hasAttribute("title")) {
                canvas.setAttribute("data-title", chart.getAttribute("title"));
            }

            // MARIO: width and height
			if (chart.hasAttribute("width")) {
				container.style.width = chart.getAttribute("width");
			}
			if (chart.hasAttribute("height")) {
				container.style.height = chart.getAttribute("height");
			}

            // copy chart definition to canvas
            var content = chart.firstChild.innerText; // don't use innerHTML, it's escaped
            canvas.innerHTML = content;

            // replace pre element by canvas element
            var parent = chart.parentElement;
            parent.insertBefore(container, chart);
			container.appendChild(canvas);
            parent.removeChild(chart);
        }

		// Get all canvases
		var canvases = document.querySelectorAll("canvas");
		for (var i = 0; i < canvases.length; i++ ){
			// check if canvas has data-chart attribute
			if ( canvases[i].hasAttribute("data-chart") ){
				var CSV = canvases[i].innerHTML.trim();
				var comments = CSV.match(/<!--[\s\S]*?-->/g);
				CSV = CSV.replace(/<!--[\s\S]*?-->/g,'').replace(/^\s*\n/gm, "") 
				if ( ! canvases[i].hasAttribute("data-chart-src") ) {
					createChart(canvases[i], CSV, comments);
				}
				else {
					var canvas = canvases[i];
					var xhr = new XMLHttpRequest();
					xhr.onload = function() {
						if (xhr.readyState === 4) {
							createChart(canvas, xhr.responseText, comments);
						}
						else {
							console.warn( 'Failed to get file ' + canvas.getAttribute("data-chart-src") +". ReadyState: " + xhr.readyState + ", Status: " + xhr.status);
						}
					};

					xhr.open( 'GET', canvas.getAttribute("data-chart-src"), false );
					try {
						xhr.send();
					}
					catch ( error ) {
						console.warn( 'Failed to get file ' + canvas.getAttribute("data-chart-src") + '. Make sure that the presentation and the file are served by a HTTP server and the file can be found there. ' + error );
					}
				}

			} 
		}
	}

	function recreateChart(canvas) {
		var config = canvas.chart.config;

        // MARIO: inject correct pixel ratio, since global setting will not be used on *recreation*
        config.options.devicePixelRatio = pixelRatio;

		canvas.chart.destroy();
		setTimeout( function() { canvas.chart = new Chart(canvas, config); }, 500); // wait for slide transition
	}

	// check if chart option is given or not
	var chartConfig = Reveal.getConfig().chart || {};

	// set global chart options
	var config = chartConfig["defaults"];
	if ( config ) {
		mergeRecursive(Chart.defaults, config);
	}		


    // are we in PDF export mode?
    var printMode = ( /print-pdf/gi ).test( window.location.search );


    // MARIO: when Reveal's scale is >1, i.e., when it is enlarging the slides
    // increase the pixelRatio for canvas elements to get crisper results.
    function adjustPixelRatio() 
    {
        if (printMode)
            pixelRatio = 2;
        else if (Reveal.getScale() > 1)
            pixelRatio = Math.max(window.devicePixelRatio, 2);
        else
            pixelRatio = window.devicePixelRatio;

        // set default pixel ratio. this one is used for *initially* creating
        // charts, *not* for recreateChart. 
        Chart.defaults.global.devicePixelRatio = pixelRatio;
    }


	Reveal.addEventListener('ready', function(){

        // MARIO: when in print mode, set animation duration to zero
        // otherwise we might get half-ready charts in exported PDF
        if (printMode)
            Chart.defaults.global.animation = false;

        adjustPixelRatio();
		initializeCharts();

		Reveal.addEventListener('slidechanged', function(){
			var canvases = Reveal.getCurrentSlide().querySelectorAll("canvas[data-chart]");
			for (var i = 0; i < canvases.length; i++ ){
				if ( canvases[i].chart && canvases[i].chart.config.options.animation ) { 
					recreateChart( canvases[i] );
				}
			}
		
		});

        Reveal.addEventListener('resize', adjustPixelRatio);
	});
})();
