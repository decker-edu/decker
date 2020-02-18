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



var RevealZoom = window.RevealZoom || (function(){


	// The current zoom level (scale)
	var level = 1;


	// The easing that will be applied when we zoom in/out
	document.body.style.transition = 'transform 0.8s ease';
	document.body.style.OTransition = '-o-transform 0.8s ease';
	document.body.style.msTransition = '-ms-transform 0.8s ease';
	document.body.style.MozTransition = '-moz-transform 0.8s ease';
	document.body.style.WebkitTransition = '-webkit-transform 0.8s ease';


	/**
	 * Applies the CSS required to zoom in, prefers the use of CSS3
	 * transforms but falls back on zoom for IE.
	 *
	 * @param {Object} rect
	 * @param {Number} scale
	 */
	function magnify( rect, scale ) {

		// Ensure a width/height is set
		rect.width = rect.width || 1;
		rect.height = rect.height || 1;

		// Center the rect within the zoomed viewport
		rect.x -= ( window.innerWidth - ( rect.width * scale ) ) / 2;
		rect.y -= ( window.innerHeight - ( rect.height * scale ) ) / 2;

		// Reset
		if( scale === 1 )
        {
			document.body.style.transform = '';
			document.body.style.OTransform = '';
			document.body.style.msTransform = '';
			document.body.style.MozTransform = '';
			document.body.style.WebkitTransform = '';
		}

		// Scale
		else
        {
			var origin = '0px 0px';
            var transform = 'translate('+ -rect.x +'px,'+ -rect.y +'px) scale('+ scale +')';

			document.body.style.transformOrigin = origin;
			document.body.style.OTransformOrigin = origin;
			document.body.style.msTransformOrigin = origin;
			document.body.style.MozTransformOrigin = origin;
			document.body.style.WebkitTransformOrigin = origin;

			document.body.style.transform = transform;
			document.body.style.OTransform = transform;
			document.body.style.msTransform = transform;
			document.body.style.MozTransform = transform;
			document.body.style.WebkitTransform = transform;
		}

		level = scale;
	}


    // zoom to element on double click
	document.querySelector( '.reveal .slides' ).addEventListener( 'dblclick', function( event ) {
		event.preventDefault();

        // which element to zoom to
        var element = event.target;

        // is it a part of an SVG (or MathJax formula)? then zoom to the SVG
        var svg = element.closest("svg");
        if (svg) element = svg;

        zoomTo(element);
	});


    // un-show answers on slide-change
    Reveal.addEventListener( 'slidechanged', function() {
		if( level !== 1 )
        {
			zoomOut();
		}
    });


	/**
	 * Zooms in on an HTML element.
	 *
	 * @param element: HTML element to zoom in on
	 */
	function zoomTo( element )
    {
		// Due to an implementation limitation we can't zoom in
		// to another element without zooming out first
		if( level !== 1 )
        {
			zoomOut();
		}
		else
        {
			// Space around the zoomed in element to leave on screen
			var padding = 5;
			var bounds  = element.getBoundingClientRect();

            // are slides zoomed up, and is this done using CSS zoom?
            // then incorporate this zoom!
            var zoom  = document.querySelector( '.reveal .slides' ).style.zoom;
            var scale = (zoom < 1) ? 1 : zoom;

            var options = {
                x:      Math.round( bounds.left   * scale - padding ),
                y:      Math.round( bounds.top    * scale - padding ),
			    width:  Math.round( bounds.width  * scale + ( padding * 2 ) ),
			    height: Math.round( bounds.height * scale + ( padding * 2 ) )
            };


            options.scale = Math.max( Math.min( window.innerWidth / options.width, window.innerHeight / options.height ), 1 );


			if( options.scale > 1 )
            {
				options.x *= options.scale;
				options.y *= options.scale;
				magnify( options, options.scale );
			}
		}
	}


    // zoom out to normal scale
	function zoomOut()
    {
		magnify( { x: 0, y: 0 }, 1 );
		level = 1;
	}


    return this;

})();

