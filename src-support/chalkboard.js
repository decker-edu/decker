/*****************************************************************
 ** Author: Asvin Goel, goel@telematique.eu
 **
 ** A plugin for reveal.js adding a chalkboard.
 **
 ** Version: 0.3--0.6
 **
 ** License: MIT license (see LICENSE.md)
 **
 ** Credits:
 ** Chalkboard effect by Mohamed Moustafa https://github.com/mmoustafa/Chalkboard
 ******************************************************************/
var homeImg = document.getElementById('home');
homeImg.src = homeIcon;

var RevealChalkboard = window.RevealChalkboard || (function(){

    var path = scriptPath();
	function scriptPath()
    {
		// obtain plugin path from the script element
		var src;
		if (document.currentScript) {
			src = document.currentScript.src;
		} else {
			var sel = document.querySelector('script[src$="/chalkboard.js"]')
			if (sel) {
				src = sel.src;
			}
		}

		var path = typeof src === undefined ? src
			: src.slice(0, src.lastIndexOf("/") + 1);
		return path;
    }


    /*****************************************************************
     ** Configuration
     ******************************************************************/
	var config = Reveal.getConfig().chalkboard || {};

    // default values
    var background = [ 'rgba(0,0,0,0)' ,
                       'rgba(255,255,255,1)' ];
    //var pen        = [ 'url(' + path + 'img/boardmarker.png), auto',
					   //'url(' + path + 'img/boardmarker.png), auto' ];
    var pen        = [ 'none', 'none' ];
	var draw       = [ drawOnSlide ,
                       drawOnBoard ];
	var color      = [ 'rgba(255,0,0,0.5)',
                       'rgba(0,0,0,1)' ];

    // handle css zoom
	var slides = document.querySelector( '.reveal .slides' );
    var zoom   = slides.style.zoom;

    // setup laser
    var laserRadius  = 17;
    var laser = document.createElement( 'img' );
    laser.src              = path + 'img/laser.png';
    laser.id               = "laser";
    laser.style.visibility = "hidden";
    laser.style.position   = "absolute";
    laser.style.zIndex     = 40;
    document.querySelector(".reveal").appendChild( laser );

    // setup eraser
    var eraserRadius = 15;
    var eraserCursor = 'url("' + path + 'img/sponge.png") 25 20, auto';
    var eraseMode = false;

    // user config
    if ( config.background )  background = config.background;
    if ( config.pen )         pen = config.pen;
    if ( config.draw )        draw = config.draw;
    if ( config.color )       color = config.color;

    var toggleChalkboardButton = config.toggleChalkboardButton == undefined ? true : config.toggleChalkboardButton;
    var toggleNotesButton = config.toggleNotesButton == undefined ? true : config.toggleNotesButton;



    /*****************************************************************
     ** Setup
     ******************************************************************/

	function whenReady( callback )
    {
		// wait for drawings to be loaded and markdown to be parsed
		if ( loaded == null || document.querySelector('section[data-markdown]:not([data-markdown-parsed])') ) {
			setTimeout( whenReady, 100, callback )
		}
		else {
			callback();
		}
	}



	if ( toggleChalkboardButton )
    {
		var buttonC = document.createElement( 'div' );
		buttonC.id             = "toggle-chalkboard";
		buttonC.style.position = "absolute";
		buttonC.style.zIndex   = 40;
		buttonC.style.fontSize = "16px";
		buttonC.style.left     = "8px";
		buttonC.style.bottom   = "8px";
		buttonC.style.top      = "auto";
		buttonC.style.right    = "auto";
		buttonC.style.padding  = "3px";
		buttonC.style.borderRadius = "3px";
        buttonC.style.color    = "lightgrey";
		buttonC.innerHTML      = '<i onclick="RevealChalkboard.toggleChalkboard()" class="fas fa-edit"></i>';
		document.querySelector(".reveal").appendChild( buttonC );
	}

	if ( toggleNotesButton )
    {
		var buttonN = document.createElement( 'div' );
		buttonN.id             = "toggle-notes";
		buttonN.style.position = "absolute";
		buttonN.style.zIndex   = 40;
		buttonN.style.fontSize = "16px";
		buttonN.style.left     = "40px";
		buttonN.style.bottom   = "8px";
		buttonN.style.top      = "auto";
		buttonN.style.right    = "auto";
		buttonN.style.padding  = "3px";
		buttonN.style.borderRadius = "3px";
        buttonN.style.color    = "lightgrey";
		buttonN.innerHTML      = '<i onclick="RevealChalkboard.toggleNotesCanvas()" class="fas fa-pencil-alt"></i>';
		document.querySelector(".reveal").appendChild( buttonN );
	}

	var drawingCanvas = [ {id: "notescanvas" }, {id: "chalkboard" } ];
	setupDrawingCanvas(0);
	setupDrawingCanvas(1);

	var mode = 0; // 0: draw on slides, 1: draw on whiteboard

	var mouseX = 0;
	var mouseY = 0;
	var xLast = null;
	var yLast = null;

	var slideIndices =  { h:0, v:0 };
    var event = null;


    // generate one of the two canvases
	function setupDrawingCanvas( id )
    {
        // size of slides
		var width  = Reveal.getConfig().width;
		var height = Reveal.getConfig().height;

        // create div container
		var container = document.createElement( 'div' );
		container.classList.add( 'overlay' );
		container.setAttribute( 'data-prevent-swipe', '' );
		container.oncontextmenu = function() { return false; }
        container.style.background = background[id];
		container.id = drawingCanvas[id].id;

        // create canvas
        var canvas = document.createElement( 'canvas' );
		canvas.style.width  = width;
		canvas.style.height = height;
        canvas.width  = width;
		canvas.height = height;
		canvas.setAttribute( 'data-chalkboard', id );
        canvas.style.cursor = pen[ id ];
		container.appendChild( canvas );

        // store relevant information
        drawingCanvas[id].canvas    = canvas;
        drawingCanvas[id].context   = canvas.getContext("2d");
        drawingCanvas[id].container = container;
        drawingCanvas[id].width     = canvas.width;
        drawingCanvas[id].height    = canvas.height;


		if ( id == "0" )
        {
			container.style.zIndex = "34";
			container.classList.add( 'visible' )
			container.style.pointerEvents = "none";
		}
		else
        {
			container.style.zIndex = "36";
            canvas.style.border    = "1px solid " + color[1];
		}


        // add div to reveal.slides
		document.querySelector( '.reveal .slides' ).appendChild( container );
	}


    /*****************************************************************
     ** Storage
     ******************************************************************/
	var storage = [
		{ width: drawingCanvas[0].width,
          height: drawingCanvas[0].height,
          data: []},
		{ width: drawingCanvas[1].width,
          height: drawingCanvas[1].height,
          data: []}
	];


	var loaded = null;
	if ( config.src != null )
    {
		loadData( config.src );
	}


	/**
	 * Load data.
	 */
	function loadData( filename )
    {
        var xhr = new XMLHttpRequest();
		xhr.onload = function()
        {
			if (xhr.readyState === 4) {
				storage = JSON.parse(xhr.responseText);
				for (var id = 0; id < storage.length; id++)
                {
					if ( drawingCanvas[id].width != storage[id].width || drawingCanvas[id].height != storage[id].height )
                    {
                        console.warn("chalkboard: loaded data does not match width/height");
					}
				}
			}
			else
            {
				console.warn( 'Failed to get file ' + filename +". ReadyState: " + xhr.readyState + ", Status: " + xhr.status);
			}
            loaded = true;
		};

        xhr.open( 'GET', filename, true );
		try {
			xhr.send();
		}
		catch ( error ) {
			console.warn( 'Failed to get file ' + filename + '. Make sure that the presentation and the file are served by a HTTP server and the file can be found there. ' + error );
		}
	}

	/**
	 * Download data.
	 */
	function downloadData()
    {
		var a = document.createElement('a');
		document.body.appendChild(a);
		try {
			var url = location.pathname;
            var basename = (url.split('\\').pop().split('/').pop().split('.'))[0];
            var filename = basename + ".json";
            a.download = filename;
			var blob = new Blob( [ JSON.stringify( storage ) ], { type: "application/json"} );
			a.href = window.URL.createObjectURL( blob );
		} catch( error ) {
			a.innerHTML += " (" + error + ")";
		}
		a.click();
		document.body.removeChild(a);

        needSave = false;
	}


	/**
	 * Returns data object for the slide with the given indices.
	 */
	function getSlideData( indices, id )
    {
		if ( id == undefined ) id = mode;
		if (!indices) indices = slideIndices;
		for (var i = 0; i < storage[id].data.length; i++)
        {
			if (storage[id].data[i].slide.h === indices.h &&
                storage[id].data[i].slide.v === indices.v &&
                storage[id].data[i].slide.f === indices.f )
            {
				return storage[id].data[i];
			}
		}

        // no data found -> add it
		storage[id].data.push( { slide: indices, events: [] } );
		return storage[id].data[storage[id].data.length-1];
	}


    // do we have slide data?
	function hasSlideData( indices, id )
    {
		if ( id == undefined ) id = mode;
		if (!indices) indices = slideIndices;
		for (var i = 0; i < storage[id].data.length; i++)
        {
			if (storage[id].data[i].slide.h === indices.h &&
                storage[id].data[i].slide.v === indices.v &&
                storage[id].data[i].slide.f === indices.f )
            {
				return storage[id].data[i].events.length > 0;
			}
		}
		return false;
    }



    /*****************************************************************
     ** Intercept page leave when data is not saved
     ******************************************************************/
    var needSave = false;
    window.onbeforeunload = function(e)
    {
        if (needSave) return "blabla";
    }



    /*****************************************************************
     ** Print
     ******************************************************************/
	var printMode = ( /print-pdf/gi ).test( window.location.search );

	function createPrintout( )
    {
        // MARIO: we want to print the drawings
		//drawingCanvas[0].container.classList.remove( 'visible' ); // do not print notes canvas

        var nextSlide = [];
        var width   = Reveal.getConfig().width;
        var height  = Reveal.getConfig().height;

        // collect next-slides for all slides with board stuff
        for (var i = 0; i < storage[1].data.length; i++)
        {
            var h = storage[1].data[i].slide.h;
            var v = storage[1].data[i].slide.v;
            var f = storage[1].data[i].slide.f;
            var slide = f ? Reveal.getSlide(h,v,f) : Reveal.getSlide(h,v);
            nextSlide.push( slide.nextSibling );
        }

        // go through board storage, paint image, insert slide
        for (var i = 0; i < storage[1].data.length; i++)
        {
            var h = storage[1].data[i].slide.h;
            var v = storage[1].data[i].slide.v;
            var f = storage[1].data[i].slide.f;
            var slide = f ? Reveal.getSlide(h,v,f) : Reveal.getSlide(h,v);

            var slideData = getSlideData( storage[1].data[i].slide, 1 );

            var parent = Reveal.getSlide( storage[1].data[i].slide.h, storage[1].data[i].slide.v ).parentElement;

            var imgCanvas = document.createElement('canvas');
            imgCanvas.width  = width;
            imgCanvas.height = height;

            var imgCtx = imgCanvas.getContext("2d");
            imgCtx.fillStyle = "white";
            color[1] = "black";
            imgCtx.rect(0,0,imgCanvas.width,imgCanvas.height);
            imgCtx.fill();

            for (var j = 0; j < slideData.events.length; j++)
            {
                switch ( slideData.events[j].type )
                {
                    case "draw":
                        for (var k = 1; k < slideData.events[j].curve.length; k++) {
                            draw[1]( imgCtx,
                                slideData.events[j].curve[k-1].x,
                                slideData.events[j].curve[k-1].y,
                                slideData.events[j].curve[k].x,
                                slideData.events[j].curve[k].y
                            );
                        }
                        break;

                    case "erase":
                        for (var k = 0; k < slideData.events[j].curve.length; k++) {
                            erase( imgCtx,
                                slideData.events[j].curve[k].x,
                                slideData.events[j].curve[k].y
                            );
                        }
                        break;

                    case "clear":
                        addPrintout( parent, nextSlide[i], imgCanvas );
                        imgCtx.clearRect(0,0,imgCanvas.width,imgCanvas.height);
                        imgCtx.fill();
                        break;

                    default:
                        break;
                }
            }

            if ( slideData.events.length )
            {
                addPrintout( parent, nextSlide[i], imgCanvas );
            }
        }
        Reveal.sync();
    }


	function addPrintout( parent, nextSlide, imgCanvas )
    {
		var slideCanvas = document.createElement('canvas');
		slideCanvas.width = Reveal.getConfig().width;
		slideCanvas.height = Reveal.getConfig().height;
		var ctx = slideCanvas.getContext("2d");
		ctx.fillStyle = "white";
		ctx.rect(0,0,slideCanvas.width,slideCanvas.height);
		ctx.fill();
		ctx.drawImage(imgCanvas, 0, 0);

		var newSlide = document.createElement( 'section' );
		newSlide.classList.add( 'present' );
		newSlide.innerHTML = '<h1 style="visibility:hidden">Drawing</h1>';
		newSlide.setAttribute("data-background-size", '100% 100%' );
		newSlide.setAttribute("data-background-repeat", 'norepeat' );
		newSlide.setAttribute("data-background", 'url("' + slideCanvas.toDataURL("image/png") +'")' );
		if ( nextSlide != null ) {
			parent.insertBefore( newSlide, nextSlide );
		}
		else {
			parent.append( newSlide );
		}
	}


    /*****************************************************************
     ** Drawings
     ******************************************************************/

	function drawOnSlide(context,fromX,fromY,toX,toY)
    {
        context.lineWidth   = 2;
        context.lineCap     = 'round';
        context.strokeStyle = color[0];
		context.beginPath();
  		context.moveTo(fromX, fromY);
  		context.lineTo(toX, toY);
        context.stroke();
	}

	function drawOnBoard(context,fromX,fromY,toX,toY)
    {
        context.lineWidth   = 2;
        context.lineCap     = 'round';
        context.strokeStyle = color[1];
		context.beginPath();
  		context.moveTo(fromX, fromY);
  		context.lineTo(toX, toY);
  		context.stroke();
	}

	function erase(context,x,y)
    {
		context.save();
		context.beginPath();
		context.arc(x, y, eraserRadius, 0, 2 * Math.PI, false);
		context.clip();
		context.clearRect(x-eraserRadius, y-eraserRadius, eraserRadius*2, eraserRadius*2);
		context.restore();
	}


    function showLaser(evt)
    {
        if (!event) // only when drawing not active
        {
            drawingCanvas[mode].canvas.style.cursor = "none";
            laser.style.left = (evt.pageX - laserRadius) +"px" ;
            laser.style.top  = (evt.pageY - laserRadius) +"px" ;
            laser.style.visibility = "visible";
        }
    }

    function hideLaser()
    {
        laser.style.visibility = "hidden";
    }


	/**
	 * Opens an overlay for the chalkboard.
	 */
    function showChalkboard()
    {
        laser.style.visibility = "hidden";
        drawingCanvas[1].container.classList.add( 'visible' );
        xLast = null;
        yLast = null;
        event = null;
        mode = 1;
    }


	/**
	 * Closes open chalkboard.
	 */
    function closeChalkboard()
    {
        laser.style.visibility = "hidden";
        drawingCanvas[1].container.classList.remove( 'visible' );
        xLast = null;
        yLast = null;
        event = null;
        mode = 0;
    }

	/**
	 * Clear current canvas.
	 */
	function clearCanvas( id )
    {
		drawingCanvas[id].context.clearRect(0,0,drawingCanvas[id].width,drawingCanvas[id].height);
	}


    /*****************************************************************
     ** record and play-back events
     ******************************************************************/

	function recordEvent( event )
    {
		var slideData = getSlideData();
        slideData.events.push(event);
        needSave = true;
	}


	function startPlayback( finalMode )
    {
        closeChalkboard();
        mode = 0;
        for ( var id = 0; id < 2; id++ )
        {
			clearCanvas( id );

            /* MARIO: don't just call getSlideData, since it pushed slide data when nothing is found
               which somehow inserts black slides for printing */
            if (hasSlideData( slideIndices, id ))
            {
			    var slideData = getSlideData( slideIndices, id );
			    var index = 0;
			    while ( index < slideData.events.length )
                {
				    playEvent( id, slideData.events[index] );
				    index++;
			    }
		    }
        }

        if ( finalMode != undefined )
        {
            mode = finalMode;
        }
		if( mode == 1 ) showChalkboard();
	};


	function playEvent( id, event )
    {
        switch ( event.type )
        {
            case "clear":
                clearCanvas( id );
                break;
            case "draw":
                drawCurve( id, event);
                break;
            case "erase":
                eraseCurve( id, event );
                break;
        }
    };


	function drawCurve( id, event )
    {
		var ctx = drawingCanvas[id].context;

		if  ( event.curve.length > 1 )
        {
			for (var i = 1; i < event.curve.length; i++)
            {
                draw[id](ctx,
                         event.curve[i-1].x,
                         event.curve[i-1].y,
                         event.curve[i].x,
                         event.curve[i].y);
			}
		}
        else
        {
            // we need to record and play single points (for math equations)
            draw[id](ctx,
                     event.curve[0].x,
                     event.curve[0].y,
                     event.curve[0].x,
                     event.curve[0].y);
        }
	};

	function eraseCurve( id, event )
    {
		if  ( event.curve.length > 1 )
        {
			var ctx = drawingCanvas[id].context;

			for (var i = 0; i < event.curve.length; i++)
            {
                erase(ctx, event.curve[i].x, event.curve[i].y);
			}
		}

	};

    /*****************************************************************
     ** User interface
     ******************************************************************/


    function startStroke(evt)
    {
        var ctx     = drawingCanvas[mode].context;

        mouseX = evt.offsetX;
        mouseY = evt.offsetY;

        // update css zoom for Chrome
        zoom = slides.style.zoom ? 1.0 / slides.style.zoom : 1.0;

        // compensate for css-zoom
        mouseX = mouseX * zoom;
        mouseY = mouseY * zoom;

        xLast  = mouseX;
        yLast  = mouseY;

        if (evt.buttons == 2)
        {
            drawingCanvas[mode].canvas.style.cursor = laserCursor;
        }
        else if ((evt.buttons == 4) || eraseMode)
        {
            event = { type: "erase", curve: [{ x: mouseX, y: mouseY }] };
            drawingCanvas[mode].canvas.style.cursor = eraserCursor;
            erase(ctx,mouseX,mouseY);
        }
        else
        {
            event = { type: "draw", curve: [{x: mouseX, y: mouseY}] };
            drawingCanvas[mode].canvas.style.cursor = pen[mode];
            draw[mode](ctx, xLast, yLast, mouseX,mouseY);
        }
    };



    function continueStroke( evt )
    {
        if (event)
        {
            mouseX = evt.offsetX;
            mouseY = evt.offsetY;

            // compensate for css-zoom
            mouseX = mouseX * zoom;
            mouseY = mouseY * zoom;

            var ctx     = drawingCanvas[mode].context;

            event.curve.push({x: mouseX, y: mouseY});

            if (mouseY < drawingCanvas[mode].height && mouseX < drawingCanvas[mode].width)
            {
                if ( event.type == "erase" )
                {
                    erase(ctx,mouseX,mouseY);
                }
                else
                {
                    draw[mode](ctx, xLast, yLast, mouseX, mouseY);
                }
                xLast = mouseX;
                yLast = mouseY;
            }
        }
    };


    function stopStroke( evt )
    {
        if (event)
        {
            recordEvent( event );
            event = null;
        }

        // hide/reset cursor
        drawingCanvas[mode].canvas.style.cursor = pen[mode];
    };


    // setup callbacks
    if (window.PointerEvent)
    {
        console.log("we have pointer events");

        document.addEventListener( 'pointerdown', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                //console.log("pointerdown: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
                //console.log("ID: " + evt.pointerId);
                switch(evt.pointerType)
                {
                    case "mouse":
                    case "pen":
                        startStroke(evt);
                        break;

                    case "touch":
                        showLaser(evt);
                        break;
                }
            }
        }, true );


        document.addEventListener( 'pointermove', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                if (evt.buttons > 0)
                {
                    //console.log("pointermove: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
                    switch(evt.pointerType)
                    {
                        case "mouse":
                        case "pen":
                            continueStroke(evt);
                            break;

                        case "touch":
                            showLaser(evt);
                            break;
                    }
                }
            }
        });


        document.addEventListener( 'pointerup', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                //console.log("pointerup: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
                switch(evt.pointerType)
                {
                    case "mouse":
                    case "pen":
                        stopStroke(evt);
                        break;

                    case "touch":
                        hideLaser();
                        break;
                }
            }
        });

    }
    else
    {
        console.log("we do not have pointer events");

        document.addEventListener( 'mousedown', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                startStroke(evt);
            }
        }, true );


        document.addEventListener( 'mousemove', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                continueStroke(evt);
            }
        });


        document.addEventListener( 'mouseup', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                stopStroke(evt);
            }
        });

        document.addEventListener( 'touchstart', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                showLaser(evt);
            }
        }, true );

        document.addEventListener( 'touchmove', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                showLaser(evt);
            }
        });

        document.addEventListener( 'touchend', function(evt) {
            if (evt.target.getAttribute('data-chalkboard') == mode)
            {
                evt.preventDefault();
                hideLaser();
            }
        });
    }


    window.addEventListener( "resize", function() {
		// Resize the canvas and draw everything again
		startPlayback( mode );
	} );


	Reveal.addEventListener( 'ready', function( evt ) {
		if ( !printMode ) {
			slideIndices = Reveal.getIndices();
            startPlayback( 0 );
		}
		else {
			whenReady( createPrintout );
		}
	});


    Reveal.addEventListener( 'slidechanged', function( evt ) {
		if ( !printMode ) {
			slideIndices = Reveal.getIndices();
			closeChalkboard();
			clearCanvas( 0 );
			clearCanvas( 1 );
            startPlayback( 0 );
		}
	});


    Reveal.addEventListener( 'fragmentshown', function( evt ) {
		if ( !printMode ) {
			slideIndices = Reveal.getIndices();
			closeChalkboard();
			clearCanvas( 0 );
			clearCanvas( 1 );
            startPlayback( 0 );
		}
	});


    Reveal.addEventListener( 'fragmenthidden', function( evt ) {
		if ( !printMode ) {
			slideIndices = Reveal.getIndices();
			closeChalkboard();
			clearCanvas( 0 );
            clearCanvas( 1 );
            startPlayback();
            closeChalkboard();
		}
	});




    // check whether slide has blackboard scribbles, and then highlight icon
    function updateIcon()
    {
		if ( !printMode )
        {
		    var idx = Reveal.getIndices();
            if (hasSlideData(idx, 1))
            {
                buttonC.style.color    = "red";
            }
            else
            {
                buttonC.style.color    = "lightgrey";
            }
        }
    }
    Reveal.addEventListener( 'slidechanged',   updateIcon );
    Reveal.addEventListener( 'fragmentshown',  updateIcon );
    Reveal.addEventListener( 'fragmenthidden', updateIcon );



	function toggleNotesCanvas()
    {
        if ( mode == 1 )
        {
            toggleChalkboard();
        }

        if ( notescanvas.style.pointerEvents != "none" )
        {
            event = null;
            buttonN.style.color = "lightgrey";
            notescanvas.style.border = "none";
            notescanvas.style.pointerEvents = "none";
        }
        else
        {
            buttonN.style.color = "#2a9ddf";
            notescanvas.style.border = "1px solid " + color[0];
            notescanvas.style.pointerEvents = "auto";
        }
	};


	function toggleChalkboard()
    {
		if ( mode == 1 )
        {
			event = null;
			closeChalkboard();
            chalkboard.style.pointerEvents = "none";
            buttonC.style.color = "lightgrey";
            updateIcon();
		}
		else
        {
			showChalkboard();
            chalkboard.style.pointerEvents = "auto";
            buttonC.style.color = "#2a9ddf";
		}
	};


	function clear()
    {
        recordEvent( { type:"clear" } );
        clearCanvas( mode );
	};

	function resetSlide( force )
    {
		var ok = force || confirm("Please confirm to delete chalkboard drawings on this slide!");
		if ( ok )
        {
			event = null;
			closeChalkboard();

			clearCanvas( 0 );
			clearCanvas( 1 );

			mode = 1;
			var slideData = getSlideData();
			slideData.events = [];

			mode = 0;
			var slideData = getSlideData();
			slideData.events = [];
		}
	};

	function resetStorage( force )
    {
		var ok = force || confirm("Please confirm to delete all chalkboard drawings!");
		if ( ok )
        {
			clearCanvas( 0 );
			clearCanvas( 1 );
			if ( mode == 1 )
            {
				event = null;
				closeChalkboard();
			}
			storage = [
				{ width: drawingCanvas[0].width, height: drawingCanvas[0].height, data: []},
				{ width: drawingCanvas[1].width, height: drawingCanvas[1].height, data: []}
			];
		}
	};


    function drawUndo()
    {
        if (hasSlideData( slideIndices, mode ))
        {
            var slideData = getSlideData( slideIndices, mode );
            slideData.events.pop();
            startPlayback( mode );
        }
    }


	this.drawUndo          = drawUndo;
	this.toggleNotesCanvas = toggleNotesCanvas;
	this.toggleChalkboard  = toggleChalkboard;
	this.clear             = clear;
	this.reset             = resetSlide;
	this.resetAll          = resetStorage;
	this.download          = downloadData;

	return this;
})();
