/*****************************************************************
 * 
 *  A plugin for reveal.js to add slide annotations and a whiteboard.
 * 
 *  Original version by Asvin Goel, goel@telematique.eu (based on v 0.6)
 *  Modified version by Mario Botsch, Bielefeld University
 *  Further contributions by Martin Heistermann, Bern University
 * 
 *  License: MIT license 
 * 
 ******************************************************************/


"use strict";


var RevealWhiteboard = (function(){

    var DEBUG = false;
    var LOCAL_STORAGE = false;


    /************************************************************************
     ** Tools
     ************************************************************************/

    /*
     * return path to this script
     */
    function scriptPath()
    {
        // obtain plugin path from the script element
        var src;
        if (document.currentScript) {
            src = document.currentScript.src;
        } else {
            var sel = document.querySelector('script[src$="/whiteboard.js"]')
            if (sel) {
                src = sel.src;
            }
        }

        var path = typeof src === undefined ? src
            : src.slice(0, src.lastIndexOf("/") + 1);
        return path;
    }




    /************************************************************************
     ** Configuration options, global variables
     ************************************************************************/

    var path = scriptPath();

    // default values or user configuration?
    var config     = Reveal.getConfig().whiteboard || {};
    var colors     = config.colors || [ "black", "red", "green", "blue", "yellow", "cyan", "magenta" ];
    var background = config.background || "white";

    // handle CSS zoom (Chrome), CSS scale (others), and highDPI/retina scale
    // (has to be updated later on, i.e., after reveal layout)
    var reveal      = document.querySelector( '.reveal' );
    var slides      = document.querySelector( '.reveal .slides' );
    var slideZoom   = slides.style.zoom || 1;
    var slideScale  = Reveal.getScale();
    var slideRect   = slides.getBoundingClientRect();
    var slideScroll = 0;
    var canvasScale = window.devicePixelRatio || 1;

    // different cursors used by whiteboard
    var eraserCursor;
    var eraserRadius = 10;
    var laserCursor;
    var penCursor;
    var currentCursor;
    var penColor  = "red";


    // setup light saber
    var lightsaber = document.createElement('div');
    var handle = document.createElement('label');
    var plasma = document.createElement('div');
    lightsaber.id = "lightsaber";
    plasma.classList.add("plasma");
    lightsaber.appendChild(handle);
    lightsaber.appendChild(plasma);
    document.body.appendChild(lightsaber);
    var laserOn  = new Audio(path+'/laserOn.mp3');
    var laserHum = new Audio(path+'/laserHum.mp3'); laserHum.loop=true;
    var laserOff = new Audio(path+'/laserOff.mp3');
    laserOn.onended  = function(){ laserHum.play(); }
    laserOff.onended = function(){ lightsaber.style.visibility = "hidden"; }
   

    // canvas for dynamic cursor generation
    var cursorCanvas = document.createElement( 'canvas' );
    cursorCanvas.id     = "CursorCanvas";
    cursorCanvas.width  = 20;
    cursorCanvas.height = 20;
    initCursors();

    // store which tools are active
    var boardMode = false;
    var ToolType = { NONE: 0, PEN: 1, ERASER: 2 };
    var tool = ToolType.NONE;
    var mode = 0; // 0: draw on slides, 1: draw on whiteboard
    var laser = false;

    // current stroke recording event data (type, coordinates, color)
    var activeStroke = null;

    // variable used to block leaving HTML page
    var needSave = false;

    // current slide's indices
    var slideIndices =  { h:0, v:0 };

    // is the user generating a PDF?
    var printMode = ( /print-pdf/gi ).test( window.location.search );




    /************************************************************************
     * Setup GUI
     ************************************************************************/

    // load css
    function loadCSS()
    {
        var head  = document.getElementsByTagName('head')[0];
        var link  = document.createElement('link');
        link.rel  = 'stylesheet';
        link.type = 'text/css';
        link.href = path + "/whiteboard.css";
        link.media = 'all';
        head.appendChild(link);
    }
    loadCSS();


    /*
     * create a button on the left side
     */
    function createButton(left, bottom, icon)
    {
        var b = document.createElement( 'div' );
        b.classList.add("whiteboard");
        b.style.position     = "absolute";
        b.style.zIndex       = 40;
        b.style.left         = left + "px";
        b.style.bottom       = bottom + "px";
        b.style.top          = "auto";
        b.style.right        = "auto";
        b.style.fontSize     = "16px";
        b.style.padding      = "3px";
        b.style.borderRadius = "3px";
        b.style.color        = "lightgrey";
        if (icon)
        {
            b.classList.add("fas");
            b.classList.add(icon);
        }
        reveal.appendChild(b);
        return b;
    }

    var buttonSave      = createButton(8, 8, "fa-save");
    buttonSave.onclick  = function(){ saveAnnotations(); }

    var buttonBoard      = createButton(8, 40, "fa-edit");
    buttonBoard.onclick  = function(){ toggleWhiteboard(); }

    var buttonPen        = createButton(8, 72, "fa-pen");
    buttonPen.onclick    = function(){ 
        if (pktimer)    clearTimeout(pktimer);
        if (!pk.isOpen) selectTool(ToolType.PEN); 
    }

    var buttonEraser     = createButton(8, 104, "fa-eraser");
    buttonEraser.onclick = function(){ selectTool(ToolType.ERASER); }

    var laserTimer;
    var buttonLaser      = createButton(8, 136, "fa-magic");
    buttonLaser.onclick  = function(){ toggleLaser(); }



    // add color picker to long-tap of buttonPen
    var pkdiv = createButton(40, 40);
    pkdiv.setAttribute("class", "color-picker");
    var pkoptions = { template: "<div class=\"whiteboard\" data-col=\"{color}\" style=\"background-color: {color}\"></div>" };
    var pk = new Piklor(pkdiv, colors, pkoptions);
    pk.colorChosen(function (col) { penColor = col; updateGUI(); });
    var pktimer;
    buttonPen.onmousedown = function(){ pktimer = setTimeout(function(){pk.open();}, 500); }


    // create container for canvases
    var container = document.createElement( 'div' );
    container.setAttribute( 'data-prevent-swipe', '' );
    container.style.transition              = "none";
    container.style.margin                  = "0";
    container.style.padding                 = "0";
    container.style.border                  = "1px solid transparent";
    container.style.boxSizing               = "content-box";
    container.style.position                = "absolute";
    container.style.top                     = "0px";
    container.style.left                    = "0px";
    container.style.width                   = "100%";
    container.style.height                  = "100%";
    container.style.maxHeight               = "100%";
    container.style.zIndex                  = "34";
    container.style.pointerEvents           = "none";
    container.style.overflowX               = 'hidden';
    container.style.overflowY               = 'hidden';
    container.style.touchAction             = 'pan-y'; // avoid Chrome's 'go-back' by horizontal swipe
    container.style.WebkitOverflowScrolling = 'auto';
    slides.appendChild( container );


    // create canvases
    var drawingCanvas = [ {id: "annotations" }, {id: "whiteboard" } ];
    setupDrawingCanvas(0);
    setupDrawingCanvas(1);


    /*
     * create a drawing canvas
     */
    function setupDrawingCanvas( id )
    {
        // size of slides
        var width  = Reveal.getConfig().width;
        var height = Reveal.getConfig().height;

        // create canvas
        var canvas = document.createElement( 'canvas' );
        canvas.setAttribute( 'data-prevent-swipe', '' );
        canvas.style.background = id==0 ? "rgba(0,0,0,0)" : background;
        canvas.style.border     = "none";
        canvas.style.boxSizing  = "border-box";
        canvas.style.position   = "relative";
        canvas.style.width      = "100%";
        canvas.style.height     = height + "px";
        canvas.width            = width  * canvasScale;
        canvas.height           = height * canvasScale;
        canvas.style.position   = "absolute";
        canvas.style.top        = "0px";
        canvas.style.left       = "0px";

        // DO NOT USE low-latency context, it fails on Chrome/Linux for whiteboards with >2 pages
        //var opts = {lowLatency: true, desynchronized: true};
        //var ctx = canvas.getContext("2d", opts);
        //if (ctx.getContextAttributes && ctx.getContextAttributes().desynchronized)
            //console.log('Low latency canvas supported!');

        // setup highDPI scaling & draw style
        var ctx = canvas.getContext("2d");
        ctx.scale(canvasScale, canvasScale);
        ctx.lineCap   = 'round';
        ctx.lineJoint = 'round';
        ctx.lineWidth = 2;

        // differences between the two canvases
        if ( id == "0" )
        {
            canvas.id = 'drawOnSlides';
            canvas.style.zIndex = "34";
        }
        else
        {
            canvas.id = 'drawOnBoard';
            canvas.style.zIndex = "36";
            canvas.style.visibility = "hidden";
        }

        // add canvas to container
        container.appendChild( canvas );

        // store relevant information
        drawingCanvas[id].canvas    = canvas;
        drawingCanvas[id].container = container;
        drawingCanvas[id].context   = ctx;
        drawingCanvas[id].width     = width;
        drawingCanvas[id].height    = height;

        // prevent accidential click, double-click, and context menu
        canvas.oncontextmenu = killEvent;
        canvas.onclick       = killEvent;
        canvas.ondblclick    = function(evt) {
            killEvent(evt);
            if (!tool && laser) toggleLightSaber(evt);
        }
    }


    /*
     * ignore this event, and don't propagate it further
     */
    function killEvent(evt)
    {
        evt.preventDefault(); 
        evt.stopPropagation();
        return false; 
    }



    /*****************************************************************
     * Interal GUI functions related to mouse cursor
     ******************************************************************/

    /*
     * create laser and eraser cursor
     */
    function initCursors()
    {
        var ctx = cursorCanvas.getContext("2d");

        // setup color gradient
        var col1 = "rgba(255, 0, 0, 1.0)";
        var col2 = "rgba(255, 0, 0, 0.0)";
        var grdLaser = ctx.createRadialGradient(10, 10, 1, 10, 10, 10);
        grdLaser.addColorStop(0, col1);
        grdLaser.addColorStop(1, col2);

        // render laser cursor
        ctx.clearRect(0, 0, 20, 20); 
        ctx.fillStyle = grdLaser;
        ctx.fillRect(0, 0, 20, 20);
        laserCursor = "url(" + cursorCanvas.toDataURL() + ") 10 10, auto";
    }


    /*
     * adjust pen cursor to have current color
     */
    function updateCursor()
    {
        var ctx  = cursorCanvas.getContext("2d");

        // convert penColor to rgb
        var elem = document.body.appendChild(document.createElement('fictum'));
        elem.style.color = penColor;
        var color = getComputedStyle(elem).color;
        var rgb = color.substring(color.indexOf('(')+1, color.lastIndexOf(')')).split(/,\s*/);
        document.body.removeChild(elem);

        // setup color gradient with pen color
        var col1 = "rgba(" + rgb[0] + "," + rgb[1] + "," + rgb[2] + ",1.0)";
        var col2 = "rgba(" + rgb[0] + "," + rgb[1] + "," + rgb[2] + ",0.0)";
        var grdPen   = ctx.createRadialGradient(10, 10, 1, 10, 10, 3);
        grdPen.addColorStop(0, col1);
        grdPen.addColorStop(1, col2);

        // render pen cursor
        ctx.clearRect(0, 0, 20, 20); 
        ctx.fillStyle = grdPen;
        ctx.fillRect(0, 0, 20, 20);
        penCursor = "url(" + cursorCanvas.toDataURL() + ") 10 10, auto";

        // render eraser cursor
        ctx.clearRect(0, 0, 20, 20); 
        ctx.strokeStyle = "rgba(128, 128, 128, 0.8)";
        ctx.fillStyle   = "rgba(255, 255, 255, 0.8)";
        ctx.lineWidth   = 1;
        ctx.beginPath();
        ctx.arc(10, 10, eraserRadius*Reveal.getScale(), 0, 2*Math.PI);
        ctx.fill(); 
        ctx.stroke(); 
        eraserCursor = "url(" + cursorCanvas.toDataURL() + ") 10 10, auto";

        // reset cursor
        container.style.cursor = tool ? 'none' : '';
    }


    // select a cursor
    function selectCursor(cur)
    {
        currentCursor = cur;
    }

    // show currently selected cursor
	function showCursor(cur)
    {
        if (cur != undefined) selectCursor(cur);
        container.style.cursor = currentCursor;
	}

    // hide cursor
	function hideCursor() 
    {
        container.style.cursor='none';
    }

    // hide cursor after 1 sec
    var hideCursorTimeout;
	function triggerHideCursor() 
    {
        clearTimeout( hideCursorTimeout );
        hideCursorTimeout = setTimeout( hideCursor, 1000 );
    }



    /*****************************************************************
     * Internal GUI functions (not called by user)
     ******************************************************************/

    /*
     * select active tool (pen, eraser, laser pointer)
     * and update GUI (which updates cursor)
     */
    function selectTool(newTool)
    {
        tool = (tool==newTool ? ToolType.NONE : newTool);
        updateGUI();
    }


    /*
     * toggle laser on/off.
     * when laser is active, reveal's cursor is overridden.
     */
    function toggleLaser()
    {
        laser = !laser;
        updateGUI();
    }

    function toggleLightSaber(evt)
    {
        laserOn.pause();
        laserOn.currentTime = 0;
        laserOff.pause();
        laserOff.currentTime = 0;
        laserHum.pause();
        laserHum.currentTime = 0;

        if (lightsaber.style.visibility == "visible")
        {
            laserOff.play();
            lightsaber.classList.remove("on");
            document.body.style.cursor = laserCursor;
        }
        else
        {
            document.body.style.cursor = 'none';
            lightsaber.style.left = evt.pageX + "px";
            lightsaber.style.top  = evt.pageY + "px";
            lightsaber.style.visibility = "visible";
            lightsaber.classList.add("on");
            laserOn.play();
        }
    }



    /*
     * Update GUI:
     * update icons based on selected tool
     * generate pen and laser cursors based on selected color
     * select cursor based on selected tool
     * enable/disable canvas pointerEvents
     */
    function updateGUI()
    {
        if (printMode) return;


        // update cursor using current color
        updateCursor();


        // reset icon states
        buttonLaser.style.color  = "lightgrey";
        buttonEraser.style.color = "lightgrey";
        buttonPen.style.color    = "lightgrey";
        buttonBoard.style.color  = "lightgrey";


        // set laser button
        if (laser) 
        {
            buttonLaser.style.color = "#2a9ddf";
            if (lightsaber.style.visibility == "visible")
                document.body.style.cursor = 'none';
            else
                document.body.style.cursor = laserCursor;
        }
        else
        {
            document.body.style.cursor = '';
        }


        // highlight active tool icon & select cursor
        switch (tool)
        {
            case ToolType.PEN:
                buttonPen.style.color = "#2a9ddf";
                selectCursor(penCursor);
                break;

            case ToolType.ERASER:
                buttonEraser.style.color = "#2a9ddf";
                selectCursor(eraserCursor);
                break;

            case ToolType.NONE:
                clearTimeout( hideCursorTimeout );
                selectCursor('');
                break;
        }


        // set whiteboard button
        if (boardMode)
            buttonBoard.style.color  = "#2a9ddf";
        else if (hasSlideData(Reveal.getIndices(), 1))
            buttonBoard.style.color = "red";


        // canvas setup
        if (tool)
        {
            container.style.border = "1px solid " + penColor;
        }
        else
        {
            container.style.border = "1px solid transparent";
        }
        drawingCanvas[mode].canvas.style.pointerEvents = (tool || laser) ? "auto" : "none";

    }


    /*
     * return height of current scribbles (max y-coordinate)
     */
    function whiteboardHeight( indices ) 
    { 
        if (!indices) indices = slideIndices;

        // minimum height: one Reveal page
        var height = 1;

        // find maximum y-coordinate of slide's curves
        if (hasSlideData(indices, 1))
        {
            var slideData = getSlideData(indices, 1);
            for (var i=0; i<slideData.events.length; i++)
            {
                var event = slideData.events[i];
                if (event.type == "draw")
                {
                    for (var j=1; j<event.coords.length; j+=2)
                    {
                        var y = event.coords[j];
                        if (y > height) height = y;
                    }
                }
            }
        }
        
        height = Math.round(height);
        if (DEBUG) console.log("slide height: " + height);

        return height;
    }


    /*
     * adjust board height to fit scribbles
     */
    function adjustWhiteboardHeight( indices ) 
    { 
        if (!indices) indices = slideIndices;
        var pageHeight     = Reveal.getConfig().height;
        var scribbleHeight = whiteboardHeight( indices );
        var height = pageHeight * Math.max(1, Math.ceil(scribbleHeight/pageHeight));
        setWhiteboardHeight(height);
    }


    /*
     * set whiteboard height to specified value
     */
    function setWhiteboardHeight(height)
    {
        // set canvas properties
        var canvas = drawingCanvas[1].canvas;
        canvas.style.height = height + "px";
        canvas.height = height * canvasScale;
        if (DEBUG) console.log("set slide height to " + height);

        // adjust canvas width to css width, which might change due to scrollbar
        var width = canvas.clientWidth;
        canvas.width = width * canvasScale;
        if (DEBUG) console.log("set slide width to " + width);

        // update context
        var ctx = drawingCanvas[1].context;
        ctx.scale(canvasScale, canvasScale);
        ctx.lineCap   = 'round';
        ctx.lineJoin  = 'round';
        ctx.lineWidth = 2;

        // remember to restore previous drawings with playbackEvents(1)!
    }


    /*
     * add one page to whiteboard (only when drawing on back-board!)
     */
    function addWhiteboardPage()
    {
        if (!tool || mode!=1) return;
        var pageHeight  = Reveal.getConfig().height;
        var boardHeight = drawingCanvas[1].canvas.clientHeight;
        setWhiteboardHeight( boardHeight + pageHeight );
        playbackEvents(1);
    }



    /*****************************************************************
     * Public GUI functions that can be triggered by user
     ******************************************************************/
    
    /* 
     * User wants to clear current slide (mapped to key Delete)
     */
    function clearSlide()
    {
        var ok = confirm("Delete notes and board on this slide?");
        if ( ok )
        {
            activeStroke = null;
            closeWhiteboard();

            clearCanvas( drawingCanvas[0].context );
            clearCanvas( drawingCanvas[1].context );

            mode = 1;
            var slideData = getSlideData();
            slideData.events = [];

            mode = 0;
            var slideData = getSlideData();
            slideData.events = [];
        }
    };


    /*
     * User triggers undo (mapped to key 'z')
     */
    function drawUndo()
    {
        if (hasSlideData( slideIndices, mode ))
        {
            var slideData = getSlideData( slideIndices, mode );
            slideData.events.pop();
            playbackEvents( mode );
        }
    }


    /*
     * Toggle whiteboard visibility (mapped to 't')
     */
    function toggleWhiteboard()
    {
        if ( boardMode )
        {
            closeWhiteboard();
        }
        else
        {
            showWhiteboard();
        }
        updateGUI();
    };


    /**
     * Opens an overlay for the whiteboard.
     */
    function showWhiteboard()
    {
        activeStroke = null;
        mode         = 1;
        boardMode    = true;

        // set container to board mode
        container.style.pointerEvents = "auto";
        container.style.overflowX     = "hidden";
        container.style.overflowY     = "scroll";

        // show board, adjust height, re-draw scribbles
        drawingCanvas[1].canvas.style.visibility = "visible";
        adjustWhiteboardHeight();
        playbackEvents(1);
    }


    /**
     * Closes open whiteboard.
     */
    function closeWhiteboard()
    {
        activeStroke = null;
        mode         = 0;
        boardMode    = false;

        // hide board
        drawingCanvas[1].canvas.style.visibility = "hidden";

        // set container to slides mode
        container.style.pointerEvents = "none";
        container.style.overflow = "hidden";
        container.scrollTop = "0px";
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


    /*
     * load scribbles from file
     * use Promise to ensure loading in init()
     */
    function loadAnnotations()
    {
        return new Promise( function(resolve) {

            // DEBUG: local storage
            if (LOCAL_STORAGE)
            {
                var data = localStorage.getItem('whiteboard');
                if (data)
                {
                    console.log("load from local storage");
                    storage = JSON.parse(data);
                    resolve();
                    return;
                }
            }

            // determine scribble filename
            var filename = annotationURL();

            console.log("whiteboard load " + filename);
            var req = new XMLHttpRequest();

            req.onload = function()
            {
                if (req.readyState == 4)
                {
                    if (req.status == 200 || req.status == 0)
                    {
                        try
                        {
                            storage = JSON.parse(req.responseText);
                            if ( drawingCanvas[0].width != storage[0].width || drawingCanvas[0].height != storage[0].height )
                            {
                                alert("Whiteboard: Loaded data does not match width/height of presentation");
                            }
                            console.log("whiteboard loaded");
                        }
                        catch(err)
                        {
                            console.error("Cannot parse " + filename + ": " + err);
                        }
                    }
                }
                else
                {
                    console.warn('Failed to get file ' + filename);
                }
                resolve();
            }

            req.onerror = function()
            {
                console.warn('Failed to get file ' + filename);
                resolve();
            }

            try
            {
                req.open('GET', filename, true);
                req.send();
            }
            catch(err)
            {
                console.warn('Failed to get file ' + filename + ': ' + err);
            }
        });
    }


    /*
     * filename for loading/saving annotations
     */
    function annotationFilename()
    {
        var url = location.pathname;
        var basename = (url.split('\\').pop().split('/').pop().split('.'))[0];

        // decker filenames vs. Mario filenames
        var filename;
        if (basename.substring(basename.length-5, basename.length) == "-deck")
            filename = basename.substring(0, basename.length-5) + "-annot.json";
        else
            filename = basename + ".json";

        return filename;
    }


    /*
     * URL for loading/saving annotations
     */
    function annotationURL()
    {
        var url = location.origin + location.pathname;
        var basename = url.substring(0, url.lastIndexOf("."));

        // decker filenames vs. Mario filenames
        var filename;
        if (basename.substring(basename.length-5, basename.length) == "-deck")
            filename = basename.substring(0, basename.length-5) + "-annot.json";
        else
            filename = basename + ".json";

        return filename;
    }


    /*
     * return annotations as JSON object
     */
    function annotationJSON()
    {
        // function to adjust precision of numbers when converting to JSON
        function twoDigits(key, val) {
            if (val != undefined)
                return val.toFixed ? Number(val.toFixed(2)) : val;
        }
        var blob = new Blob( [ JSON.stringify( storage, twoDigits ) ], { type: "application/json"} );
        return blob;
    }


    /*
     * save annotations to decker server
     */
    function saveAnnotations()
    {
        console.log("whiteboard: save annotations to decker");
        var xhr = new XMLHttpRequest();
        xhr.open('put', annotationURL(), true);
        xhr.onloadend = function() {
            if (xhr.status == 200) {
                console.log("whiteboard: save success");
                needSave = false;
                buttonSave.style.color = "lightgrey";
            } else {
                console.log("whiteboard: could not save to decker, download instead");
                downloadAnnotations();
            }
        };
        xhr.send(annotationJSON());
    }


    /*
     * download scribbles to user's Download directory
     */
    function downloadAnnotations()
    {
        var a = document.createElement('a');
        a.classList.add("whiteboard"); // otherwise a.click() is prevented/cancelled by global listener
        document.body.appendChild(a);
        try {
            a.download = annotationFilename();
            a.href = window.URL.createObjectURL( annotationJSON() );

        } catch( error ) {
            console.error("whiteboard download error: " + error);
        }
        a.click();
        document.body.removeChild(a);

        needSave = false;
        buttonSave.style.color = "lightgrey";
    }


    /*
     * get data object for given slide and given canvas
     */
    function getSlideData( indices, id )
    {
        if ( id == undefined ) id = mode;
        if (!indices) indices = slideIndices;

        // distinguish slide fragments only for slide annotations,
        // not for whiteboard mode
        for (var i = 0; i < storage[id].data.length; i++)
        {
            if (storage[id].data[i].slide.h === indices.h &&
                storage[id].data[i].slide.v === indices.v &&
                (id==1 || storage[id].data[i].slide.f === indices.f) )
            {
                return storage[id].data[i];
            }
        }

        // no data found -> add it
        storage[id].data.push( { slide: indices, events: [] } );
        return storage[id].data[storage[id].data.length-1];
    }


    /*
     * return whether there are scribbles on given slide?
     */
    function hasSlideData( indices, id )
    {
        if ( id == undefined ) id = mode;
        if (!indices) indices = slideIndices;

        // distinguish slide fragments only for slide annotations,
        // not for whiteboard mode
        for (var i = 0; i < storage[id].data.length; i++)
        {
            if (storage[id].data[i].slide.h === indices.h &&
                storage[id].data[i].slide.v === indices.v &&
                (id==1 || storage[id].data[i].slide.f === indices.f) )
            {
                return storage[id].data[i].events.length > 0;
            }
        }
        return false;
    }




    /*****************************************************************
     * Generate PDF
     ******************************************************************/

    function createPrintout( )
    {
        console.log("whiteboard: create printout");

        // slide dimensions
        var width   = Reveal.getConfig().width;
        var height  = Reveal.getConfig().height;

        // setup canvas
        var canvasScale = 2.0;
        var canvas = document.createElement( 'canvas' );
        canvas.style.background = "rgba(0,0,0,0)";
        canvas.style.border     = "none";
        canvas.style.width      = width  + "px";
        canvas.style.height     = height + "px";
        canvas.width            = width  * canvasScale;
        canvas.height           = height * canvasScale;

        // setup context
        var ctx = canvas.getContext("2d");
        ctx.scale(canvasScale, canvasScale);
        ctx.lineCap   = 'round';
        ctx.lineJoint = 'round';
        ctx.lineWidth = 2;


        // slide annotations
        for (var i = 0; i < storage[0].data.length; i++)
        {
            // get slide indices
            var h = storage[0].data[i].slide.h;
            var v = storage[0].data[i].slide.v;
            var f = storage[0].data[i].slide.f;

            // only use scribble on slides with fragments when fragments are
            // exported as separate PDF pages
            if (f != undefined && !Reveal.getConfig().pdfSeparateFragments)
                continue;

            // get slide data
            var slide = (f!=undefined) ? Reveal.getSlide(h,v,f) : Reveal.getSlide(h,v);
            var slideData = getSlideData( storage[0].data[i].slide, 0 );

            // draw strokes to image
            clearCanvas(ctx, canvasScale);
            for (var j = 0; j < slideData.events.length; j++)
                playEvent(ctx, slideData.events[j]);

            // convert canvas to image, add to slide
            var img = new Image();
            img.src = canvas.toDataURL();
            img.style.position  = "absolute";
            img.style.top       = "0px";
            img.style.left      = "0px";
            img.style.width     = width + "px";
            img.style.height    = height + "px";
            img.style.border    = "none";
            img.style.margin    = "0px";
            img.style.boxSizing = "border-box";
            img.style.zIndex    = "34";
            slide.appendChild( img );

            // add fragment data
            if (f != undefined)
            {
                assignFragmentIndex( slide ); // we need fragment-index per fragment!
                img.classList.add("fragment");
                img.classList.add("sequence");
                img.setAttribute("data-fragment-index", f);
            }
        }


        // collect next-slides for all slides with board stuff
        var nextSlide = [];
        for (var i = 0; i < storage[1].data.length; i++)
        {
            var h = storage[1].data[i].slide.h;
            var v = storage[1].data[i].slide.v;
            var slide = Reveal.getSlide(h,v);
            nextSlide.push( slide.nextSibling );
        }


        // go through board storage, paint image, insert slide
        for (var i = 0; i < storage[1].data.length; i++)
        {
            var h         = storage[1].data[i].slide.h;
            var v         = storage[1].data[i].slide.v;
            var slide     = Reveal.getSlide(h,v);
            var slideData = getSlideData( storage[1].data[i].slide, 1 );
            var parent    = Reveal.getSlide( storage[1].data[i].slide.h, storage[1].data[i].slide.v ).parentElement;

            if ( slideData.events.length )
            {
                var scribbleHeight = whiteboardHeight( storage[1].data[i].slide );

                // split oversize whiteboards into multiple slides
                for (var y = 0; y < scribbleHeight; y += height)
                {
                    // draw strokes to image (translate to respective page)
                    clearCanvas(ctx, canvasScale);
                    ctx.save();
                    ctx.translate(0, -y);
                    for (var j = 0; j < slideData.events.length; j++)
                        playEvent(ctx, slideData.events[j]);
                    ctx.restore();

                    // create new slide element
                    var newSlide = document.createElement( 'section' );
                    newSlide.classList.add( 'present' );
                    newSlide.style.width  = "100%";
                    newSlide.style.height = canvas.height/canvasScale + "px";

                    // convert page to image, add image to slide
                    var img = new Image();
                    img.src = canvas.toDataURL();
                    img.style.position  = "absolute";
                    img.style.top       = "0px";
                    img.style.left      = "0px";
                    img.style.width     = width  + "px";
                    img.style.height    = height + "px";
                    img.style.maxHeight = height + "px";
                    img.style.border    = "none";
                    img.style.margin    = "0px";
                    img.style.zIndex    = "34";
                    newSlide.appendChild( img );

                    if ( nextSlide[i] != null ) {
                        parent.insertBefore( newSlide, nextSlide[i] );
                    }
                    else {
                        parent.append( newSlide );
                    }
                }
            }
        }
    }


    /*
     * this function is (mostly) copied from reveal.js.
     * it sorts fragments and assigns a data-fragment-index to each fragment
     */
	function assignFragmentIndex( slide ) 
    {
        // all fragments of current slide
        var fragments = slide.querySelectorAll( '.fragment' );
		fragments = Array.prototype.slice.call( fragments );

		var ordered = [],
			unordered = [],
			sorted = [];

		// Group ordered and unordered elements
		fragments.forEach( function( fragment, i ) {
			if( fragment.hasAttribute( 'data-fragment-index' ) ) {
				var index = parseInt( fragment.getAttribute( 'data-fragment-index' ), 10 );
				if( !ordered[index] ) ordered[index] = [];
				ordered[index].push( fragment );
			}
			else {
				unordered.push( [ fragment ] );
			}
		} );

		// Append fragments without explicit indices in their
		// DOM order
		ordered = ordered.concat( unordered );

		// Manually count the index up per group to ensure there
		// are no gaps
		var index = 0;

		// Push all fragments in their sorted order to an array,
		// this flattens the groups
		ordered.forEach( function( group ) {
			group.forEach( function( fragment ) {
				sorted.push( fragment );
				fragment.setAttribute( 'data-fragment-index', index );
			} );
			index ++;
		} );
	}



    /*****************************************************************
     * Record and play-back events
     * Call low-level drawing routines
     ******************************************************************/

    /*
     * Push current event to slide's event list
     */
    function recordEvent( event )
    {
        var slideData = getSlideData();
        slideData.events.push(event);
        needSave = true;
        buttonSave.style.color = "#2a9ddf";
    }


    /*
     * Playback all events of the current slide for given canvas
     */
    function playbackEvents( id, ctx )
    {
        if (ctx == undefined) ctx = drawingCanvas[id].context;

        clearCanvas( ctx );

        if (hasSlideData( slideIndices, id))
        {
            var slideData = getSlideData( slideIndices, id );
            var index = 0;
            while ( index < slideData.events.length )
            {
                playEvent( ctx, slideData.events[index] );
                index++;
            }
        }
    };


    /* 
     * Playback one event (i.e. stroke)
     */
    function playEvent( ctx, event )
    {
        switch ( event.type )
        {
            case "draw":
                drawCurve( ctx, event);
                break;
            case "erase":
                eraseCurve( ctx, event );
                break;
        }
    };


    /*
     * clear given canvas
     */
    function clearCanvas( ctx, scale=canvasScale )
    {
        ctx.clearRect( 0, 0, ctx.canvas.width/scale, ctx.canvas.height/scale );
    }


    /*
     * Draw the curve stored in event to canvas ID
     */
    function drawCurve( ctx, event )
    {
        ctx.strokeStyle = event.color;

        // draw curve as quadratic spline
        var coords = event.coords;
        var cx, cy;
        if (coords.length > 4)
        {
            ctx.beginPath();
            ctx.moveTo(coords[0], coords[1]);
            cx = 0.5 * (coords[0] + coords[2]);
            cy = 0.5 * (coords[1] + coords[3]);
            ctx.lineTo(cx, cy);

            for (var i=2; i<coords.length-3; i+=2)
            {
                cx = 0.5 * (coords[i  ] + coords[i+2]);
                cy = 0.5 * (coords[i+1] + coords[i+3]);
                ctx.quadraticCurveTo(coords[i], coords[i+1], cx, cy);
            }
            ctx.stroke();
        }
    };


    /*
     * Erase the "curve" stored in event to canvas ID
     */
    function eraseCurve( ctx, event )
    {
        ctx.save();
        ctx.globalCompositeOperation = "destination-out";
        ctx.lineWidth = 2 * eraserRadius;
        ctx.beginPath();

        var coords = event.coords;
        ctx.moveTo(coords[0], coords[1]);
        for (var i=2; i<coords.length-1; i+=2)
            ctx.lineTo(coords[i], coords[i+1]);

        ctx.stroke();
        ctx.restore();
    };



    /*****************************************************************
     * GUI methods to start, continue, and stop a stroke
     * Are called from pointer/mouse callback
     * Call low-level drawing routines
     ******************************************************************/

    /*
     * start a stroke:
     * compute mouse position from event data, remember it
     * setup new stroke event (draw or erase)
     * call low-level draw/erase
     * update mouse cursor
     */
    function startStroke(evt)
    {
        // cancel timeouts
        clearTimeout( hideCursorTimeout );

        // update scale, zoom, and bounding rectangle
        slideZoom  = slides.style.zoom || 1;

        // convert pointer/touch position to local coordiantes
        var mouseX = evt.offsetX / slideZoom;
        var mouseY = evt.offsetY / slideZoom;

        if (mouseY < drawingCanvas[mode].canvas.height && mouseX < drawingCanvas[mode].canvas.width)
        {
            var ctx = drawingCanvas[mode].context;

            // erase mode
            if ((tool==ToolType.ERASER) || (evt.buttons > 1))
            {
                showCursor(eraserCursor);
                activeStroke = { type:  "erase", 
                                 coords: [mouseX, mouseY, mouseX, mouseY] };
                eraseCurve(ctx, activeStroke);
            }
            // draw mode
            else
            {
                showCursor(penCursor);
                // add start point twice to get endpoint interpolation for quadratic splines
                // but change if by one pixel to make single points more visible
                // (lines from point to same point are too small)
                activeStroke = { type:  "draw", 
                                 color: penColor, 
                                 coords: [mouseX, mouseY, mouseX+1, mouseY] };
                drawCurve(ctx, activeStroke);
            }
        }

        // don't propagate event any further
        killEvent(evt);
    };


    /*
     * continue the active stroke:
     * compute mouse position from event data, remember it
     * append data to active stroke
     * call low-level draw/erase
     */
    function continueStroke( evt )
    {
        if (activeStroke)
        {
            // convert touch position to mouse position
            var mouseX = evt.offsetX / slideZoom;
            var mouseY = evt.offsetY / slideZoom;

            // last position
            var n = activeStroke.coords.length;
            var lastX  = activeStroke.coords[n-2];
            var lastY  = activeStroke.coords[n-1];

            // (squared) distance to last mouse position
            var dist = (mouseX-lastX)*(mouseX-lastX) + (mouseY-lastY)*(mouseY-lastY);

            // only do something if mouse position changed and we are within bounds
            if ((dist > 4) &&
                (mouseY < drawingCanvas[mode].canvas.height) && 
                (mouseX < drawingCanvas[mode].canvas.width))
            {
                var ctx = drawingCanvas[mode].context;

                activeStroke.coords.push(mouseX);
                activeStroke.coords.push(mouseY);

                if ( activeStroke.type == "erase" )
                {
                    eraseCurve(ctx, activeStroke);
                }
                else
                {
                    drawCurve(ctx, activeStroke);
                }
            }

            // don't propagate event any further
            killEvent(evt);
        }
    };


    /*
     * stop current stroke:
     * stroke stroke to slide data
     * adjust height of board
     */
    function stopStroke(evt)
    {
        if (activeStroke)
        {
            // don't propagate event any further
            killEvent(evt);

            // duplicate last control point to get endpoint interpolation
            // of quadratic spline curves
            if ( activeStroke.type == "draw" )
            {
                var n     = activeStroke.coords.length;
                var lastX = activeStroke.coords[n-2];
                var lastY = activeStroke.coords[n-1];
                activeStroke.coords.push(lastX);
                activeStroke.coords.push(lastY);

                var ctx = drawingCanvas[mode].context;
                drawCurve(ctx, activeStroke);
            }

            // save stroke to slide's event data
            recordEvent( activeStroke );

            // inactive stroke
            activeStroke = null;
        }

        // pen mode? switch back to pen cursor
        if (tool==ToolType.PEN) 
        {
            showCursor(penCursor);
        }
        triggerHideCursor();
    };


    function duplicatePrevious()
    {
        let cur = Reveal.getIndices();

        if (cur.f === undefined || cur.f < 0) {
            // no previous frame
            console.log("duplicate: no previous frame");
            return;
        }
        if (hasSlideData(cur))
        {
            if (!confirm("Really copy from last slide? Current slide is not empty")) {
                return;
            }
        }
        let last = {
            h: cur.h,
            v: cur.v,
            f: cur.f - 1};

        function jsonCopy(src) { return JSON.parse(JSON.stringify(src)); }
        var data = getSlideData(cur);
        data.events = jsonCopy(getSlideData(last).events);
        console.log("duplicated", last, " to ", cur);
        playbackEvents(0);
    }



    /*****************************************************************
     * pointer and mouse callbacks
     ******************************************************************/

    function pointerdown(evt) 
    {
        // no tool selected -> return
        if (!tool) return;

        switch(evt.pointerType)
        {
            case "pen":
            case "mouse":
                switch(tool)
                {
                    case ToolType.PEN:
                    case ToolType.ERASER:
                        startStroke(evt);
                        break;
                }
                break;

            case "touch":
                showCursor(laserCursor);
                triggerHideCursor();
                break;
        }
    }


    function pointermove(evt) 
    {
        // no tool selected -> return
        if (!tool) return;

        // no mouse button pressed -> show laser, active auto-hide, return
        if (!evt.buttons && !laser)
        {
            showCursor();
            triggerHideCursor();
            return;
        }

        // mouse button pressed
        switch(evt.pointerType)
        {
            case "pen":
            case "mouse":
                switch(tool)
                {
                    case ToolType.PEN:
                    case ToolType.ERASER:
                        // try to exploit coalesced events
                        var events = [evt];
                        if (evt.getCoalescedEvents) 
                        {
                            events = evt.getCoalescedEvents() || events;
                            if (DEBUG) console.log( events.length + " coalesced move events");
                        }
                        for (let e of events) 
                            if (e.buttons > 0) 
                                continueStroke(e);
                        break;
                }
                break;

            case "touch":
                showCursor(laserCursor);
                triggerHideCursor();
                break;
        }
    }


    function pointerup(evt) 
    {
        // no tool selected -> return
        if (!tool) return;

        switch(evt.pointerType)
        {
            case "pen":
            case "mouse":
                switch(tool)
                {
                    case ToolType.PEN:
                    case ToolType.ERASER:
                        stopStroke(evt);
                        break;
                }
                break;

            case "touch":
                break;
        }
    }



    /*****************************************************************
     * Setup event listeners
     ******************************************************************/

    // setup pointer events
    if (window.PointerEvent)
    {
        container.addEventListener( 'pointerdown', pointerdown, true );
        container.addEventListener( 'pointermove', pointermove, {passive: false} );
        container.addEventListener( 'pointerup',   pointerup );
    }
    else
    {
        console.err("whiteboard requires PointerEvents");
    }


    // toggle light saber 
    window.addEventListener( 'dblclick', function(evt){
        if (laser) toggleLightSaber(evt);
    });


    // move light saber 
    window.addEventListener( 'pointermove', function(evt){
        if (lightsaber.style.visibility == "visible")
        {
            lightsaber.style.left = evt.pageX + "px";
            lightsaber.style.top  = evt.pageY + "px";
        }
    });


    // Intercept page leave when data is not saved
    window.onbeforeunload = function(e)
    {
        if (LOCAL_STORAGE)
        {
            console.log("save to local storage");
            localStorage['whiteboard'] = JSON.stringify(storage);
            return;
        }

        if (needSave) return "blabla";
    }


    // when drawing, stop ANY context menu from being opened
    window.addEventListener( "contextmenu", function(evt) 
    {
        if (tool)
        {
            evt.preventDefault();
            evt.stopPropagation();
            return false;
        }
    }, true );


    // when drawing, stop ANY click (e.g. menu icon)
    // only allow clicks for our (.whiteboard) buttons
    window.addEventListener( "click", function(evt) 
    {
        if (tool && !evt.target.classList.contains("whiteboard"))
        {
            evt.preventDefault();
            evt.stopPropagation();
            return false;
        }
    }, true );


    // bind to undo event (CTRL-Z or CMD-Z).
    // doesn't work with Reveal's key bindings,
    // probably due to CTRL/CMD key and the 
    // missing preventDefault.
    window.addEventListener('keydown', function(evt) 
    { 
        if ((evt.ctrlKey || evt.metaKey) && (!evt.shiftKey) &&
            String.fromCharCode(evt.which).toLowerCase() == 'z') 
        {
            evt.preventDefault();
            drawUndo();
        }
    });


    // what to do when the slide changes 
    function slideChanged(evt)
    {
        if ( !printMode ) {
            slideIndices = Reveal.getIndices();
            closeWhiteboard();
            playbackEvents( 0 );
        }
    }


    // whenever slide changes, update slideIndices and redraw
    Reveal.addEventListener( 'ready',          slideChanged );
    Reveal.addEventListener( 'slidechanged',   slideChanged );
    Reveal.addEventListener( 'fragmentshown',  slideChanged );
    Reveal.addEventListener( 'fragmenthidden', slideChanged );

    // update GUI (button) on slide change
    Reveal.addEventListener( 'ready',          updateGUI );
    Reveal.addEventListener( 'slidechanged',   updateGUI );
    Reveal.addEventListener( 'fragmentshown',  updateGUI );
    Reveal.addEventListener( 'fragmenthidden', updateGUI );

    // eraser cursor has to be updated on resize (i.e. scale change)
    Reveal.addEventListener( 'resize',         updateGUI );



    /*****************************************************************
     * Setup key bindings
     ******************************************************************/

    Reveal.addKeyBinding( { keyCode: 46, key: 'Delete', 
        description: 'Clear Slide' }, 
        clearSlide );

    Reveal.addKeyBinding( { keyCode: 68, key: 'D', 
        description: 'Toggle Drawing' }, 
        function(){ selectTool(ToolType.PEN); } );

    Reveal.addKeyBinding( { keyCode: 69, key: 'E',
        description: 'Toggle Eraser' },
        function(){ selectTool(ToolType.ERASER); });

    Reveal.addKeyBinding( { keyCode: 76, key: 'L', 
        description: 'Toggle Laser' }, 
        toggleLaser );

    Reveal.addKeyBinding( { keyCode: 87, key: 'W', 
        description: 'Toggle Whiteboard' }, 
        toggleWhiteboard );

    Reveal.addKeyBinding( { keyCode: 90, key: 'Z', 
        description: 'Whiteboard Undo' }, 
        drawUndo );

    Reveal.addKeyBinding( { keyCode: 13, key: 'Enter', 
        description: 'Extend whiteboard by one page' }, 
        addWhiteboardPage );

    Reveal.addKeyBinding( { keyCode: 82, key: 'R', 
        description: 'Repeat (duplicate) drawings from previous frame' }, 
        duplicatePrevious );



	return {
		init: function() { 

            // print some infos
            console.log("HighDPI scaling:  " + canvasScale);
            console.log("Pointer events:   " + !!(window.PointerEvent));
            console.log("Coalesced events: " + !!(window.PointerEvent && (new PointerEvent("pointermove")).getCoalescedEvents));

            return new Promise( function(resolve) {
                
                if (printMode)
                {
                    // load scribbles, create whiteboard slides, then resolve promise
                    loadAnnotations().then(createPrintout).then(resolve);
                }
                else
                {
                    // load scribbles, then resolve promise
                    loadAnnotations().then(resolve);
                }
            });
        },


        // menu plugin need access to trigger it
        downloadNotes: downloadAnnotations
    }

})();

Reveal.registerPlugin( 'whiteboard', RevealWhiteboard );

