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


let RevealWhiteboard = (function(){

    const LOCAL_STORAGE = false;
    const QUADRATIC_SPLINE = true;
    const STROKE_STEP_THRESHOLD = 2;


    /************************************************************************
     ** Tools
     ************************************************************************/

    /*
     * return path to this script
     */
    function scriptPath()
    {
        // obtain plugin path from the script element
        let src;
        if (document.currentScript) {
            src = document.currentScript.src;
        } else {
            let sel = document.querySelector('script[src$="/whiteboard.js"]')
            if (sel) {
                src = sel.src;
            }
        }

        let path = typeof src === undefined ? src
            : src.slice(0, src.lastIndexOf("/") + 1);
        return path;
    }

    const path = scriptPath();




    /************************************************************************
     ** Configuration options, global variables
     ************************************************************************/

    // default values or user configuration?
    const config     = Reveal.getConfig().whiteboard || {};

    // colors
    const colors = config.colors || [ "black", "red", "green", "blue", "yellow", "cyan", "magenta" ];
    let penColor = config.penColor || "blue";

    // reveal elements
    let reveal = document.querySelector( '.reveal' );
    let slides = document.querySelector( '.reveal .slides' );

    // different cursors used by whiteboard
    let eraserCursor;
    let eraserRadius = 10;
    let laserCursor;
    let penCursor;
    let currentCursor;

    // canvas for dynamic cursor generation
    let cursorCanvas = document.createElement( 'canvas' );
    cursorCanvas.id     = "CursorCanvas";
    cursorCanvas.width  = 20;
    cursorCanvas.height = 20;
    initCursors();

    // store which tools are active
    const ToolType = { PEN: 1, ERASER: 2, LASER: 3 };
    let tool = ToolType.PEN;

    // letiable used to block leaving HTML page
    let needSave = false;

    // is the user generating a PDF?
    const printMode = ( /print-pdf/gi ).test( window.location.search );

    // whiteboard canvas
    let svg = null;
    let stroke = null;
    let points = null; 

    // global whiteboard status
    let whiteboardActive = false;

    // currently active fragment
    let currentFragmentIndex = 0;


    /************************************************************************
     * Setup GUI
     ************************************************************************/

    // load css
    function loadCSS()
    {
        let head  = document.getElementsByTagName('head')[0];
        let link  = document.createElement('link');
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
        let b = document.createElement( 'button' );
        b.classList.add("whiteboard");
        b.style.left   = left + "px";
        b.style.bottom = bottom + "px";
        b.style.visibility = 'hidden'; // hide per default (for PDF export)
        b.classList.add("fas");
        b.classList.add(icon);
        reveal.appendChild(b);
        return b;
    }


    let buttonWhiteboard = createButton(8, 8, "fa-edit");
    buttonWhiteboard.onclick = toggleWhiteboard;

    let buttonSave      = createButton(8, 38, "fa-save");
    buttonSave.onclick  = saveAnnotations;

    let buttonAdd       = createButton(8, 68, "fa-plus");
    buttonAdd.onclick   = addWhiteboardPage;

    let buttonGrid      = createButton(8, 98, "fa-border-all");
    buttonGrid.onclick  = toggleGrid;

    let buttonUndo      = createButton(8, 128, "fa-undo");
    buttonUndo.onclick  = undoStroke;

    let buttonPen        = createButton(8, 168, "fa-pen");
    buttonPen.onclick    = function(){ 
        if (tool == ToolType.PEN) pk.open();
        else selectTool(ToolType.PEN); 
    }

    let buttonEraser     = createButton(8, 198, "fa-eraser");
    buttonEraser.onclick = function(){ selectTool(ToolType.ERASER); }

    let buttonLaser      = createButton(8, 228, "fa-magic");
    buttonLaser.onclick = function(){ selectTool(ToolType.LASER); }


    // add color picker
    let pkdiv = document.createElement( 'div' );
    pkdiv.classList.add("color-picker");
    reveal.appendChild(pkdiv);
    let pkoptions = { template: "<div class=\"whiteboard\" data-col=\"{color}\" style=\"background-color: {color}\"></div>" };
    let pk = new Piklor(pkdiv, colors, pkoptions);
    pk.colorChosen( (col) => { penColor = col; updateGUI(); } );


    // create whiteboard SVG for current slide
    // is currently called for each slide, even if we don't have strokes yet
    function setupSVG(slide, height)
    {
        // get slide
        if (!slide)  slide = Reveal.getCurrentSlide();
        if (!slide) return;

        // does SVG exist already?
        svg = slide.querySelector('svg.whiteboard');
        if (svg) return svg;

        // otherwise, let's create the SVG
        svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.classList.add("whiteboard");
        svg.setAttribute( 'data-prevent-swipe', '' );
        svg.setAttribute( 'preserveAspectRatio', 'none' );
        svg.style.pointerEvents = "none";
        svg.style.border = "1px solid transparent";

        // SVG dimensions
        const pageHeight = Reveal.getConfig().height;
        svg.style.width  = "100%";
        if (!height) height = pageHeight;
        svg.style.height = height + "px";

        // prevent accidential click, double-click, and context menu
        svg.oncontextmenu = killEvent;
        svg.onclick       = killEvent;
        svg.ondblclick    = killEvent;

        // add SVG to slide
        slide.appendChild( svg );

        // slide indices (used for saving)
        let idx = Reveal.getIndices(slide);
        svg.h = idx.h;
        svg.v = idx.v ? idx.v : 0;

        return svg;
    }


    /*
     * setup hidden SVG for defining grid pattern
     */
    function setupGridPattern()
    {
        let svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        svg.style.position      = 'absolute';
        svg.style.left          = '0px';
        svg.style.top           = '0px';
        svg.style.width         = '10px';
        svg.style.height        = '10px';
        svg.style.pointerEvents = 'none';
        slides.insertBefore(svg, slides.firstChild);

        const pageWidth   = Reveal.getConfig().width;
        const pageHeight  = Reveal.getConfig().height;
        const h           = Math.floor(Math.min(pageWidth, pageHeight) / 25);

        svg.innerHTML = 
            `<defs>
                 <pattern id="smallPattern" width="${h}" height="${h}" patternUnits="userSpaceOnUse">
                     <path d="M ${h} 0 L 0 0 0 ${h}" fill="none" stroke="#EEEEEE" stroke-width="2"/>
                 </pattern>
                 <pattern id="gridPattern" width="${pageWidth}" height="${pageHeight}" patternUnits="userSpaceOnUse">
                     <rect width="${pageWidth}" height="${pageHeight}" fill="url(#smallPattern)" stroke="lightgrey" stroke-width="3"/>
                 </pattern>
             </defs>`;
    }
    setupGridPattern();


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
        let ctx = cursorCanvas.getContext("2d");

        // setup color gradient
        let col1 = "rgba(255, 0, 0, 1.0)";
        let col2 = "rgba(255, 0, 0, 0.0)";
        let grdLaser = ctx.createRadialGradient(10, 10, 1, 10, 10, 10);
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
        let ctx = cursorCanvas.getContext("2d");
        cursorCanvas.width  = 20;
        cursorCanvas.height = 20;

        // convert penColor to rgb
        let elem = document.body.appendChild(document.createElement('fictum'));
        elem.style.color = penColor;
        let color = getComputedStyle(elem).color;
        let rgb = color.substring(color.indexOf('(')+1, color.lastIndexOf(')')).split(/,\s*/);
        document.body.removeChild(elem);

        // setup color gradient with pen color
        let col1 = "rgba(" + rgb[0] + "," + rgb[1] + "," + rgb[2] + ",1.0)";
        let col2 = "rgba(" + rgb[0] + "," + rgb[1] + "," + rgb[2] + ",0.0)";
        let grdPen   = ctx.createRadialGradient(10, 10, 1, 10, 10, 3);
        grdPen.addColorStop(0, col1);
        grdPen.addColorStop(1, col2);

        // render pen cursor
        ctx.clearRect(0, 0, 20, 20); 
        ctx.fillStyle = grdPen;
        ctx.fillRect(0, 0, 20, 20);
        penCursor = "url(" + cursorCanvas.toDataURL() + ") 10 10, auto";

        // render eraser cursor (adjust canvas size and eraser radius using Reveal scale)
        const slideScale = Reveal.getScale();
        const radius = eraserRadius * slideScale;
        const width  = 2*radius;
        cursorCanvas.width  = width;
        cursorCanvas.height = width;
        ctx.clearRect(0, 0, width, width); 
        ctx.fillStyle = "rgba(255,255,255,0)";
        ctx.fillRect(0, 0, width, width);
        ctx.strokeStyle = "rgba(128, 128, 128, 0.8)";
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.arc(radius, radius, radius-2, 0, 2*Math.PI);
        ctx.stroke(); 
        eraserCursor = "url(" + cursorCanvas.toDataURL() + ") " + radius + " " + radius + ", auto";
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
        slides.style.cursor = currentCursor;
	}

    // hide cursor
	function hideCursor() 
    {
        slides.style.cursor='none';
    }

    // hide cursor after 1 sec
    let hideCursorTimeout;
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
        tool = newTool;
        updateGUI();
    }


    function toggleWhiteboard()
    {
        whiteboardActive = !whiteboardActive;
        updateGUI();
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

        // is whiteboard disabled?
        if (!whiteboardActive)
        {
            // hide buttons
            buttonWhiteboard.style.visibility = 'visible';
            buttonWhiteboard.style.color      = 'lightgrey';
            buttonSave.style.visibility   = 'hidden';
            buttonAdd.style.visibility    = 'hidden';
            buttonGrid.style.visibility   = 'hidden';
            buttonUndo.style.visibility   = 'hidden';
            buttonPen.style.visibility    = 'hidden';
            buttonEraser.style.visibility = 'hidden';
            buttonLaser.style.visibility  = 'hidden';

            // reset SVG
            if (svg) {
                svg.style.border = "1px solid transparent";
                svg.style.pointerEvents = "none";
            }

            // reset cursor
            clearTimeout( hideCursorTimeout );
            slides.style.cursor = '';
            return;
        }


        // whiteboard is active
        buttonWhiteboard.style.visibility = 'visible';
        buttonWhiteboard.style.color      = '#2a9ddf';
        buttonSave.style.visibility   = 'visible';
        buttonAdd.style.visibility    = 'visible';
        buttonGrid.style.visibility   = 'visible';
        buttonUndo.style.visibility   = 'visible';
        buttonPen.style.visibility    = 'visible';
        buttonEraser.style.visibility = 'visible';
        buttonLaser.style.visibility  = 'visible';
        if (svg) {
            svg.style.border = "1px dashed lightgrey";
            svg.style.pointerEvents = "auto";
        }


        // update cursor using current color
        updateCursor();


        // save icon
        buttonSave.style.color = needSave ? "#2a9ddf" : "lightgrey";

        // grid icon
        buttonGrid.style.color = (svg && getGridRect()) ? "#2a9ddf" : "lightgrey";

        // tool icons
        buttonLaser.style.color  = "lightgrey";
        buttonEraser.style.color = "lightgrey";
        buttonPen.style.color    = "lightgrey";
        switch (tool)
        {
            case ToolType.PEN:
                buttonPen.style.color = penColor;
                selectCursor(penCursor);
                break;

            case ToolType.ERASER:
                buttonEraser.style.color = "#2a9ddf";
                selectCursor(eraserCursor);
                break;

            case ToolType.LASER:
                buttonLaser.style.color = "#2a9ddf";
                selectCursor(laserCursor);
                break;
        }
    }


    /*
     * add one page to whiteboard (only when drawing on back-board!)
     */
    function addWhiteboardPage()
    {
        if (!svg) return;
        let pageHeight  = Reveal.getConfig().height;
        let boardHeight = svg.clientHeight;
        setWhiteboardHeight(boardHeight + pageHeight);
    }


    function adjustWhiteboardHeight()
    {
        // height of one page
        let pageHeight = Reveal.getConfig().height;

        // height of current board
        let bbox = svg.getBBox();
        let scribbleHeight = bbox.y + bbox.height;

        // rounding
        var height = pageHeight * Math.max(1, Math.ceil(scribbleHeight/pageHeight));
        setWhiteboardHeight(height);
    }


    function setWhiteboardHeight(svgHeight)
    {
        const pageHeight    = Reveal.getConfig().height;
        const pageWidth     = Reveal.getConfig().width;
        const needScrollbar = svgHeight > pageHeight;

        // set height of SVG
        svg.style.height = svgHeight + "px";

        // if grid exists, adjust its height
        let rect = getGridRect();
        if (rect) rect.setAttribute('height', svgHeight - pageHeight);

        // update scrollbar of slides container
        if (needScrollbar)
            slides.classList.add('needScrollbar');
        else
            slides.classList.remove('needScrollbar');

        // adjust with of slides container to accomodate scrollbar
        let currentWidth = slides.clientWidth;
        if (currentWidth != pageWidth)
        {
            const width = (2*pageWidth - currentWidth);
            slides.style.width = width + "px";
        }

        // activate/deactivate pulsing border indicator
        if (needScrollbar)
        {
            // (re-)start border pulsing
            // (taken from https://css-tricks.com/restart-css-animation/)
            slides.classList.remove("pulseBorder");
            void slides.offsetWidth; // this does the magic!
            slides.classList.add("pulseBorder");
        }
        else
        {
            slides.classList.remove("pulseBorder");
        }
    }



    /*****************************************************************
     * Public GUI functions that can be triggered by user
     ******************************************************************/
    
    /* 
     * User wants to clear current slide (mapped to key Delete)
     */
    function clearSlide()
    {
        if (confirm("Delete notes and board on this slide?"))
        {
            svg.querySelectorAll( 'svg>path' ).forEach( stroke => { stroke.remove(); } );
            svg.querySelector( 'svg>rect' ).remove();
            setWhiteboardHeight(Reveal.getConfig().height);
            updateGUI();
        }
    };



    /*
     * User triggers undo (mapped to key 'z')
     */
    function undoStroke()
    {
        if (svg.lastChild) 
        {
            svg.removeChild(svg.lastChild);
        }
    }


    /*
     * return grid rect
     */
    function getGridRect()
    {
        if (svg) return svg.querySelector('svg>rect');
    }


    /*
     * add/remove background rectangle with grid pattern
     */
    function toggleGrid()
    {
        if (!svg) return;

        // if grid exists, remove it
        let rect = getGridRect();
        if (rect) 
        {
            rect.remove();
        }

        // otherwise, add it
        else
        {
            const pageHeight  = Reveal.getConfig().height;
            const boardHeight = svg.clientHeight;

            // add large rect with this pattern
            let rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            if (svg.children.length > 1)
                svg.insertBefore(rect, svg.children[1]);
            else
                svg.appendChild(rect);

            rect.setAttribute('x', 0);
            rect.setAttribute('y', pageHeight);
            rect.setAttribute('width', '100%');
            rect.setAttribute('height', Math.max(0, boardHeight - pageHeight));

            rect.style.fill          = 'url(#gridPattern)';
            rect.style.stroke        = 'none';
            rect.style.pointerEvents = 'none';
        }

        updateGUI();
    }




    /*****************************************************************
     ** Load and save
     ******************************************************************/

    /*
     * load scribbles from file
     * use Promise to ensure loading in init()
     */
    function loadAnnotations()
    {
        return new Promise( function(resolve) {

            // determine scribble filename
            let filename = annotationURL();

            console.log("whiteboard load " + filename);
            let req = new XMLHttpRequest();

            req.onload = function()
            {
                if (req.readyState == 4)
                {
                    if (req.status == 200 || req.status == 0)
                    {
                        try
                        {
                            // parse JSON
                            const storage = JSON.parse(req.responseText);

                            // create SVGs
                            if (storage.whiteboardVersion && storage.whiteboardVersion >= 2)
                            {
                                storage.annotations.forEach( page => {
                                    let slide = document.getElementById(page.slide);
                                    if (slide)
                                    {
                                        // use global SVG
                                        svg = setupSVG(slide);
                                        if (svg)
                                        {
                                            svg.innerHTML = page.svg;
                                        }
                                    }
                                });
                                console.log("whiteboard loaded");
                            }

                            // adjust height for PDF export
                            if (printMode)
                            {
                                slides.querySelectorAll( 'svg.whiteboard' ).forEach( mysvg => { 
                                    svg=mysvg; adjustWhiteboardHeight();
                                });
                            }
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
        let url = location.pathname;
        let basename = (url.split('\\').pop().split('/').pop().split('.'))[0];

        // decker filenames vs. Mario filenames
        let filename;
        if (basename.endsWith("-deck"))
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
        let url = location.origin + location.pathname;
        let basename = url.substring(0, url.lastIndexOf("."));

        // decker filenames vs. Mario filenames
        let filename;
        if (basename.endsWith("-deck"))
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
        let storage = { whiteboardVersion: 2.0, annotations: [] };
            
        slides.querySelectorAll( 'svg.whiteboard' ).forEach( svg => { 
            if (svg.children.length) {
                storage.annotations.push( { slide: svg.parentElement.id,
                                            svg:   svg.innerHTML } );
            }
        });
       
        let blob = new Blob( [ JSON.stringify( storage ) ], { type: "application/json"} );
        return blob;
    }


    /*
     * save annotations to decker server
     */
    function saveAnnotations()
    {
        console.log("whiteboard: save annotations to decker");
        let xhr = new XMLHttpRequest();
        xhr.open('put', annotationURL(), true);
        xhr.onloadend = function() {
            if (xhr.status == 200) {
                console.log("whiteboard: save success");
                needSave = false;
                updateGUI();
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
        let a = document.createElement('a');
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
        updateGUI();
    }




    /*****************************************************************
     ** Geometry helper functions
     ******************************************************************/

    // Euclidean distance between points p and q 
    function distance(p, q)
    {
        const dx = p[0]-q[0];
        const dy = p[1]-q[1];
        return Math.sqrt( dx*dx + dy*dy );
    }


    // compute midpoint between p0 and p1
    function center(p0, p1)
    {
        return [ 0.5*(p0[0]+p1[0]), 0.5*(p0[1]+p1[1]) ];
    }

    
    // return string representation of point p (two decimal digits)
    function printPoint(p)
    {
        return (p[0].toFixed(2) + ' ' + p[1].toFixed(2));
    }

    
    // convert points to quadratic Bezier spline
    function pointsToBezier(points, stroke)
    {
        let path = "";

        if (QUADRATIC_SPLINE)
        {
            let c;

            path += ('M '  + printPoint(points[0]));
            path += (' L ' + printPoint(center(points[0], points[1])));

            for (let i=1; i<points.length-1; ++i)
            {
                c = center(points[i], points[i+1]);
                path += (' Q ' + printPoint(points[i]) + ' ' + printPoint(c));
            }
            path += (' L ' + printPoint(points[points.length-1]));
        }
        else
        {
            path += ('M '  + printPoint(points[0]));
            for (let i=1; i<points.length; ++i)
                path += (' L ' + printPoint(points[i]));
        }

        stroke.setAttribute('d', path);
    }


    // is point close enough to stroke to be counted as intersection?
    function isPointInStroke(path, point)
    {
        const length = path.getTotalLength();
        const precision = 10;
        let   p;
        let   d;

        for (let s=0; s<length; s+=precision)
        {
            p = path.getPointAtLength(s);
            d = distance( point, [p.x, p.y] );
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
    function startStroke(evt)
    {
        // mouse position
        const slideZoom  = slides.style.zoom || 1;
        const mouseX = evt.offsetX / slideZoom;
        const mouseY = evt.offsetY / slideZoom;

        // add stroke to SVG
        stroke = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        svg.appendChild(stroke);
        stroke.style.fill = 'none';
        stroke.style.stroke = penColor;
        stroke.style.strokeWidth = '2px';

        // add point, convert to Bezier spline
        points = [ [ mouseX, mouseY ], [mouseX+1, mouseY+1] ];
        pointsToBezier(points, stroke);

        // add fragment index to stroke
        if (currentFragmentIndex != undefined)
        {
            stroke.setAttribute('data-frag', currentFragmentIndex);
        } 
    };


    /*
     * continue the active stroke:
     * append mouse point to active stroke
     */
    function continueStroke( evt )
    {
        if (stroke && evt.target==svg)
        {
            // mouse position
            const slideZoom  = slides.style.zoom || 1;
            const mouseX = evt.offsetX / slideZoom;
            const mouseY = evt.offsetY / slideZoom;

            const newPoint = [ mouseX, mouseY ];
            const oldPoint = points[points.length-1];

            // only do something if mouse position changed and we are within bounds
            if (distance(newPoint, oldPoint) > STROKE_STEP_THRESHOLD)
            {
                points.push(newPoint);
                pointsToBezier(points, stroke);
            }
        }
    };


    /*
     * stop current stroke:
     */
    function stopStroke(evt)
    {
        if (stroke && evt.target==svg)
        {
            // mouse position
            const slideZoom = slides.style.zoom || 1;
            const mouseX    = evt.offsetX / slideZoom;
            const mouseY    = evt.offsetY / slideZoom;
            const newPoint  = [ mouseX, mouseY ];

            // add final point to stroke
            points.push(newPoint);
            pointsToBezier(points, stroke);
        }

        // reset stroke
        stroke = null;

        // new stroke -> we have to save
        needSave = true;
        updateGUI();
    };


    /*
     * erase a stroke:
     * compute mouse position, compute "collision" with each stroke
     */
    function eraseStroke(evt)
    {
        // mouse position
        const slideZoom  = slides.style.zoom || 1;
        const mouseX = evt.offsetX / slideZoom;
        const mouseY = evt.offsetY / slideZoom;
        const point  = [mouseX, mouseY];

        svg.querySelectorAll( 'path' ).forEach( stroke => {
            if (isPointInStroke(stroke, point))
                stroke.remove();
        });
    };




    /*****************************************************************
     * pointer and mouse callbacks
     ******************************************************************/

    function pointerdown(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // only pen and mouse events
        if (evt.pointerType != 'pen' && evt.pointerType != 'mouse') return;

        // cancel timeouts
        showCursor();
        clearTimeout( hideCursorTimeout );


        // laser mode or right mouse button
        if (tool == ToolType.LASER || evt.buttons == 2)
        {
            showCursor();
            triggerHideCursor();
        }

        // eraser mode or middle mouse button
        else if (tool == ToolType.ERASER || evt.buttons == 4)
        {
            showCursor(eraserCursor);
            eraseStroke(evt);
        }

        // pencil mode
        else if (tool == ToolType.PEN)
        {
            startStroke(evt);
        }


        // don't propagate event any further
        killEvent(evt);
    }


    function pointermove(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // only pen and mouse events
        if (evt.pointerType != 'pen' && evt.pointerType != 'mouse') return;

        // no mouse button pressed -> show cursor, active auto-hide, return
        if (!evt.buttons)
        {
            showCursor();
            triggerHideCursor();
            return;
        }


        // laser mode or right mouse button
        if (tool == ToolType.LASER || evt.buttons == 2)
        {
            showCursor(laserCursor);
            triggerHideCursor();
        }

        // eraser mode or middle mouse button
        else if (tool == ToolType.ERASER || evt.buttons == 4)
        {
            showCursor(eraserCursor);
            eraseStroke(evt);
        }

        // pencil mode
        else if (tool == ToolType.PEN)
        {
            showCursor(penCursor);
            let events = [evt];
            if (evt.getCoalescedEvents) 
                events = evt.getCoalescedEvents() || events;
            for (let e of events) 
                if (e.buttons > 0) 
                    continueStroke(e);
        }


        // don't propagate event any further
        killEvent(evt);
    }


    function pointerup(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // only pen and mouse events
        if (evt.pointerType != 'pen' && evt.pointerType != 'mouse') return;

        if (tool == ToolType.PEN)
        {
            stopStroke(evt);
        }

        // re-activate cursor hiding
        triggerHideCursor();

        // don't propagate event any further
        killEvent(evt);
    }



    /*****************************************************************
     * Setup event listeners
     ******************************************************************/

    // setup pointer events
    if (window.PointerEvent)
    {
        slides.addEventListener( 'pointerdown', pointerdown, true );
        slides.addEventListener( 'pointermove', pointermove );
        slides.addEventListener( 'pointerup',   pointerup );
    }
    else
    {
        console.err("whiteboard requires PointerEvents");
    }


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
        if (whiteboardActive)
        {
            killEvent(evt);
            return false;
        }
    }, true );


    // when drawing, stop ANY click (e.g. menu icon)
    // only allow clicks for our (.whiteboard) buttons
    window.addEventListener( "click", function(evt) 
    {
        if (whiteboardActive && !evt.target.classList.contains("whiteboard"))
        {
            killEvent(evt);
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
            killEvent(evt);
            undoStroke();
        }
    });


    // what to do when the slide changes 
    function slideChanged(evt)
    {
        if ( !printMode ) 
        {
            // determine current fragment index
            currentFragmentIndex = Reveal.getIndices().f;

            // hide all SVG's
            slides.querySelectorAll( 'svg.whiteboard' ).forEach( svg => { 
                svg.style.display = 'none';
            });

            // show current slide's SVG
            setupSVG();
            svg.style.display = 'block';

            // set height based on annotations
            adjustWhiteboardHeight();

            // setup slides container
            slides.scrollTop  = 0;
            if (svg.clientHeight > slides.clientHeight)
                slides.classList.add('needScrollbar');
            else
                slides.classList.remove('needScrollbar');

            // adjust fragment visibility
            fragmentChanged();
        }
    }


    // handle fragments
    function fragmentChanged()
    {
        // determine current fragment index
        currentFragmentIndex = Reveal.getIndices().f;

        if (currentFragmentIndex != undefined)
        {
            // adjust fragment visibility
            svg.querySelectorAll('svg>path[data-frag]').forEach( stroke => { 
                stroke.style.visibility = 
                    stroke.getAttribute('data-frag') > currentFragmentIndex ? 'hidden' : 'visible';
            });
        }
    }



    // whenever slide changes, update slideIndices and redraw
    Reveal.addEventListener( 'ready',        slideChanged );
    Reveal.addEventListener( 'slidechanged', slideChanged );

    // whenever fragment changes, update stroke visibility
    Reveal.addEventListener( 'fragmentshown',   fragmentChanged );
    Reveal.addEventListener( 'fragmenthidden',  fragmentChanged );

    // update GUI (button) on slide change
    Reveal.addEventListener( 'ready',        updateGUI );
    Reveal.addEventListener( 'slidechanged', updateGUI );

    // eraser cursor has to be updated on resize (i.e. scale change)
    Reveal.addEventListener( 'resize', updateGUI );



    /*****************************************************************
     * Setup key bindings
     ******************************************************************/

    Reveal.addKeyBinding( { keyCode: 46, key: 'Delete', 
        description: 'Clear Slide' }, 
        clearSlide );

    Reveal.addKeyBinding( { keyCode: 87, key: 'W', 
        description: 'Toggle Whiteboard' }, 
        toggleWhiteboard );



	return {
		init: function() { 

            // print some infos
            console.log("Pointer events:   " + !!(window.PointerEvent));
            console.log("Coalesced events: " + !!(window.PointerEvent && (new PointerEvent("pointermove")).getCoalescedEvents));

            return new Promise( (resolve) => loadAnnotations().then(resolve) );
        },

        // menu plugin need access to trigger it
        downloadNotes: downloadAnnotations
    }

})();

Reveal.registerPlugin( 'whiteboard', RevealWhiteboard );

