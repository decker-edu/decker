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

    const STROKE_STEP_THRESHOLD = 2;


    /************************************************************************
     ** Configuration options, global variables
     ************************************************************************/

    // default values or user configuration?
    const config = Reveal.getConfig().whiteboard || {};

    // colors
    const penColors = config.colors || [ "blue", "red", "green", "cyan", "magenta", "yellow", "black" ];
    let penColor  = penColors[0];
    let penWidth = 2;

    // colors for buttons
    const activeColor   = 'var(--whiteboard-active-color)';
    const inactiveColor = 'var(--whiteboard-inactive-color)';

    // reveal setting wrt slide dimension
    const pageHeight = Reveal.getConfig().height;
    const pageWidth  = Reveal.getConfig().width;

    // reveal elements
    let reveal = document.querySelector( '.reveal' );
    let slides = document.querySelector( '.reveal .slides' );
    let slideZoom = slides.style.zoom || 1;

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

    // store which tools are active
    const ToolType = { PEN: 1, ERASER: 2, LASER: 3 };
    let tool = ToolType.PEN;

    // variable used to block leaving HTML page
    let unsavedAnnotations = false;

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

    // here we save SVG snapshots for undo/redo
    let undoHistory = [];
    const undoBufferSize = 10;


    // handle browser features
    const weHavePointerEvents   = !!(window.PointerEvent);
    const weHaveCoalescedEvents = !!(window.PointerEvent && (new PointerEvent("pointermove")).getCoalescedEvents);
    let   userShouldBeWarned    = !weHavePointerEvents || !weHaveCoalescedEvents;
    let   userHasBeenWarned     = false;

    function warnUser() {
        if (!weHavePointerEvents)
        {
            alert("Your browser does not support pointer events.\nWhiteboard will not work.\nBetter use Chrome/Chromium or Firefox.");
        }
        else if (!weHaveCoalescedEvents)
        {
            alert("Your browser does not support coalesced pointer events.\nWhiteboard drawing might be laggy.\nBetter use Chrome/Chromium or Firefox.");
        }
        userHasBeenWarned = true;
    }


    /************************************************************************
     * Setup GUI
     ************************************************************************/

    // generate file open dialog
    let fileInput = document.createElement( 'input' );
    fileInput.type = "file";
    fileInput.style.display = "none";
    reveal.appendChild(fileInput);
    fileInput.onchange = function(evt) {
        if (evt.target.files.length) {
            loadAnnotationsFromFile(evt.target.files[0]);
        }
    }


    // generate container for whiteboard buttons
    let buttons = document.createElement( 'div' );
    buttons.id = 'whiteboardButtons';
    reveal.appendChild(buttons);


    // function to generate a button
    function createButton(icon, callback, active=false, tooltip)
    {
        let b = document.createElement( 'button' );
        b.classList.add("whiteboard");
        b.classList.add("fas");
        b.classList.add(icon);
        b.onclick = callback;
        b.style.color = active ? activeColor : inactiveColor;
        if (tooltip) b.setAttribute('data-tooltip', tooltip);
        buttons.appendChild(b);
        return b;
    }


    let buttonWhiteboard = createButton("fa-edit", toggleWhiteboard, false, 'toggle whiteboard');
    buttonWhiteboard.id  = "whiteboardButton";
    //let buttonOpen       = createButton("fa-folder-open", () => { fileInput.click(); }, true, 'load annotations');
    let buttonSave       = createButton("fa-save", saveAnnotations, false, 'save annotations');
    let buttonAdd        = createButton("fa-plus", addWhiteboardPage, true, 'add whiteboard page');
    let buttonGrid       = createButton("fa-border-all", toggleGrid, false, 'toggle background grid');
    let buttonUndo       = createButton("fa-undo", undo, false, 'undo');
    let buttonPen        = createButton("fa-pen", () => {
        if (tool != ToolType.PEN) {
            selectTool(ToolType.PEN);
        }
        else {
            if (colorPicker.style.visibility == 'visible')
                hideColorPicker();
            else
                showColorPicker();
        }
    }, false, 'pen / properties');
    let buttonEraser = createButton("fa-eraser", () => { selectTool(ToolType.ERASER); }, false, 'eraser' );
    let buttonLaser  = createButton("fa-magic", () => { selectTool(ToolType.LASER); }, false, 'laser pointer' );


    // generate color picker container
    let colorPicker = document.createElement( 'div' );
    colorPicker.id = 'whiteboardColorPicker';
    reveal.appendChild(colorPicker);
    penColors.forEach( color => {
        let b = document.createElement( 'button' );
        b.classList.add("whiteboard", "fas", "fa-pen");
        b.onclick = () => { selectPenColor(color); }
        b.style.color = color;
        colorPicker.appendChild(b);
    });


    // generate penWidth slider
    let penWidthSlider = document.createElement( 'input' );
    penWidthSlider.id = "whiteboardSlider";
    penWidthSlider.type="range";
    penWidthSlider.min=1;
    penWidthSlider.max=10;
    penWidthSlider.value=2;
    colorPicker.appendChild(penWidthSlider);
    penWidthSlider.onchange = () => { selectPenRadius(parseInt(penWidthSlider.value)); };
    penWidthSlider.oninput  = () => { penWidthSlider.style.setProperty('--size', (parseInt(penWidthSlider.value)+1)+'px'); }

   
    // adjust slide height: slides should always have full page height,
    // even if they have not ;-). Might happen when using center:true
    // in Reveal's settings. In this case Reveal centers the slide by
    // adding a margin to the css:top variable. This is not compatible
    // with the whiteboard, since then the whiteboard page would be taller
    // than the slide itself. We fix this by removing the top-setting and
    // instead centering the slide through top/bottom padding. This allows
    // us to enforce full slide height (as configured in Reveal's settings).
    function adjustSlideHeight()
    {
        if (Reveal.getConfig().center)
        {
            let slide = Reveal.getCurrentSlide();
            const top = slide.style.top;
            console.log(top);
            if (top != '' && top != '0px')
            {
                slide.style.top = '';
                slide.style.paddingTop = top;
                slide.style.paddingBottom = top;
                slide.style.height = pageHeight + 'px';
            }
        }
    }


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

        const h = Math.floor(Math.min(pageWidth, pageHeight) / 25);

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

    function createLaserCursor()
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


    function createPenCursor()
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
    }


    function createEraserCursor()
    {
        let ctx = cursorCanvas.getContext("2d");
        cursorCanvas.width  = 20;
        cursorCanvas.height = 20;

        // (adjust canvas size and eraser radius using Reveal scale)
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
     * select active tool and update buttons & cursor
     */
    function selectTool(newTool)
    {
        tool = newTool;

        // update tool icons, update cursor
        buttonLaser.style.color  = inactiveColor;
        buttonEraser.style.color = inactiveColor;
        buttonPen.style.color    = inactiveColor;
        switch (tool)
        {
            case ToolType.PEN:
                buttonPen.style.color = penColor;
                selectCursor(penCursor);
                break;

            case ToolType.ERASER:
                buttonEraser.style.color = activeColor;
                selectCursor(eraserCursor);
                break;

            case ToolType.LASER:
                buttonLaser.style.color = activeColor;
                selectCursor(laserCursor);
                break;
        }
    }


    function showColorPicker()
    {
        colorPicker.style.visibility = 'visible';
    }


    function hideColorPicker()
    {
        colorPicker.style.visibility = 'hidden';
    }

    
    function selectPenColor(col)
    {
        hideColorPicker();
        penWidthSlider.style.setProperty("--color", col);
        penColor = col;
        buttonPen.style.color = penColor;
        createPenCursor();
        selectCursor(penCursor);
    }


    function selectPenRadius(radius)
    {
        hideColorPicker();
        penWidth = radius;
        penWidthSlider.value = radius;
        penWidthSlider.style.setProperty("--size", (radius+1)+'px');
        selectCursor(penCursor);
    }


    function toggleWhiteboard(state)
    {
        whiteboardActive = (typeof state === 'boolean') ? state : !whiteboardActive;
        
        if (!whiteboardActive)
        {
            // hide buttons
            buttons.classList.remove('active');
            buttonWhiteboard.style.color = inactiveColor;
            hideColorPicker();

            // reset SVG
            if (svg) {
                svg.style.border = "1px solid transparent";
                svg.style.pointerEvents = "none";
            }

            // reset cursor
            clearTimeout( hideCursorTimeout );
            slides.style.cursor = '';
        }
        else
        {
            if (userShouldBeWarned && !userHasBeenWarned) warnUser();

            // show buttons
            buttons.classList.add('active');
            buttonWhiteboard.style.color = activeColor;

            // activate SVG
            if (svg) {
                svg.style.border = "1px dashed lightgrey";
                svg.style.pointerEvents = "auto";
            }
        }
    }


    // set unsavedAnnotations and update save icon
    function needToSave(b)
    {
        unsavedAnnotations = b;
        buttonSave.style.color = unsavedAnnotations ? activeColor : inactiveColor;
    }



    /*
     * add one page to whiteboard (only when drawing on back-board!)
     */
    function addWhiteboardPage()
    {
        if (!svg) return;
        let boardHeight = svg.clientHeight;
        setWhiteboardHeight(boardHeight + pageHeight);
    }


    function adjustWhiteboardHeight()
    {
        // hide grid, so that height computation only depends on strokes
        let rect = getGridRect();
        let display;
        if (rect) {
            display = rect.style.display;
            rect.style.display = "none"; 
        }

        // height of current board (w/o grid)
        let bbox = svg.getBBox();
        let scribbleHeight = bbox.y + bbox.height;

        // show grid again
        if (rect) rect.style.display = display;

        // rounding
        var height = pageHeight * Math.max(1, Math.ceil(scribbleHeight/pageHeight));
        setWhiteboardHeight(height);
    }


    function setWhiteboardHeight(svgHeight)
    {
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
            if ( !printMode ) 
            {
                // (re-)start border pulsing
                // (taken from https://css-tricks.com/restart-css-animation/)
                slides.classList.remove("pulseBorder");
                void slides.offsetWidth; // this does the magic!
                slides.classList.add("pulseBorder");
            }
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
            let strokes = svg.querySelectorAll( 'svg>path' );
            if (strokes)
            {
                strokes.forEach( stroke => { stroke.remove(); } );
                needToSave(true);
            }

            let grid = svg.querySelector( 'svg>rect' );
            if (grid)
            {
                grid.remove();
                buttonGrid.style.color = inactiveColor;
                needToSave(true);
            }

            setWhiteboardHeight(pageHeight);
        }
    };



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
            buttonGrid.style.color = activeColor;
        }

        // otherwise, add it
        else
        {
            const boardHeight = svg.clientHeight;

            // add large rect with this pattern
            let rect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            svg.insertBefore(rect, svg.firstChild);

            rect.setAttribute('x', 0);
            rect.setAttribute('y', pageHeight);
            rect.setAttribute('width', '100%');
            rect.setAttribute('height', Math.max(0, boardHeight - pageHeight));

            rect.style.fill          = 'url(#gridPattern)';
            rect.style.stroke        = 'none';
            rect.style.pointerEvents = 'none';

            buttonGrid.style.color = activeColor;
        }

        needToSave(true);
    }




    /*****************************************************************
     ** Undo and re-do
     ******************************************************************/

    function clearUndoHistory()
    {
        undoHistory = [];
        buttonUndo.style.color = inactiveColor;
        buttonUndo.setAttribute('data-tooltip', 'undo');
    }

    function pushUndoHistory(action)
    {
        undoHistory.push( { action: action, svg: svg.innerHTML } );
        buttonUndo.style.color = activeColor;
        buttonUndo.setAttribute('data-tooltip', 'undo: ' + action);
        if (undoHistory.length > undoBufferSize) undoHistory.shift();
    }

    function undo()
    {
        if (undoHistory.length)
        {
            svg.innerHTML = undoHistory.pop().svg;

            if (undoHistory.length)
            {
                let action = undoHistory[undoHistory.length-1].action;
                buttonUndo.setAttribute('data-tooltip', 'undo: ' + action);
            }
            else
            {
                buttonUndo.style.color = inactiveColor;
                buttonUndo.setAttribute('data-tooltip', 'undo');
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
    function loadAnnotationsFromURL()
    {
        return new Promise( function(resolve) {

            // electron? try to load annotation from local file
            if (window.loadAnnotation) {
                window.loadAnnotation(annotationURL()).then( (storage) => {
                    if (storage) {
                        parseAnnotations(storage);
                        resolve();
                        return;
                    }
                });
            }

            // determine scribble filename
            let filename = annotationURL();

            console.log("whiteboard load " + filename);
            let xhr = new XMLHttpRequest();

            xhr.onloadend = function()
            {
                if (xhr.status == 200 || xhr.status == 0)
                {
                    try
                    {
                        const storage = JSON.parse(xhr.responseText);
                        parseAnnotations(storage);
                    }
                    catch(err)
                    {
                        console.error("Cannot parse " + filename + ": " + err);
                    }
                }
                else
                {
                    console.warn('Failed to get file ' + filename);
                }

                resolve();
            }

            xhr.open('GET', filename, true);
            xhr.send();
        });
    }

    
    /*
     * load scribbles from local file
     * use Promise to ensure loading in init()
     */
    function loadAnnotationsFromFile(filename)
    {
        if (window.File && window.FileReader && window.FileList) 
        {
            var reader = new FileReader();

            reader.onload = function(evt) {
                try
                {
                    const storage = JSON.parse(evt.target.result);
                    parseAnnotations(storage);

                    // re-setup current slide
                    slideChanged();
                }
                catch(err)
                {
                    console.error("Cannot parse " + filename + ": " + err);
                }
            }

            reader.readAsText(filename);
        }
        else {
            console.log("Your browser does not support the File API");
        }
    }



    /*
     * parse the annnotations blob loaded from URL or file
     */
    function parseAnnotations(storage)
    {
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
                        svg.style.display = 'none';
                    }
                }
            });
            console.log("whiteboard loaded");
        }

        // adjust height for PDF export
        if (printMode)
        {
            slides.querySelectorAll( 'svg.whiteboard' ).forEach( mysvg => { 
                svg=mysvg; 
                svg.style.display = 'block';
                adjustSlideHeight();
                adjustWhiteboardHeight();
            });
        }
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
    function annotationData()
    {
        let storage = { whiteboardVersion: 2.0, annotations: [] };
            
        slides.querySelectorAll( 'svg.whiteboard' ).forEach( svg => { 
            if (svg.children.length) {
                storage.annotations.push( { slide: svg.parentElement.id,
                                            svg:   svg.innerHTML } );
            }
        });
       
        return storage;
    }


    /*
     * return annotations as Blob
     */
    function annotationBlob()
    {
        return new Blob( [ JSON.stringify( annotationData() ) ], { type: "application/json"} );
    }


    /*
     * save annotations to decker server
     */
    function saveAnnotations()
    {
        // electron app?
        if (window.saveAnnotation) {
            if (window.saveAnnotation(annotationData(), annotationURL()))
            {
                console.log("whiteboard: save success");
                needToSave(false);
                return;
            }
        }

        console.log("whiteboard: save annotations to decker");
        let xhr = new XMLHttpRequest();
        xhr.open('put', annotationURL(), true);
        xhr.onloadend = function() {
            if (xhr.status == 200) {
                console.log("whiteboard: save success");
                needToSave(false);
            } else {
                console.log("whiteboard: could not save to decker, download instead");
                downloadAnnotations();
            }
        };
        xhr.send(annotationBlob());
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
            a.href = window.URL.createObjectURL( annotationBlob() );

        } catch( error ) {
            console.error("whiteboard download error: " + error);
        }
        a.click();
        document.body.removeChild(a);

        needToSave(false);
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
        return (p[0].toFixed(1) + ' ' + p[1].toFixed(1));
    }

    
    // convert points to quadratic Bezier spline
    function renderStroke(points, stroke)
    {
        const n=points.length;
        if (n < 2) return;

        let path = "";
        let c;

        path += ('M '  + printPoint(points[0]));
        path += (' L ' + printPoint(center(points[0], points[1])));

        if (n > 2)
        {
            path += ' Q ';
            for (let i=1; i<points.length-1; ++i)
            {
                c = center(points[i], points[i+1]);
                path += (' ' + printPoint(points[i]) + ' ' + printPoint(c));
            }
        }

        path += (' L ' + printPoint(points[n-1]));

        stroke.setAttribute('d', path);
    }


    // is point close enough to stroke to be counted as intersection?
    function isPointInStroke(path, point)
    {
        const length = path.getTotalLength();
        const precision = 10;
        let   p;
        let   d;

        for (let s=0; s<=length; s+=precision)
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
        const mouseX = evt.offsetX / slideZoom;
        const mouseY = evt.offsetY / slideZoom;

        // remember current state for later undo
        pushUndoHistory('paint stroke');

        // add stroke to SVG
        stroke = document.createElementNS('http://www.w3.org/2000/svg', 'path');
        svg.appendChild(stroke);
        stroke.style.stroke = penColor;
        stroke.style.strokeWidth = penWidth+'px';

        // add point, convert to Bezier spline
        points = [ [ mouseX, mouseY ], [mouseX, mouseY] ];
        renderStroke(points, stroke);

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
        // we need an active stroke
        if (!stroke) return;

        // collect coalesced events
        let events = [evt];
        if (evt.getCoalescedEvents) 
            events = evt.getCoalescedEvents() || events;

        // process events
        for (let evt of events) 
        {
            if (evt.buttons > 0)
            {
                // mouse position
                const mouseX = evt.offsetX / slideZoom;
                const mouseY = evt.offsetY / slideZoom;

                const newPoint = [ mouseX, mouseY ];
                const oldPoint = points[points.length-1];

                // only do something if mouse position changed and we are within bounds
                if (distance(newPoint, oldPoint) > STROKE_STEP_THRESHOLD)
                    points.push(newPoint);
            }
        }

        // update svg stroke
        renderStroke(points, stroke);
    };


    /*
     * stop current stroke:
     */
    function stopStroke(evt)
    {
        if (stroke)
        {
            // mouse position
            const mouseX    = evt.offsetX / slideZoom;
            const mouseY    = evt.offsetY / slideZoom;
            const newPoint  = [ mouseX, mouseY ];

            // add final point to stroke
            points.push(newPoint);
            renderStroke(points, stroke);
        }

        // reset stroke
        stroke = null;

        // new stroke -> we have to save
        needToSave(true);
    };


    /*
     * erase a stroke:
     * compute mouse position, compute "collision" with each stroke
     */
    function eraseStroke(evt)
    {
        // mouse position
        const mouseX = evt.offsetX / slideZoom;
        const mouseY = evt.offsetY / slideZoom;
        const point  = [mouseX, mouseY];

        svg.querySelectorAll( 'path' ).forEach( stroke => {
            if (isPointInStroke(stroke, point))
            {
                pushUndoHistory('erase stroke');
                stroke.remove();
                needToSave(true);
            }
        });
    };




    /*****************************************************************
     * pointer and mouse callbacks
     ******************************************************************/

    function pointerdown(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // event has to happen for SVG
        if (evt.target != svg) return;

        // only pen and mouse events
        if (evt.pointerType != 'pen' && evt.pointerType != 'mouse') return;

        // remove timer for cursor hiding
        clearTimeout( hideCursorTimeout );

        // laser mode or right mouse button
        if (tool==ToolType.LASER || (tool==ToolType.PEN && evt.buttons==2))
        {
            showCursor( laserCursor );
        }

        // eraser mode or middle mouse button
        else if (tool==ToolType.ERASER || (tool==ToolType.PEN && evt.buttons>=4))
        {
            showCursor(eraserCursor);
            eraseStroke(evt);
        }

        // pencil mode
        else if (tool == ToolType.PEN)
        {
            hideCursor();
            startStroke(evt);
        }


        // don't propagate event any further
        return killEvent(evt);
    }


    function pointermove(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // event has to happen for SVG
        if (evt.target != svg) return;

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
        if (tool==ToolType.LASER || (tool==ToolType.PEN && evt.buttons==2))
        {
            //showCursor(laserCursor);
            //triggerHideCursor();
        }

        // eraser mode or middle mouse button
        else if (tool==ToolType.ERASER || (tool==ToolType.PEN && evt.buttons>=4))
        {
            eraseStroke(evt);
        }

        // pencil mode
        else if (tool == ToolType.PEN)
        {
            continueStroke(evt);
        }


        // don't propagate event any further
        return killEvent(evt);
    }


    function pointerup(evt) 
    {
        // only when whiteboard is active
        if (!whiteboardActive) return;

        // event has to happen for SVG
        if (evt.target != svg) return;

        // only pen and mouse events
        if (evt.pointerType != 'pen' && evt.pointerType != 'mouse') return;

        // finish pen stroke
        if (tool == ToolType.PEN)
        {
            stopStroke(evt);
            selectCursor(penCursor); // might be laser/eraser due to buttons pressed
        }

        // re-activate cursor hiding
        triggerHideCursor();

        // don't propagate event any further
        return killEvent(evt);
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
        if (unsavedAnnotations) return "blabla";
    }


    // when drawing, stop ANY context menu from being opened
    window.addEventListener( "contextmenu", function(evt) 
    {
        if (whiteboardActive)
        {
            return killEvent(evt);
        }
    }, true );


    // when drawing, prevent touch events triggering clicks 
    // (e.g. menu icon, control arrows)
    // only allow clicks for our (.whiteboard) buttons
    function preventTouchClick(evt)
    {
        if (whiteboardActive && !evt.target.classList.contains("whiteboard"))
        {
            return killEvent(evt);
        }
    }
    window.addEventListener( "touchstart", preventTouchClick, true );
    window.addEventListener( "touchend",   preventTouchClick, true );
  

    // prevent iPad pen to trigge scrolling (by killing touchstart
    // whenever force is detected
    function preventPenScroll(evt) 
    {
        if (evt.targetTouches[0].force) {
            return killEvent(evt);
        }
    }
    slides.addEventListener( "touchstart", preventPenScroll );


    // bind to undo event (CTRL-Z or CMD-Z).
    // doesn't work with Reveal's key bindings,
    // probably due to CTRL/CMD key and the 
    // missing preventDefault.
    window.addEventListener('keydown', function(evt) 
    { 
        if ((evt.ctrlKey || evt.metaKey) && (!evt.shiftKey) &&
            String.fromCharCode(evt.which).toLowerCase() == 'z') 
        {
            undo();
            return killEvent(evt);
        }
    });


    // what to do when the slide changes 
    function slideChanged(evt)
    {
        if ( !printMode ) 
        {
            // hide pen dialog
            hideColorPicker();

            // determine current fragment index
            currentFragmentIndex = Reveal.getIndices().f;

            // hide all SVG's
            slides.querySelectorAll( 'svg.whiteboard' ).forEach( svg => { 
                svg.style.display = 'none';
            });

            // adjust slide height (call before setupSVG!)
            adjustSlideHeight();

            // setup and show current slide's SVG (adjust slide height before!)
            setupSVG();
            svg.style.display = 'block';

            // activate/deactivate SVG
            toggleWhiteboard(whiteboardActive); 

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

            // update SVG grid icon
            buttonGrid.style.color = (svg && getGridRect()) ? activeColor : inactiveColor;

            // clear undo history (updates icon)
            clearUndoHistory();

            // just to be sure, update slide zoom
            slideZoom = slides.style.zoom || 1;
        }
    }


    // handle fragments
    function fragmentChanged()
    {
        // hide pen dialog
        hideColorPicker();

        // determine current fragment index
        currentFragmentIndex = Reveal.getIndices().f;

        if (svg && currentFragmentIndex != undefined)
        {
            // adjust fragment visibility
            svg.querySelectorAll('svg>path[data-frag]').forEach( stroke => { 
                stroke.style.visibility = 
                    ((stroke.getAttribute('data-frag') > currentFragmentIndex) && 
                     (stroke.getBBox().y < pageHeight))
                    ? 'hidden' : 'visible';
            });
        }
    }



    // whenever slide changes, update slideIndices and redraw
    Reveal.addEventListener( 'ready', slideChanged );
    Reveal.addEventListener( 'slidechanged', slideChanged );

    // whenever fragment changes, update stroke visibility
    Reveal.addEventListener( 'fragmentshown',   fragmentChanged );
    Reveal.addEventListener( 'fragmenthidden',  fragmentChanged );

    // eraser cursor has to be updated on resize (i.e. scale change)
    Reveal.addEventListener( 'resize', () => { 
        // hide pen dialog
        hideColorPicker();
        // size of eraser cursor has to be adjusted
        createEraserCursor();
        // slide zoom might change
        slideZoom = slides.style.zoom || 1;
    });



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

            // generate cursors
            createLaserCursor();
            createEraserCursor();
            createPenCursor();

            // set default state
            toggleWhiteboard(false);
            selectTool(ToolType.PEN);
            selectPenColor(penColors[0]);
            selectPenRadius(2);

            // hide buttons in print mode
            if (printMode) buttons.style.display = 'none';

            // load annotations
            return new Promise( (resolve) => loadAnnotationsFromURL().then(resolve) );
        },

        // menu plugin need access to trigger it
        downloadNotes: downloadAnnotations
    }

})();

Reveal.registerPlugin( 'whiteboard', RevealWhiteboard );

