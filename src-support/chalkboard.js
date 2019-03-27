/*****************************************************************
 ** Author: Asvin Goel, goel@telematique.eu
 **
 ** A plugin for reveal.js adding a chalkboard.
 **
 ** Version: 0.6
 **
 ** License: MIT license (see LICENSE.md)
 **
 ** Credits:
 ** Chalkboard effect by Mohamed Moustafa https://github.com/mmoustafa/Chalkboard
 ** Edited by Mario Botsch
 ** Edited by Jan-Philipp Stauffert
 ******************************************************************/

import laserImg from './img/laser.png';
import spongeImg from './img/sponge.png';
import blackboardImg from './img/blackboard.png';
import boardmarkerImg from './img/boardmarker.png';
import chalkImg from './img/chalk.png';

export default class RevealChalkboard {
  constructor() {
    // config
    let config = Reveal.getConfig().chalkboard;
    this.background = config.background ? config.background : [
      'rgba(127,127,127,0.0)',
      'url(' + blackboardImg + '), auto'
    ];
    this.pen = config.pen ? config.pen : [
      'url(' + boardmarkerImg + '), auto',
      'url(' + chalkImg + '), auto'
    ];
    this.draw = config.draw ? config.draw : [this.drawOnSlide.bind(this), this.drawOnBoard.bind(this)];
    this.color = config.color ? config.color : [
      'rgba(0,0,255,1)',
      'rgba(255,255,255,0.5)'
    ];
    if (config.toggleChalkboardButton !== false) {
      this.setupToggleChalkboardButton();
    }
    if (config.toggleNotesButton !== false) {
      this.setupToggleNotesButton();
    }

    // handle zoom
    this.slides = document.querySelector('.reveal .slides');
    this.zoom = this.slides.style.zoom;

    // setup laser
    this.laserRadius = 17;
    this.laser = document.createElement('img');
    this.laser.src = laserImg;
    this.laser.id = "laser";
    this.laser.style.visibility = "hidden";
    this.laser.style.position = "absolute";
    this.laser.style.zIndex = 40;
    document.querySelector(".reveal").appendChild(this.laser);

    // setup eraser
    this.eraserRadius = 15;
    this.eraserCursor = 'url("' + spongeImg + '") 25 20, auto';
    this.eraseMode = false;

    // setup canvas
    this.drawingCanvas = [{
      id: "notescanvas"
    }, {
      id: "chalkboard"
    }];
    this.setupDrawingCanvas(0);
    this.setupDrawingCanvas(1);

    // setup
    this.mode = 0; // 0: draw on slides, 1: draw on whiteboard

    this.mouseX = 0;
    this.mouseY = 0;
    this.xLast = null;
    this.yLast = null;

    this.slideIndices = {
      h: 0,
      v: 0
    };
    this.event = null;

    this.storage = [{
        width: this.drawingCanvas[0].width,
        height: this.drawingCanvas[0].height,
        data: []
      },
      {
        width: this.drawingCanvas[1].width,
        height: this.drawingCanvas[1].height,
        data: []
      }
    ];
    this.isLoaded = null;
    if (config.src != null) {
      this.loadData(config.src);
    };

    // Intercept page leave when data is not saved
    this.needSave = false;
    window.onbeforeunload = (function (e) {
      if (this.needSave) {
        e.preventDefault();
        return "Your chalkboard drawings were not yet saved.";
      }
    }).bind(this);

    this.printMode = (/print-pdf/gi).test(window.location.search);

    this.setupCallbacks();
  }

  whenReady(callback) {
    // wait for drawings to be loaded and markdown to be parsed
    if (this.isLoaded === null || document.querySelector('section[data-markdown]:not([data-markdown-parsed])')) {
      setTimeout(whenReady, 100, callback)
    } else {
      callback();
    }
  };

  setupToggleChalkboardButton() {
    const buttonC = document.createElement('div');
    buttonC.id = "toggle-chalkboard";
    buttonC.style.position = "absolute";
    buttonC.style.zIndex = 40;
    buttonC.style.fontSize = "16px";
    buttonC.style.left = "8px";
    buttonC.style.bottom = "8px";
    buttonC.style.top = "auto";
    buttonC.style.right = "auto";
    buttonC.style.padding = "3px";
    buttonC.style.borderRadius = "3px";
    buttonC.style.color = "lightgrey";
    buttonC.innerHTML = '<i onclick="RevealChalkboard.toggleChalkboard()" class="fas fa-edit"></i>';
    document.querySelector(".reveal").appendChild(buttonC);
    this.buttonC = buttonC;
  }

  setupToggleNotesButton() {
    const buttonN = document.createElement('div');
    buttonN.id = "toggle-notes";
    buttonN.style.position = "absolute";
    buttonN.style.zIndex = 40;
    buttonN.style.fontSize = "16px";
    buttonN.style.left = "40px";
    buttonN.style.bottom = "8px";
    buttonN.style.top = "auto";
    buttonN.style.right = "auto";
    buttonN.style.padding = "3px";
    buttonN.style.borderRadius = "3px";
    buttonN.style.color = "lightgrey";
    buttonN.innerHTML = '<i onclick="RevealChalkboard.toggleNotesCanvas()" class="fas fa-pencil-alt"></i>';
    document.querySelector(".reveal").appendChild(buttonN);
    this.buttonN = buttonN;
  }


  setupDrawingCanvas(id) {
    // size of slides
    const width = Reveal.getConfig().width;
    const height = Reveal.getConfig().height;

    // create div container
    const container = document.createElement('div');
    container.classList.add('overlay');
    container.setAttribute('data-prevent-swipe', '');
    container.oncontextmenu = function () {
      return false;
    }
    container.style.background = this.background[id];
    container.id = this.drawingCanvas[id].id;

    // create canvas
    var canvas = document.createElement('canvas');
    canvas.style.width = width;
    canvas.style.height = height;
    canvas.width = width;
    canvas.height = height;
    canvas.setAttribute('data-chalkboard', id);
    canvas.style.cursor = this.pen[id];
    container.appendChild(canvas);

    // store relevant information
    this.drawingCanvas[id].canvas = canvas;
    this.drawingCanvas[id].context = canvas.getContext("2d");
    this.drawingCanvas[id].container = container;
    this.drawingCanvas[id].width = canvas.width;
    this.drawingCanvas[id].height = canvas.height;


    if (id === 0) {
      container.style.zIndex = "34";
      container.classList.add('visible')
      container.style.pointerEvents = "none";
      this.notescanvas = container;
    } else {
      container.style.zIndex = "36";
      canvas.style.border = "1px solid " + this.color[1];
      this.chalkboard = container;
    }

    // add div to reveal.slides
    document.querySelector('.reveal .slides').appendChild(container);
  }

  /**
   * Load data.
   */
  loadData(filename) {
    const xhr = new XMLHttpRequest();
    const self = this;
    xhr.onload = function () {
      if (xhr.readyState === 4 && xhr.status !== 404) {
        self.storage = JSON.parse(xhr.responseText);
        for (let id = 0; id < self.storage.length; id++) {
          if (
            self.drawingCanvas[id].width != self.storage[id].width ||
            self.drawingCanvas[id].height != self.storage[id].height
          ) {
            console.warn("chalkboard: loaded data does not match width/height");
          }
        }
      } else {
        console.warn('Didn\'t find prerecorded chalkboard data: ' + filename + ". ReadyState: " + xhr.readyState + ", Status: " + xhr.status);
      }
      self.isLoaded = true;
    };

    xhr.open('GET', filename, true);
    try {
      xhr.send();
    } catch (error) {
      console.warn('Failed to get file ' + filename + '. Make sure that the presentation and the file are served by a HTTP server and the file can be found there. ' + error);
    }
  }

  /**
   * Download data.
   */
  downloadData() {
    const a = document.createElement('a');
    document.body.appendChild(a);
    try {
      const url = location.pathname;
      const basename = (url.split('\\').pop().split('/').pop().split('.'))[0];
      const filename = basename + ".json";
      a.download = filename;
      var blob = new Blob([JSON.stringify(this.storage)], {
        type: "application/json"
      });
      a.href = window.URL.createObjectURL(blob);
    } catch (error) {
      a.innerHTML += " (" + error + ")";
    }
    a.click();
    document.body.removeChild(a);

    this.needSave = false;
  }

  /**
   * Returns data object for the slide with the given indices.
   */
  getSlideData(indices, id) {
    if (id === undefined) id = this.mode;
    if (!indices) indices = this.slideIndices;
    for (let i = 0; i < this.storage[id].data.length; i++) {
      if (this.storage[id].data[i].slide.h === indices.h &&
        this.storage[id].data[i].slide.v === indices.v &&
        this.storage[id].data[i].slide.f === indices.f) {
        return this.storage[id].data[i];
      }
    }

    // no data found -> add it
    this.storage[id].data.push({
      slide: indices,
      events: []
    });
    return this.storage[id].data[this.storage[id].data.length - 1];
  }

  /**
   * do we have slide data? 
   */
  hasSlideData(indices, id) {
    if (id == undefined) id = this.mode;
    if (!indices) indices = this.slideIndices;
    for (let i = 0; i < this.storage[id].data.length; i++) {
      if (this.storage[id].data[i].slide.h === indices.h &&
        this.storage[id].data[i].slide.v === indices.v &&
        this.storage[id].data[i].slide.f === indices.f) {
        return this.storage[id].data[i].events.length > 0;
      }
    }
    return false;
  }

  createPrintout() {
    // MARIO: we want to print the drawings
    //drawingCanvas[0].container.classList.remove( 'visible' ); // do not print notes canvas

    const nextSlide = [];
    const width = Reveal.getConfig().width;
    const height = Reveal.getConfig().height;

    // collect next-slides for all slides with board stuff
    for (let i = 0; i < this.storage[1].data.length; i++) {
      const h = this.storage[1].data[i].slide.h;
      const v = this.storage[1].data[i].slide.v;
      const f = this.storage[1].data[i].slide.f;
      const slide = f ? Reveal.getSlide(h, v, f) : Reveal.getSlide(h, v);
      nextSlide.push(slide.nextSibling);
    }

    // go through board storage, paint image, insert slide
    for (let i = 0; i < this.storage[1].data.length; i++) {
      const h = this.storage[1].data[i].slide.h;
      const v = this.storage[1].data[i].slide.v;
      const f = this.storage[1].data[i].slide.f;
      const slide = f ? Reveal.getSlide(h, v, f) : Reveal.getSlide(h, v);

      const slideData = getSlideData(this.storage[1].data[i].slide, 1);

      const parent = Reveal.getSlide(
        this.storage[1].data[i].slide.h,
        this.storage[1].data[i].slide.v
      ).parentElement;

      const imgCanvas = document.createElement('canvas');
      imgCanvas.width = width;
      imgCanvas.height = height;

      const imgCtx = imgCanvas.getContext("2d");
      imgCtx.fillStyle = "white";
      this.color[1] = "black";
      imgCtx.rect(0, 0, imgCanvas.width, imgCanvas.height);
      imgCtx.fill();

      for (let j = 0; j < slideData.events.length; j++) {
        switch (slideData.events[j].type) {
          case "draw":
            for (let k = 1; k < slideData.events[j].curve.length; k++) {
              this.draw[1](imgCtx,
                slideData.events[j].curve[k - 1].x,
                slideData.events[j].curve[k - 1].y,
                slideData.events[j].curve[k].x,
                slideData.events[j].curve[k].y
              );
            }
            break;

          case "erase":
            for (let k = 0; k < slideData.events[j].curve.length; k++) {
              this.erase(imgCtx,
                slideData.events[j].curve[k].x,
                slideData.events[j].curve[k].y
              );
            }
            break;

          case "clear":
            this.addPrintout(parent, nextSlide[i], imgCanvas);
            imgCtx.clearRect(0, 0, imgCanvas.width, imgCanvas.height);
            imgCtx.fill();
            break;

          default:
            break;
        }
      }

      if (slideData.events.length) {
        this.addPrintout(parent, nextSlide[i], imgCanvas);
      }
    }
    Reveal.sync();
  }

  addPrintout(parent, nextSlide, imgCanvas) {
    const slideCanvas = document.createElement('canvas');
    slideCanvas.width = Reveal.getConfig().width;
    slideCanvas.height = Reveal.getConfig().height;
    const ctx = slideCanvas.getContext("2d");
    ctx.fillStyle = "white";
    ctx.rect(0, 0, slideCanvas.width, slideCanvas.height);
    ctx.fill();
    ctx.drawImage(imgCanvas, 0, 0);

    const newSlide = document.createElement('section');
    newSlide.classList.add('present');
    newSlide.innerHTML = '<h1 style="visibility:hidden">Drawing</h1>';
    newSlide.setAttribute("data-background-size", '100% 100%');
    newSlide.setAttribute("data-background-repeat", 'norepeat');
    newSlide.setAttribute("data-background", 'url("' + slideCanvas.toDataURL("image/png") + '")');
    if (nextSlide !== null) {
      parent.insertBefore(newSlide, nextSlide);
    } else {
      parent.append(newSlide);
    }
  }

  drawOnSlide(context, fromX, fromY, toX, toY) {
    context.lineWidth = 2;
    context.lineCap = 'round';
    context.strokeStyle = this.color[0];
    context.beginPath();
    context.moveTo(fromX, fromY);
    context.lineTo(toX, toY);
    context.stroke();
  }

  drawOnBoard(context, fromX, fromY, toX, toY) {
    context.lineWidth = 2;
    context.lineCap = 'round';
    context.strokeStyle = this.color[1];
    context.beginPath();
    context.moveTo(fromX, fromY);
    context.lineTo(toX, toY);
    context.stroke();
  }

  erase(context, x, y) {
    context.save();
    context.beginPath();
    context.arc(x, y, this.eraserRadius, 0, 2 * Math.PI, false);
    context.clip();
    context.clearRect(
      x - this.eraserRadius,
      y - this.eraserRadius,
      this.eraserRadius * 2,
      this.eraserRadius * 2
    );
    context.restore();
  }


  showLaser(evt) {
    if (!this.event) // only when drawing not active
    {
      this.drawingCanvas[this.mode].canvas.style.cursor = "none";
      this.laser.style.left = (evt.pageX - this.laserRadius) + "px";
      this.laser.style.top = (evt.pageY - this.laserRadius) + "px";
      this.laser.style.visibility = "visible";
    }
  }

  hideLaser() {
    this.laser.style.visibility = "hidden";
  }

  /**
   * Opens an overlay for the chalkboard.
   */
  showChalkboard() {
    this.laser.style.visibility = "hidden";
    this.drawingCanvas[1].container.classList.add('visible');
    this.xLast = null;
    this.yLast = null;
    this.event = null;
    this.mode = 1;
  }


  /**
   * Closes open chalkboard.
   */
  closeChalkboard() {
    this.laser.style.visibility = "hidden";
    this.drawingCanvas[1].container.classList.remove('visible');
    this.xLast = null;
    this.yLast = null;
    this.event = null;
    this.mode = 0;
  }

  /**
   * Clear current canvas.
   */
  clearCanvas(id) {
    this.drawingCanvas[id].context.clearRect(
      0, 0,
      this.drawingCanvas[id].width,
      this.drawingCanvas[id].height
    );
  }

  recordEvent(event) {
    const slideData = this.getSlideData();
    slideData.events.push(event);
    this.needSave = true;
  }

  startPlayback(finalMode) {
    this.closeChalkboard();
    this.mode = 0;
    for (let id = 0; id < 2; id++) {
      this.clearCanvas(id);

      /* MARIO: don't just call getSlideData, since it pushed slide data when nothing is found
         which somehow inserts black slides for printing */
      if (this.hasSlideData(this.slideIndices, id)) {
        const slideData = this.getSlideData(this.slideIndices, id);
        let index = 0;
        while (index < slideData.events.length) {
          this.playEvent(id, slideData.events[index]);
          index++;
        }
      }
    }

    if (finalMode !== undefined) {
      this.mode = finalMode;
    }
    if (this.mode == 1) this.showChalkboard();
  }

  playEvent(id, event) {
    switch (event.type) {
      case "clear":
        this.clearCanvas(id);
        break;
      case "draw":
        this.drawCurve(id, event);
        break;
      case "erase":
        this.eraseCurve(id, event);
        break;
    }
  }

  drawCurve(id, event) {
    const ctx = this.drawingCanvas[id].context;

    if (event.curve.length > 1) {
      for (let i = 1; i < event.curve.length; i++) {
        this.draw[id](ctx,
          event.curve[i - 1].x,
          event.curve[i - 1].y,
          event.curve[i].x,
          event.curve[i].y);
      }
    } else {
      // we need to record and play single points (for math equations)
      this.draw[id](ctx,
        event.curve[0].x,
        event.curve[0].y,
        event.curve[0].x,
        event.curve[0].y);
    }
  }

  eraseCurve(id, event) {
    if (event.curve.length > 1) {
      const ctx = this.drawingCanvas[id].context;

      for (let i = 0; i < event.curve.length; i++) {
        this.erase(ctx, event.curve[i].x, event.curve[i].y);
      }
    }
  }

  startStroke(evt) {
    let ctx = this.drawingCanvas[this.mode].context;

    this.mouseX = evt.offsetX;
    this.mouseY = evt.offsetY;

    // update css zoom for Chrome
    this.zoom = this.slides.style.zoom ? 1.0 / this.slides.style.zoom : 1.0;

    // compensate for css-zoom
    this.mouseX = this.mouseX * this.zoom;
    this.mouseY = this.mouseY * this.zoom;

    this.xLast = this.mouseX;
    this.yLast = this.mouseY;

    if (evt.buttons == 2) {
      this.drawingCanvas[this.mode].canvas.style.cursor = this.laserCursor;
    } else if ((evt.buttons == 4) || this.eraseMode) {
      this.event = {
        type: "erase",
        curve: [{
          x: this.mouseX,
          y: this.mouseY
        }]
      };
      this.drawingCanvas[this.mode].canvas.style.cursor = this.eraserCursor;
      this.erase(ctx, this.mouseX, this.mouseY);
    } else {
      this.event = {
        type: "draw",
        curve: [{
          x: this.mouseX,
          y: this.mouseY
        }]
      };
      this.drawingCanvas[this.mode].canvas.style.cursor = this.pen[this.mode];
      this.draw[this.mode](ctx, this.xLast, this.yLast, this.mouseX, this.mouseY);
    }
  }

  continueStroke(evt) {
    if (this.event) {
      this.mouseX = evt.offsetX;
      this.mouseY = evt.offsetY;

      // compensate for css-zoom
      this.mouseX = this.mouseX * this.zoom;
      this.mouseY = this.mouseY * this.zoom;

      let ctx = this.drawingCanvas[this.mode].context;

      this.event.curve.push({
        x: this.mouseX,
        y: this.mouseY
      });

      if (
        this.mouseY < this.drawingCanvas[this.mode].height &&
        this.mouseX < this.drawingCanvas[this.mode].width
      ) {
        if (this.event.type == "erase") {
          this.erase(ctx, this.mouseX, this.mouseY);
        } else {
          this.draw[this.mode](ctx, this.xLast, this.yLast, this.mouseX, this.mouseY);
        }
        this.xLast = this.mouseX;
        this.yLast = this.mouseY;
      }
    }
  }

  stopStroke(evt) {
    if (this.event) {
      this.recordEvent(this.event);
      this.event = null;
    }

    // hide/reset cursor
    this.drawingCanvas[this.mode].canvas.style.cursor = this.pen[this.mode];
  }

  setupCallbacks() {
    let self = this;
    if (window.PointerEvent) {

      document.addEventListener('pointerdown', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          //console.log("pointerdown: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
          //console.log("ID: " + evt.pointerId);
          switch (evt.pointerType) {
            case "mouse":
            case "pen":
              self.startStroke(evt);
              break;

            case "touch":
              self.showLaser(evt);
              break;
          }
        }
      }, true);


      document.addEventListener('pointermove', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          if (evt.buttons > 0) {
            //console.log("pointermove: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
            switch (evt.pointerType) {
              case "mouse":
              case "pen":
                self.continueStroke(evt);
                break;

              case "touch":
                self.showLaser(evt);
                break;
            }
          }
        }
      });

      document.addEventListener('pointerup', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          //console.log("pointerup: " + evt.pointerType + ", " + evt.button + ", " + evt.buttons);
          switch (evt.pointerType) {
            case "mouse":
            case "pen":
              self.stopStroke(evt);
              break;

            case "touch":
              self.hideLaser();
              break;
          }
        }
      });

    } else {
      console.log("we do not have pointer events");

      document.addEventListener('mousedown', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          self.startStroke(evt);
        }
      }, true);


      document.addEventListener('mousemove', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          self.continueStroke(evt);
        }
      });


      document.addEventListener('mouseup', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          self.stopStroke(evt);
        }
      });

      document.addEventListener('touchstart', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          self.showLaser(evt);
        }
      }, true);

      document.addEventListener('touchmove', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          self.showLaser(evt);
        }
      });

      document.addEventListener('touchend', function (evt) {
        if (evt.target.getAttribute('data-chalkboard') == self.mode) {
          evt.preventDefault();
          self.hideLaser();
        }
      });
    }

    window.addEventListener("resize", function () {
      // Resize the canvas and draw everything again
      self.startPlayback(self.mode);
    });


    Reveal.addEventListener('ready', function (evt) {
      if (!self.printMode) {
        self.slideIndices = Reveal.getIndices();
        self.startPlayback(0);
      } else {
        self.whenReady(self.createPrintout);
      }
    });


    Reveal.addEventListener('slidechanged', function (evt) {
      if (!self.printMode) {
        self.slideIndices = Reveal.getIndices();
        self.closeChalkboard();
        self.clearCanvas(0);
        self.clearCanvas(1);
        self.startPlayback(0);
      }
    });


    Reveal.addEventListener('fragmentshown', function (evt) {
      if (!self.printMode) {
        self.slideIndices = Reveal.getIndices();
        self.closeChalkboard();
        self.clearCanvas(0);
        self.clearCanvas(1);
        self.startPlayback(0);
      }
    });


    Reveal.addEventListener('fragmenthidden', function (evt) {
      if (!self.printMode) {
        self.slideIndices = Reveal.getIndices();
        self.closeChalkboard();
        self.clearCanvas(0);
        self.clearCanvas(1);
        self.startPlayback();
        self.closeChalkboard();
      }
    });

    Reveal.addEventListener('slidechanged', self.updateIcon.bind(self));
    Reveal.addEventListener('fragmentshown', self.updateIcon.bind(self));
    Reveal.addEventListener('fragmenthidden', self.updateIcon.bind(self));
  }

  updateIcon() {
    if (!this.printMode) {
      let idx = Reveal.getIndices();
      if (this.hasSlideData(idx, 1)) {
        this.buttonC.style.color = "red";
      } else {
        this.buttonC.style.color = "lightgrey";
      }
    }
  }

  toggleNotesCanvas() {
    if (this.mode == 1) {
      this.toggleChalkboard();
    }

    if (this.notescanvas.style.pointerEvents != "none") {
      this.event = null;
      this.buttonN.style.color = "lightgrey";
      this.notescanvas.style.border = "none";
      this.notescanvas.style.pointerEvents = "none";
    } else {
      this.buttonN.style.color = "#2a9ddf";
      this.notescanvas.style.border = "1px solid " + this.color[0];
      this.notescanvas.style.pointerEvents = "auto";
    }
  }


  toggleChalkboard() {
    if (this.mode == 1) {
      this.event = null;
      this.closeChalkboard();
      this.chalkboard.style.pointerEvents = "none";
      this.buttonC.style.color = "lightgrey";
      this.updateIcon();
    } else {
      this.showChalkboard();
      this.chalkboard.style.pointerEvents = "auto";
      this.buttonC.style.color = "#2a9ddf";
    }
  }

  clear() {
    this.recordEvent({
      type: "clear"
    });
    this.clearCanvas(this.mode);
  }

  resetSlide(force) {
    const ok = force || confirm("Please confirm to delete chalkboard drawings on this slide!");
    if (ok) {
      this.event = null;
      this.closeChalkboard();

      this.clearCanvas(0);
      this.clearCanvas(1);

      this.mode = 1;
      let slideData = this.getSlideData();
      slideData.events = [];

      this.mode = 0;
      slideData = getSlideData();
      slideData.events = [];
    }
  }

  resetStorage(force) {
    const ok = force || confirm("Please confirm to delete all chalkboard drawings!");
    if (ok) {
      this.clearCanvas(0);
      this.clearCanvas(1);
      if (this.mode === 1) {
        this.event = null;
        this.closeChalkboard();
      }
      this.storage = [{
          width: this.drawingCanvas[0].width,
          height: this.drawingCanvas[0].height,
          data: []
        },
        {
          width: this.drawingCanvas[1].width,
          height: this.drawingCanvas[1].height,
          data: []
        }
      ];
    }
  }

  drawUndo() {
    if (this.hasSlideData(this.slideIndices, this.mode)) {
      const slideData = this.getSlideData(this.slideIndices, this.mode);
      slideData.events.pop();
      this.startPlayback(mode);
    }
  }
}

window.RevealChalkboard = new RevealChalkboard();