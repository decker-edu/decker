# Whiteboard Plugin

This plugin adds an interactive board to [reveal.js](https://github.com/hakimel/reveal.js/). 
It was inspired by and initially based on version 0.6 of 
[Asvin Goel's chalkboard plugin](https://github.com/rajgoel/reveal.js-plugins/). 
Using this plugin, you can draw on your to add annotations, and you can append empty pages at the bottom of each slide, e.g., for longer derivations or more complex drawings. 


## Features

- Low-latency drawing by exploiting coalesced pointer events 
  (thanks to Martin Heistermann, Bern University).
- Crisp and resolution-independent annotations due to vector-based SVG rendering.
- Quadratic splines with endpoint interpolation yield smooth pen strokes.
- Support for multiple pen colors and stroke widths.
- Annotations can be exported to PDF (see [reveal.js PDF export](https://github.com/hakimel/reveal.js/#pdf-export)). 
- Automatric hiding of mouse cursor after 3 seconds.
- Show user warning when trying to close/leave the presentation with unsaved annotations.


## Requirements

The whiteboard plugin requires a browser that supports [pointer events](https://caniuse.com/#feat=pointer) and that for optimal performance should also support [coalesced pointer events](https://caniuse.com/#feat=mdn-api_pointerevent_getcoalescedevents). Exporting additional whiteboard pages to PDF requires `pdfMaxPagesPerSlide` to be set to a sufficiently large value.

The plugin was tested on these configurations:

- Lenovo X1 Yoga running Linux
- MacBookPro+iPad with Apple Sidecar
- MacBookPro+iPad with [Astropad](https://astropad.com/)
- MacBookPro with Wacom Cintiq


## Usage

The whiteboard plugin is mainly controlled through the buttons in the lower left corner (from bottom to top):
- Toggle whiteboard on/off.
- Save whiteboard annotations (see below).
- Add whiteboard page to the bottom of the current slide.
- Toggle background grid on/off for the additional whiteboard pages.
- Undo: deletes last pen stroke.
- Select pen tool. If already selected, show pen property dialog.
- Select eraser tool. You can also erase in pen-mode by using the middle-mouse button or using the eraser tip of the Wacom pen.
- Select laser pointer. You can also trigger the laser pointer in pen-mode by using the right mouse button.

We also have the following keyboard shortcuts:
- `w` toggles whiteboard on/off.
- `Ctrl+z` triggers undo, i.e., deltes last pen stroke.
- `Delete` deletes all annotations for the current slide.

Whiteboard annotations are saved to your browser's download directory, or, when used in combination with our [decker tool](https://gitlab2.informatik.uni-wuerzburg.de/decker/decker), to your slide deck directory. When not using decker, you have to copy the downloaded annotation file to the directory where your reveal.js presentation resides.


## License

MIT licensed

Copyright (C) 2016 Asvin Goel\
Copyright (C) 2017-2020 Mario Botsch

