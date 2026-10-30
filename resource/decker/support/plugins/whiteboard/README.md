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
- Automatic hiding of mouse cursor after 3 seconds.
- Automatic saving when leaving the presentation, or a warning about unsaved annotations.


## Requirements

The whiteboard plugin requires a browser that supports [pointer events](https://caniuse.com/#feat=pointer) and that for optimal performance should also support [coalesced pointer events](https://caniuse.com/#feat=mdn-api_pointerevent_getcoalescedevents). Exporting additional whiteboard pages to PDF requires `pdfMaxPagesPerSlide` to be set to a sufficiently large value.

The plugin was tested on these configurations:

- Lenovo X1 Yoga running Linux
- MacBookPro+iPad with Apple Sidecar
- MacBookPro+iPad with [Astropad](https://astropad.com/)
- MacBookPro with Wacom Cintiq


## Usage

In Decker, the whiteboard only works in presenter mode (press `P` three times). The toggle button is in the lower left corner. While the whiteboard is on, that corner shows the current tool, and a click on it opens the panel with these buttons (from bottom to top):

- Toggle whiteboard on/off.
- Select laser pointer. You can also trigger the laser pointer in pen mode with the right mouse button or the button of a stylus.
- Select eraser tool. You can also erase in pen mode with the middle mouse button or the eraser tip of a stylus.
- Select pen tool.
- Pen settings: colors and widths.
- Undo the last action (up to 10 steps).
- Add a whiteboard page to the bottom of the current slide.
- Toggle background grid on/off for the additional whiteboard pages.
- Toggle automatic saving on/off.
- Save the annotations now.

We also have the following keyboard shortcuts:

- `w` toggles the whiteboard on/off.
- `e` toggles the eraser, `l` the laser pointer.
- `1` to `7` select a pen color, `8`, `9` and `0` a pen width.
- `Ctrl+z` or `Cmd+z` undoes the last action.
- `Delete` deletes all annotations on the current slide.

Saving puts `<deck>-annot.json` into your browser's download directory. With a Decker server, it also uploads the file next to the source of the deck, from where the next build copies it to `public/`. Without one, you have to copy the file there yourself.

With automatic saving, which is on by default, this happens when you leave the presentation, not on every change. Without it, the browser warns about unsaved annotations when you leave.

## Options

Set these below the meta data key `whiteboard`:

- `colors`: the list of pen colors. The keys `1` to `7` expect the 16 default colors.
- `autosave`: save when leaving the presentation, default is `true`.
- `autotoggle`: turn on when a stylus hovers over the slide, default is `false`.

## License

MIT licensed

Copyright (C) 2016 Asvin Goel\
Copyright (C) 2017-2020 Mario Botsch

