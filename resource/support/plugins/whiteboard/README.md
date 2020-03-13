# Whiteboard Plugin

This plugin adds an interactive board to reveal.js. It is based on version 0.6 of 
[Asvin Goel's chalkboard plugin](https://github.com/rajgoel/reveal.js-plugins/).

Using this plugin, you can annotate your slides and use an empty whiteboard behind each slide for longer derivations or more complex drawings. These two modes are called *slide annotations* and *whiteboard drawings* in the following.

The whiteboard plugin is tested using a Lenovo X1 Yoga running Linux and a MacBook with Astropad.


## Usage

- 'w' and board icon on bottom left show/hide the whiteboard behind each slide.
  When there are drawings on the board, this icon will be marked red to inform users about it.
- 'd' and pen icon on bottom left enable/disable drawing on the slide or the board.
- 'e' and eraser icon on bottom left toggle eraser mode.
- hold pen icon for 1s to show color dialog.
- use left mouse or pen for drawing.
- use right mouse or pen-with-button-pressed for erasing.
- use touch events for controlling the virtual laser pointer.
- 'z' un-does last stroke.
- 'Enter' extends the board by one page height.
- 'DEL' deletes all drawings for the current slide.
- The notes icon in the bottom right menu downloads the annotations and drawings to a JSON file. 
  Copy this file into the folder of your HTML presentation 
  to have the scribbles auto-loaded when starting the presentation.


## Changes to Asvin Goel's original version

- Removed recording drawings and automatic playback.
- Removed network-transmitting drawings to another client.
- Removed original chalkboard effect to get cleaner drawing.
- Support for mouse events, touch events, pointer events (most efficient), and (somewhat) iPad stylus.
- More responsive drawing by exploiting Chrome's coalesced pointer events 
  (thanks to Martin Heistermann, Bern University!).
- Support for highDPI displays (thanks to Martin Heistermann). 
- Curves are rendered as Bezier curves to achieve a smoother appearance.
- Drawing supports multiple colors.
- Whiteboard drawings are more consistent under window rescaling.
- Both slide annotations and board drawings are exported to PDF.
- Cursor auto-hide, cool eraser cursor, laser pointer cursor in current pen color.
- Board can be enlarged by pressing `Return` for even longer derivations (thanks to Markus Nebel for the idea!).
- Prevent leaving presentation without saving board drawings.
- Supports latest reveal API and plugin structure.



## License

MIT licensed

Copyright (C) 2016 Asvin Goel\
Copyright (C) 2017-2019 Mario Botsch

CSS-based light saber adapted from https://codepen.io/ncerminara/pen/KzurJ
Light saber sounds from https://www.soundboard.com/sb/LightsaberSounds
