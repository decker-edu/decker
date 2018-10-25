require('reveal.js/lib/js/head.min.js');
Reveal = require('reveal.js/js/reveal');
require('reveal.js/css/reveal.scss');
require('reveal.js/css/theme/white.css');
require('./decker.scss');

if(window.location.search.match( /print-pdf/gi )){
  require('reveal.js/css/print/pdf.css');
} else {
  require('reveal.js/css/print/paper.css');
}