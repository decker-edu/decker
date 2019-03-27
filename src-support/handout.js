import $ from "jquery";
require("@fortawesome/fontawesome-free/css/all.css");
require("@fortawesome/fontawesome-free/js/all");
require("bootstrap/dist/css/bootstrap.css");
require("./handout.scss");

document.addEventListener("load", () => {
  $("table").addClass(
    "table table-striped table-bordered table-hover table-condensed table-responsive"
  );
});

// Webpack handling of MathJax copied from
// https://github.com/mathjax/mathjax-v3/wiki/A-first-usable-demo-(using-webpack)
// the MathJax core
const MathJax = require("mathjax3/mathjax3/mathjax.js").MathJax;
// MathML input
const TeX = require("mathjax3/mathjax3/input/tex.js").TeX;
// HTML output
const CHTML = require("mathjax3/mathjax3/output/chtml.js").CHTML;
// Use browser DOM
const adaptor = require("mathjax3/mathjax3/adaptors/browserAdaptor").browserAdaptor();
// Register the HTML document handler
require("mathjax3/mathjax3/handlers/html.js").RegisterHTMLHandler(adaptor);
require("mathjax3/mathjax3/input/tex/ams/AmsConfiguration.js");
require("mathjax3/mathjax3/input/tex/base/BaseConfiguration.js");
require("mathjax3/mathjax3/input/tex/ams/AmsConfiguration.js");
require("mathjax3/mathjax3/input/tex/noundefined/NoUndefinedConfiguration.js");
require("mathjax3/mathjax3/input/tex/newcommand/NewcommandConfiguration.js");
require("mathjax3/mathjax3/input/tex/boldsymbol/BoldsymbolConfiguration.js");
require("mathjax3/mathjax3/input/tex/braket/BraketConfiguration.js");
require("mathjax3/mathjax3/input/tex/mhchem/MhchemConfiguration.js");
require("mathjax3/mathjax3/input/tex/physics/PhysicsConfiguration.js");
require("mathjax3/mathjax3/input/tex/verb/VerbConfiguration.js");
require("mathjax3/mathjax3/input/tex/cancel/CancelConfiguration.js");
require("mathjax3/mathjax3/input/tex/enclose/EncloseConfiguration.js");

// initialize mathjax with with the browser DOM document; other documents are possible
const html = MathJax.document(document, {
  InputJax: new TeX({
    inlineMath: [["$", "$"], ["\\(", "\\)"]],
    packages: [
      "base",
      "ams",
      "noundefined",
      "newcommand",
      "boldsymbol",
      "braket",
      "mhchem",
      "physics",
      "verb",
      "cancel",
      "enclose"
    ]
  }),
  OutputJax: new CHTML({
    fontURL:
      "https://cdn.rawgit.com/mathjax/mathjax-v3/3.0.0-alpha.4/mathjax2/css/"
  })
});

window.addEventListener("load", function() {
  console.time("wrapper");
  // process the document
  html
    .findMath()
    .compile()
    .getMetrics()
    .typeset()
    .updateDocument();
  console.timeEnd("wrapper");
});
