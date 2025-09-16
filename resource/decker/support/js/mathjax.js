/*
 * Inject CSS rules that
 *  - make SVG equations automatically shrink down to fit the enclosing container,
 *  - remove pointer events from the equation parts, such that zoom plugin works on equations,
 *  - adjust colors of the math explorer's highlighting.
 */
export function injectMathJaxCSS() {
  const style = document.createElement("style");
  style.textContent = String.raw`
    /* fit equation into container (disable for tables) */
    mjx-container > svg:not(table mjx-container > svg) {
        object-fit: contain;
        max-width: 100%;
    }

    /* don't (dbl)click/zoom SVG interiors */
    mjx-container > svg * {
        pointer-events: none;
    }

    /* eqn refs have to be clickable */
    mjx-container > svg a, 
    mjx-container > svg a * {
        pointer-events: all;
    }

    /* adjust color for focus highlighting */
    mjx-container rect[sre-highlighter-added="true"] {
      fill: rgb(from var(--focus-color) r g b / 20%) !important;
      stroke: var(--focus-color) !important;
      & ~ g {
        fill: var(--foreground-color);
      }
    }

    /* adjust color of help icon */
    mjx-help {
      & circle {
        fill: rgb(from var(--focus-color) r g b / 20%) !important;
        stroke: var(--focus-color);
      }
      & line {
        fill: var(--foreground-color);
        stroke: var(--foreground-color);
      }
    }

    mjx-help-background {
        z-index: 128;
    }
    `;
  document.head.append(style);
}

export function configureMathJax() {
  // get mathjax config
  const options = Decker?.meta?.math;
  if (!options) console.error("MathJax not configured. Should not happen.");

  // from where to load mathjax
  const mathjax = Decker.meta.supportPath + "/vendor/mathjax/";

  // is initial a11y mode requested?
  const a11y = /a11y/gi.test(window.location.search);

  // language used for speech
  const language = Decker.meta.lang || navigator.language;

  // user-defined macros + \fragment{...} funtion
  let macros = { fragment: ["\\class{fragment}{#1}", 1] };
  if (options.macros) {
    macros = Object.assign(macros, options.macros);
  }

  // MathJax configuration
  window.MathJax = {
    loader: {
      load: ["[tex]/action", "[tex]/color"],
    },
    startup: {
      ready: () => {
        console.error("override mathjax startup");
      },
    },
    svg: {
      fontCache: "local", // or 'global' or 'none'
    },
    output: {
      font: "mathjax-" + (options.font || "newcm"),
      fontPath: mathjax + "/fonts/%%FONT%%-font",
      scale: window.Decker.meta.math.scale || 1.0, // global scaling factor for all expressions
      mtextInheritFont: true, // true to make mtext elements use surrounding font
      merrorInheritFont: true, // true to make merror text use surrounding font
    },
    tex: {
      tags: "ams",
      packages: { "[+]": ["action", "color"] },
      macros: macros,
      inlineMath: { "[+]": [["$", "$"]] },
    },
    options: {
      // skipHtmlTags: { "[+]": ["details"] },
      enableMenu: a11y,
      //      enableExplorer: a11y,
      menuOptions: {
        settings: {
          enrich: a11y, // true to enable semantic-enrichment
          //          collapsible: false, // true to enable collapsible math
          //          speech: a11y, // true to enable speech generation
          //          braille: a11y, // true to enable Braille generation
          //          assistiveMml: false, // true if hidden assistive MathML should be generated for screen readers
        },
      },
      a11y: {
        //        speech: a11y, // true to enable speech generation
        //        braille: a11y, // true to enable Braille generation
      },
      sre: {
        locale: language === "de" ? "de" : "en",
      },
    },
  };
}

export function loadMathJax() {
  const mathjax = Decker.meta.supportPath + "/vendor/mathjax/";
  const url = mathjax + `tex-svg-nofont.js`;
  const script = document.createElement("script");
  script.src = url;
  document.head.appendChild(script);
}
