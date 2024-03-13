export function init() {
  const options = Decker?.meta?.math;
  if (!options?.mathjax) {
    console.error(
      "No MathJax source URI has been configured. This should not happen!",
      "The config.math.mathjax value is usually configured in your resource pack's 'deck.html' and 'page.html'",
      "Please contact the developers: https://github.com/decker-edu/decker"
    );
    return;
  }
  const url = options.mathjax + "tex-chtml.js";
  const script = document.createElement("script");
  script.type = "text/javascript";
  script.id = "MathJax-script";
  script.src = url;

  const macros = options.macros ? options.macros : {};

  window.MathJax = {
    loader: {
      failed: (error) => {
        console.error(error);
      },
    },
    startup: {
      ready: () => {
        if (Decker?.meta?.math?.callbacks) {
          for (const callback of Decker.meta.math.callbacks) {
            callback();
          }
        }
        //        console.log("MathJax startup ready() is done");
        window.MathJax.startup.defaultReady();
      },
    },
    svg: {
      scale: window.Decker.meta.math.scale || 1.0, // global scaling factor for all expressions
      minScale: 0.5, // smallest scaling factor to use
      mtextInheritFont: true, // true to make mtext elements use surrounding font
      merrorInheritFont: true, // true to make merror text use surrounding font
      mathmlSpacing: false, // true for MathML spacing rules, false for TeX rules
      skipAttributes: {}, // RFDa and other attributes NOT to copy to the output
      exFactor: 0.5, // default size of ex in em units
      displayAlign: "center", // default for indentalign when set to 'auto'
      displayIndent: "0", // default for indentshift when set to 'auto'
      fontCache: "none", // or 'global' or 'none'
      localID: null, // ID to use for local font cache (for single equation processing)
      internalSpeechTitles: true, // insert <title> tags with speech content
      titleID: 0, // initial id number to use for aria-labeledby titles
    },
    tex: {
      inlineMath: [
        // start/end delimiter pairs for in-line math
        ["$", "$"],
        ["\\(", "\\)"],
      ],
      displayMath: [
        // start/end delimiter pairs for display math
        ["$$", "$$"],
        ["\\[", "\\]"],
      ],
      macros: macros,
    },
    options: {
      enableMenu: false,
    },
  };

  document.documentElement.appendChild(script);
}
