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
  // Put this into the load callback because very large pages might have not all content available for MathJax
  // to typeset before the script starts execution, which is very weird ... (even when not lazy)
  window.addEventListener("load", () => {
    const url = options.mathjax + "tex-svg.js";
    const script = document.createElement("script");
    script.type = "text/javascript";
    script.id = "MathJax-script";
    script.src = url;

    const macros = options.macros ? options.macros : {};

    let loadModules = ["[tex]/ams", "[tex]/action", "[tex]/color"];
    let lazyMargin = null;
    if (options?.lazy) {
      loadModules.push("ui/lazy");
      lazyMargin = options.lazyMargin ? options.lazyMargin : "256px";
    }

    window.MathJax = {
      loader: {
        load: loadModules,
        failed: (error) => {
          console.error(error);
        },
      },
      startup: {
        pageReady: () => {
          window.MathJax.typesetPromise().then(enableTypesetDetails);
          window.MathJax.startup.defaultPageReady();
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
        tags: "ams",
        packages: { "[+]": ["ams", "action", "color"] },
        macros: macros,
      },
      options: {
        skipHtmlTags: { "[+]": ["details"] },
        //        lazyMargin: "inserted programmatically down below"
        enableMenu: false,
      },
    };

    if (options?.lazy) {
      window.MathJax.options.lazyMargin = lazyMargin;
    }

    document.head.appendChild(script);
  });
}

function enableTypesetDetails() {
  window.MathJax.config.options.skipHtmlTags = null;
  window.MathJax.startup.getComponents();
  const details = document.body.querySelectorAll("details");
  for (let detail of details) {
    detail.addEventListener("toggle", async () => {
      if (detail.open) {
        try {
          await window.MathJax.typesetPromise([detail]);
        } catch (error) {
          console.error(error);
        }
      }
    });
  }
}
