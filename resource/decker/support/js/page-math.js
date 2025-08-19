import { injectMathJaxCSS, configureMathJax, loadMathJax } from "./mathjax.js";

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

export function init() {
  window.addEventListener("load", () => {
    injectMathJaxCSS();
    configureMathJax();

    window.MathJax.startup.ready = () => {
      const startTime = Date.now();
      console.log("mathjax: start");
      MathJax.startup.defaultReady();
      MathJax.startup.promise.then(() => {
        enableTypesetDetails();
        const endTime = Date.now();
        const timeTaken = endTime - startTime;
        console.log(`mathjax: done (took ${timeTaken} ms)`);
      });
    };
    window.MathJax.options.skipHtmlTags = { "[+]": ["details"] };

    loadMathJax();
  });
}

init();
