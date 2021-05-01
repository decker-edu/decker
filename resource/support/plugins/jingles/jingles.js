"use strict";

let RevealJingle = (function () {

  const config = Reveal.getConfig().jingles || {};
  let jingleLayout = null;
  let jingles = [];


  function createJingle(filename, style) {
    const defaultStyle = "display:block; position:absolute; left:0; right:0; top:0; bottom:0; margin:auto; object-fit:contain; pointer-events:none;"

    let vid = document.createElement("video");
    vid.style = style ? defaultStyle+style : defaultStyle;
    vid.src = filename;
    vid.onplay = () => { jingleLayout.style.display = "flex"; vid.style.display = "block"; }
    vid.onended = () => { jingleLayout.style.display = vid.style.display = "none"; }
    jingleLayout.appendChild(vid);

    return vid;
  }

  function setupJingles() {
    // setup container
    const reveal = document.querySelector(".reveal");
    jingleLayout = document.createElement("div");
    jingleLayout.style = "display:flex; flex-flow: column nowrap; justify-content:center; position:fixed; left:0; right:0; top:0; bottom:0; margin:0; z-index:100; background:none;";
    reveal.appendChild(jingleLayout);

    // setup video element per jingle
    for (let i=0; i<config.length; i++) {
      jingles.push( createJingle(config[i].vid, config[i].css) );
    }
  }

  function playJingle(i) {
    jingles[i].play();
  }

  function setupKeyBindings() {
    const N = Math.min(9, jingles.length);
    for (let i = 0; i < N; i++) {
      Reveal.addKeyBinding(
        {
          keyCode: 49 + i,
          key: String.fromCharCode(49 + i),
          description: "Play jingle " + i,
        },
        () => {
          playJingle(i);
        }
      );
    }
  }

  return {
    init: function () {
      console.log("init jingle plugin");
      if (config.length) {
        setupJingles();
        setupKeyBindings();
      }
    },
  };
})();

Reveal.registerPlugin("jingle", RevealJingle);
