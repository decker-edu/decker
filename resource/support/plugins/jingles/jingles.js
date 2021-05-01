"use strict";

let RevealJingle = (function () {

  const config = Reveal.getConfig().jingles || {};
  let jingleLayout = null;
  let jingles = [];


  function createJingle(filename, style, volume) {
    console.log("  add jingle " + filename + " with volume " + (volume?volume:"1.0"));

    const defaultStyle = "display:none; position:absolute; left:0; right:0; top:0; bottom:0; margin:auto; object-fit:contain; pointer-events:none;"

    let vid = document.createElement("video");
    vid.style = style ? defaultStyle+style : defaultStyle;
    if (volume) vid.volume = volume;
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
    jingleLayout.style = "display:none; flex-flow: column nowrap; justify-content:center; position:fixed; left:0; right:0; top:0; bottom:0; margin:0; z-index:40; background:none; pointer-events:none;";
    reveal.appendChild(jingleLayout);

    // setup video element per jingle
    for (let i=0; i<config.length; i++) {
      jingles.push( createJingle(config[i].vid, config[i].css, config[i].vol) );
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
          description: "Play jingle " + (i+1),
        },
        () => {
          playJingle(i);
        }
      );
    }
  }

  return {
    init: function () {
      console.log("initialize jingles plugin");
      if (config.length) {
        setupJingles();
        setupKeyBindings();
      }
    },
  };
})();

Reveal.registerPlugin("jingle", RevealJingle);
