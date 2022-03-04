// reference to Reveal object
let Reveal;

let config;
let jingleLayout = null;
let jingles = [];

function createJingle(filename, style, volume) {
  console.log(
    "  add jingle " + filename + " with volume " + (volume ? volume : "1.0")
  );

  const defaultStyle =
    "display:none; position:absolute; left:0; right:0; top:0; bottom:0; width:100%; height:100%; margin:auto; object-fit:contain; pointer-events:none;";

  let vid = document.createElement("video");
  vid.style = style ? defaultStyle + style : defaultStyle;
  if (volume) vid.volume = volume;
  vid.src = filename;
  vid.onplay = () => {
    jingleLayout.style.display = "flex";
    vid.style.display = "block";
  };
  vid.onended = () => {
    jingleLayout.style.display = vid.style.display = "none";
  };
  jingleLayout.appendChild(vid);

  return vid;
}

function setupJingles() {
  // setup container
  const reveal = Reveal.getRevealElement();
  jingleLayout = document.createElement("div");
  jingleLayout.style =
    "display:none; flex-flow: column nowrap; justify-content:center; position:fixed; left:0; right:0; top:0; bottom:0; margin:0; z-index:40; background:none; pointer-events:none;";
  reveal.appendChild(jingleLayout);

  // setup video element per jingle
  for (let i = 0; i < config.length; i++) {
    jingles.push(createJingle(config[i].vid, config[i].css, config[i].vol));
  }
}

function playJingle(i) {
  for (let j = 0; j < jingles.length; j++) if (j != i) jingles[j].pause();
  if (i < jingles.length) {
    jingles[i].currentTime = 0;
    jingles[i].play();
  }
}

function setupKeyBindings() {
  window.addEventListener("keydown", function (evt) {
    if (evt.shiftKey && evt.ctrlKey && evt.altKey) {
      switch (evt.code) {
        case "Digit1":
          playJingle(0);
          evt.preventDefault();
          evt.stopPropagation();
          break;

        case "Digit2":
          playJingle(1);
          evt.preventDefault();
          evt.stopPropagation();
          break;

        case "Digit3":
          playJingle(2);
          evt.preventDefault();
          evt.stopPropagation();
          break;

        case "Digit4":
          playJingle(3);
          evt.preventDefault();
          evt.stopPropagation();
          break;

        case "Digit5":
          playJingle(4);
          evt.preventDefault();
          evt.stopPropagation();
          break;
      }
    }
  });
}

const Plugin = {
  id: "jingles",
  init: (deck) => {
    Reveal = deck;
    config = Reveal.getConfig().jingles || {};
    if (config.length) {
      setupJingles();
      setupKeyBindings();
    }
  },
};

export default Plugin;
