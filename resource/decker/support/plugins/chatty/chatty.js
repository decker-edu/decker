import setup from "../../chatty/chatty.js";

let Reveal;
let dialog;

function createGUI(deck) {
  // first check whether chatty is configured
  const server = window.Decker?.meta?.chatty?.server;
  const prompt = window.Decker?.meta?.chatty?.prompt;
  if (!server || !prompt) return;

  // create dialog
  dialog = document.createElement("dialog");
  dialog.id = "chatty-popover";
  dialog.setAttribute("closedby", "any");
  document.body.appendChild(dialog);
  dialog.onclick = (e) => {
    // workaround for stupid Safari
    if (e.target === e.currentTarget) {
      e.stopPropagation();
      dialog.close();
    }
  };

  // fill dialog with chatty content
  setup(dialog, Reveal);

  // create button
  let button = document.createElement("button");
  button.id = "chatty-button";
  button.title = button.ariaLabel =
    navigator.language === "de" ? "Prof. Bot fragen" : "Ask Prof. Bot";
  button.className = "fa-button fa-solid fa-robot";
  button.onclick = () => {
    dialog.showModal();
  };

  // place button
  if (!Reveal.hasPlugin("ui-anchors")) {
    console.error("no decker ui anchor plugin loaded");
  } else {
    Reveal.getPlugin("ui-anchors").placeButton(button, "TOP_RIGHT");
  }

  // toggle chatty with key c
  Reveal.addKeyBinding(
    {
      keyCode: 67,
      key: "C",
      description:
        navigator.language === "de"
          ? "Chatte mit Prof. Bot"
          : "Chat with Prof. Bot"
    },
    () => {
      dialog.showModal();
    }
  );
}

const Plugin = {
  id: "chatty",
  init: (deck) => {
    Reveal = deck;
    Reveal.on("ready", createGUI);
  },
  send: (input) => {
    dialog.sendToChatty(input);
    dialog.showModal();
  },
  show: () => {
    dialog.showModal();
  }
};

export default Plugin;
