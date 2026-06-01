import setup from "../../chatty/chatty.js";

let Reveal;
let dialog;

function createGUI(deck) {
  // not in PDF mode
  if (window.location.search.match(/print-pdf/gi)) return;

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

  // add test-me-button to last slide
  let lastSlide = document.querySelector(
    "div.reveal div.slides section:last-of-type"
  );
  if (lastSlide) {
    const lang = Decker.meta.lang || navigator.language;
    let button = document.createElement("button");
    lastSlide.appendChild(button);
    button.setAttribute(
      "askChatty",
      lang === "de" ? "Frag' mich ab!" : "Test me!"
    );
    button.innerText =
      lang === "de"
        ? "Stelle mir 5 Verständnisfragen zu dem aktuellen Foliensatz. Frage nicht zum Literaturverzeichnis. Stelle mir die Fragen nacheinander. Bewerte im Anschluss, wie gut ich den Foliensatz verstanden habe."
        : "Ask me 5 comprehension questions about the current slide deck. Do not ask about the listed literature. Ask me the questions one at a time. Afterwards, assess how well I understood the slide deck.";
  }

  // initialize per-slide button for triggering chatty
  document.querySelectorAll(".reveal button[askChatty]").forEach((b) => {
    const text = b.getAttribute("askChatty");
    const question = b.innerText.trim();
    if (question)
      b.onclick = () => {
        askChatty(question);
      };
    b.innerHTML = `<i class="fa-solid fa-robot"></i> &thinsp; ${text}`;
    b.classList.add("fa-button", "fa-solid");
    b.classList.add("glowing-border");
  });
}

async function wait(ms) {
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve();
    }, ms);
  });
}

window.askChatty = async (question) => {
  dialog.showModal();
  await wait(500);
  dialog.sendToChatty(question);
};

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
