import setup from "../../chatty/chatty.js";

let Reveal;
let dialog;

// default configuration for the test-me button
// can be overridden by window.Decker.meta.chatty.testme
// example: { active: true, slideOffsetFromEnd: 1, slideOffsetJustHorizontal: true }
// active: whether the button should be created at all
// slideOffsetFromEnd: how many slides from the end the button should be placed (0 = last slide, 1 = second to last slide, etc.)
// slideOffsetJustHorizontal: whether to only consider horizontal slides when counting from the end
const DEFAULT_TEST_ME_CONFIG = {
  active: false,
  slideOffsetFromEnd: 0,
  slideOffsetJustHorizontal: false
};

// helper function to parse boolean values from strings or other types
// returns the fallback value if the input cannot be parsed as a boolean
function parseBoolean(value, fallback) {
  if (typeof value === "boolean") {
    return value;
  }

  if (typeof value === "string") {
    const normalized = value.trim().toLowerCase();
    if (["true", "1", "yes", "on"].includes(normalized)) {
      return true;
    }
    if (["false", "0", "no", "off"].includes(normalized)) {
      return false;
    }
  }

  return fallback;
}

// retrieves the configuration for the test-me button from window.Decker.meta.chatty.testme
// returns the default configuration if not set or invalid
// ensures that the returned configuration is always an object with the expected properties
function getTestMeConfig() {
  const config = window.Decker?.meta?.chatty?.testme;

  if (typeof config === "boolean") {
    return { ...DEFAULT_TEST_ME_CONFIG, active: config };
  }

  if (!config || typeof config !== "object") {
    return { ...DEFAULT_TEST_ME_CONFIG };
  }

  const offset = Number.parseInt(
    config["slide-offset-from-end"] ?? config.slideOffsetFromEnd,
    10
  );

  return {
    ...DEFAULT_TEST_ME_CONFIG,
    active: parseBoolean(config.active, true),
    slideOffsetFromEnd: Number.isFinite(offset) ? offset : 0,
    slideOffsetJustHorizontal: parseBoolean(
      config["slide-offset-just-horizontal"] ??
        config.slideOffsetJustHorizontal,
      false
    )
  };
}

// determines the target slide for placing the test-me button based on the configuration
// returns the last slide if the configuration is invalid or the target slide cannot be found
// uses Reveal.getHorizontalSlides() if slideOffsetJustHorizontal is true, otherwise uses Reveal.getSlides()
// returns the last slide if the calculated index is out of bounds
// falls back to the last slide in the DOM if Reveal.getSlides() is not available
// returns null if no slides are found
function getTestMeTargetSlide() {
  const config = getTestMeConfig();
  const slides =
    config.slideOffsetJustHorizontal && Reveal?.getHorizontalSlides
      ? Reveal.getHorizontalSlides()
      : Reveal?.getSlides?.();

  if (slides?.length) {
    const index = slides.length - 1 - config.slideOffsetFromEnd;
    if (index >= 0 && index < slides.length) {
      return slides[index];
    }
  }

  return document.querySelector("div.reveal div.slides > section:last-of-type");
}

// determines the appropriate insertion target for the test-me button
// if the target slide is a section, it looks for the first child section to insert into
// otherwise, it returns the target slide itself
function getTestMeInsertionTarget(targetSlide) {
  if (targetSlide?.matches?.("section")) {
    const firstChildSlide = targetSlide.querySelector(":scope > section");
    if (firstChildSlide) {
      return firstChildSlide;
    }
  }

  return targetSlide;
}

// creates the test-me button and appends it to the target slide
// sets the button's label and question based on the provided parameters
// adds a click event listener to trigger the askChatty function with the question
function createTestMeButton(targetSlide, label, question) {
  const insertionTarget = getTestMeInsertionTarget(targetSlide);
  const wrapper = document.createElement("div");
  wrapper.className = "chatty-testme-wrapper";
  insertionTarget.appendChild(wrapper);

  const button = document.createElement("button");
  button.type = "button";
  button.tabIndex = 0;
  button.setAttribute("askChatty", label);
  button.innerText = question;
  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    askChatty(question);
  });
  wrapper.appendChild(button);
  return button;
}

// retrieves the appropriate label and question for the test-me button based on the current language
// returns an object with the label and question properties
// defaults to English if the language is not German
function getTestMeTexts() {
  const isGerman = (window.Decker?.meta?.lang || navigator.language) === "de";
  return {
    label: isGerman ? "Frag' mich ab!" : "Test me!",
    question: isGerman
      ? "Stelle mir 5 Verständnisfragen zu dem aktuellen Foliensatz. Frage nicht zum Literaturverzeichnis. Stelle mir die Fragen nacheinander. Bewerte im Anschluss, wie gut ich den Foliensatz verstanden habe."
      : "Ask me 5 comprehension questions about the current slide deck. Do not ask about the listed literature. Ask me the questions one at a time. Afterwards, assess how well I understood the slide deck."
  };
}

// initializes buttons with the askChatty attribute on the current slide
// sets the button's label and question based on the askChatty attribute and inner text
// adds a click event listener to trigger the askChatty function with the question
function initializeAskChattyButtons() {
  document.querySelectorAll(".reveal button[askChatty]").forEach((button) => {
    const text = button.getAttribute("askChatty");
    const question = button.innerText.trim();
    if (question) {
      button.onclick = () => {
        askChatty(question);
      };
    }
    button.innerHTML = `<i class="fa-solid fa-robot"></i> &thinsp; ${text}`;
    button.classList.add("fa-button", "fa-solid", "glowing-border");
  });
}

// creates the chatty GUI elements, including the dialog and button
// checks for PDF mode and chatty configuration before creating the GUI
// places the button using the ui-anchors plugin if available
// adds a key binding for toggling chatty with the "C" key
// creates a test-me button on a configurable slide if enabled in the configuration
function createGUI() {
  // not in PDF mode
  if (window.location.search.match(/print-pdf/gi)) return;

  // first check whether chatty is configured
  const { server, prompt } = window.Decker?.meta?.chatty ?? {};
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
  const button = document.createElement("button");
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

  // add test-me-button to a configurable slide from the end
  const testMeConfig = getTestMeConfig();
  if (testMeConfig.active) {
    const targetSlide = getTestMeTargetSlide();
    if (targetSlide) {
      const { label, question } = getTestMeTexts();
      createTestMeButton(targetSlide, label, question);
    }
  }

  // initialize per-slide button for triggering chatty
  initializeAskChattyButtons();
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
