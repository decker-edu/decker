import setup, {
  getChattyIdentity,
  getLocalizedText,
  renderIcon,
  setButtonIcon
} from "../../chatty/chatty.js";

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

const HEADING_SELECTOR = "h1, h2, h3, h4, h5, h6";
const DIRECT_HEADING_SELECTOR = HEADING_SELECTOR.split(", ")
  .map((selector) => `:scope > ${selector}`)
  .join(", ");

function currentLanguage() {
  return window.Decker?.meta?.lang || navigator.language;
}

function currentIdentity() {
  return getChattyIdentity(window.Decker?.meta?.chatty ?? {});
}

function launcherTooltip(id, defaults) {
  return getLocalizedText(
    id.launcher?.tooltip ?? id.tooltip,
    currentLanguage(),
    currentLanguage() === "de" ? defaults.de : defaults.en
  );
}

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
    return {
      ...DEFAULT_TEST_ME_CONFIG,
      active: config
    };
  }

  if (!config || typeof config !== "object") {
    return {
      ...DEFAULT_TEST_ME_CONFIG
    };
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

// determines where the automatically placed test-me button should be inserted
// if the target slide contains vertical slides, uses the first child slide
function getAutomaticTestMeInsertionTarget(targetSlide) {
  if (targetSlide?.matches?.("section")) {
    const firstChildSlide = targetSlide.querySelector(":scope > section");

    if (firstChildSlide) {
      return firstChildSlide;
    }
  }

  return targetSlide;
}

// applies the shared visual treatment for buttons that trigger chatty prompts
function decorateAskChattyButton(button, label) {
  const icon = document.createElement("span");
  renderIcon(icon, currentIdentity().launcher?.icon);
  button.replaceChildren(icon, document.createTextNode(` \u2009 ${label}`));
  button.classList.add("fa-button", "fa-solid", "glowing-border");
}

// creates a generated test-me button with its chatty prompt handler attached
function createTestMeButtonElement(label, question, { manual = false } = {}) {
  const button = document.createElement("button");
  button.type = "button";
  button.tabIndex = 0;
  button.classList.add("chatty-testme-button");
  if (manual) {
    button.classList.add("chatty-testme-button-manual");
  }
  button.setAttribute("askChatty", label);
  decorateAskChattyButton(button, label);

  button.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    askChatty(question);
  });

  return button;
}

// creates the shared wrapper used around generated test-me buttons
function createTestMeWrapper(button) {
  const wrapper = document.createElement("div");
  wrapper.className = "chatty-testme-wrapper";
  wrapper.appendChild(button);
  return wrapper;
}

// places the automatic test-me button at the configured target slide
function createAutomaticTestMeButton(targetSlide, label, question) {
  const insertionTarget = getAutomaticTestMeInsertionTarget(targetSlide);
  const button = createTestMeButtonElement(label, question);
  const wrapper = createTestMeWrapper(button);
  insertionTarget.appendChild(wrapper);
  return button;
}

// determines the appropriate target for the manual test-me button based on the provided marker element
// if the marker is inside a box with the class "testme-button", it returns that box as the target
// otherwise, it returns the marker itself as the target
function getManualTestMeTarget(marker) {
  return marker?.closest(".box.testme-button") ?? marker;
}

// determines the appropriate insertion reference for the manual test-me button based on the target container
// if the target container is a heading, it returns the target container itself as the reference
// otherwise, it looks for the first direct child heading of the target container and returns that as the reference
function getManualTestMeInsertionReference(targetContainer) {
  if (targetContainer?.matches?.(HEADING_SELECTOR)) {
    return targetContainer;
  }

  return targetContainer?.querySelector?.(DIRECT_HEADING_SELECTOR);
}

// checks if a manual test-me button already exists in the target container or after the insertion reference
// returns true if a button is found, false otherwise
function hasManualTestMeButton(targetContainer, insertionReference) {
  if (!targetContainer) {
    return true;
  }

  if (
    insertionReference?.nextElementSibling?.classList.contains(
      "chatty-testme-wrapper"
    )
  ) {
    return true;
  }

  return Boolean(
    targetContainer.querySelector(":scope > .chatty-testme-wrapper")
  );
}

// places a manual test-me button in a marked target container
// skips targets that already contain a generated test-me wrapper
function createManualTestMeButton(targetContainer, label, question) {
  const insertionReference = getManualTestMeInsertionReference(targetContainer);

  if (hasManualTestMeButton(targetContainer, insertionReference)) {
    return null;
  }

  const button = createTestMeButtonElement(label, question, { manual: true });
  const wrapper = createTestMeWrapper(button);

  if (insertionReference) {
    insertionReference.after(wrapper);
  } else {
    targetContainer.appendChild(wrapper);
  }

  return button;
}

// returns deduplicated manual test-me targets from Decker's generated DOM
function getManualTestMeTargets() {
  return new Set(
    Array.from(document.querySelectorAll(".reveal .testme-button")).map(
      getManualTestMeTarget
    )
  );
}

// initializes all manually marked test-me button containers in the deck
function initializeManualTestMeButtons() {
  const { label, question } = getTestMeTexts();

  getManualTestMeTargets().forEach((container) => {
    createManualTestMeButton(container, label, question);
  });
}

// initializes the automatically placed test-me button if enabled in meta
function initializeAutomaticTestMeButton() {
  const testMeConfig = getTestMeConfig();

  if (!testMeConfig.active) {
    return;
  }

  const targetSlide = getTestMeTargetSlide();

  if (targetSlide) {
    const { label, question } = getTestMeTexts();
    createAutomaticTestMeButton(targetSlide, label, question);
  }
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

// initializes author-defined buttons with the askChatty attribute
// generated test-me buttons are already fully wired when created
function initializeAskChattyButtons() {
  document
    .querySelectorAll(".reveal button[askChatty]:not(.chatty-testme-button)")
    .forEach((button) => {
      const text = button.getAttribute("askChatty");
      const question = button.innerText.trim();

      if (question) {
        button.onclick = () => {
          askChatty(question);
        };
      }

      decorateAskChattyButton(button, text);
    });
}

// creates the chatty GUI elements, including the dialog and button
// checks for PDF mode and chatty configuration before creating the GUI
// places the button using the ui-anchors plugin if available
// adds a key binding for toggling chatty with the "C" key
// initializes manual and automatic test-me buttons
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
  const id = currentIdentity();
  const button = document.createElement("button");
  button.id = "chatty-button";
  button.title = button.ariaLabel = launcherTooltip(id, {
    de: `${id.name} fragen`,
    en: `Ask ${id.name}`
  });
  button.className = "fa-button";
  setButtonIcon(button, id.launcher?.icon);

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
        currentLanguage() === "de"
          ? `Chatte mit ${id.name}`
          : `Chat with ${id.name}`
    },

    () => {
      dialog.showModal();
    }
  );

  // initialize explicit manual test-me blocks
  initializeManualTestMeButtons();

  // initialize the optional automatic test-me button
  initializeAutomaticTestMeButton();

  // initialize author-defined buttons for triggering chatty
  initializeAskChattyButtons();
}

async function wait(ms) {
  return new Promise((resolve) => {
    setTimeout(
      () => {
        resolve();
      },

      ms
    );
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
