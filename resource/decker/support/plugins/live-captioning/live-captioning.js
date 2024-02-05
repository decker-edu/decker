/**
 * Reveal Plugin that adds a black area to show a live transcription of the speaker's presentation.
 *
 * The speech recognition currently only works in Google Chrome (and maybe Safari).
 *
 * @author Sebastian Lukas Hauer
 */
import bwipjs from "../examiner/bwip.js";

const RESTART_INTERVAL = 2000; // Try to restart recognition if no event happens for two seconds.
const MINIMAL_RESTART_TIME = 2500; // If we just restarted skip the first retry.

let localization;

// Using the custom web component here is optional and can be replaced by something less
// bleeding edge
let button_template = document.createElement("template");
// button_template.innerHTML = String.raw`<button is="awesome-button" class="fa-button" icon="fa-closed-captioning" icon-style="fas" title="Activate Live Captioning" aria-label="Activate Live Captioning"></button>`;
button_template.innerHTML = String.raw`<button class="fa-button fas fa-closed-captioning" title="Activate Live Captioning" aria-label="Activate Live Captioning"></button>`;

let SpeechRecognitionImpl = undefined;

/* Check if feature is available
 * Usage tutorial: https://developer.mozilla.org/en-US/docs/Web/API/Web_Speech_API/Using_the_Web_Speech_API */
if (
  !!window.SpeechRecognition ||
  !(typeof webkitSpeechRecognition === "undefined")
) {
  SpeechRecognitionImpl = window.SpeechRecognition || webkitSpeechRecognition;
}

function getLanguageCode(string) {
  // Two Letter String: Probably already a language code
  if (string.length === 2) {
    return string;
  }
  // 3 letter language code: deu or eng
  if (string.length === 3) {
    switch (string) {
      case "deu":
        return "de-DE";
      case "eng":
        return "en-GB";
    }
    return undefined;
  }
  //BCP 47 language tag
  if (/^..-../i.test(string)) {
    return string;
  }
  //Language names
  // TODO: This whole system needs to be improved
  let code = undefined;
  switch (lang) {
    case "Deutsch":
    case "German":
      code = "de-DE";
      break;
    case "Englisch":
    case "English":
      code = "en-GB";
      break;
  }
  return code;
}

/**
 * Plugin class to be registered so the activation-(decker-)button is at the right place.
 */
class LiveCaptioning {
  constructor(reveal) {
    this.reveal = reveal;

    /* Attributes Decker Plugin Manager */
    this.record_button =
      button_template.content.cloneNode(true).firstElementChild;
    this.position = "TOP_RIGHT";

    this.record_button.addEventListener("click", () => this.toggleCaptioning());

    this.captioning = false;
    this.speechRecog = undefined;
    this.connection = undefined;

    this.defibrilator = undefined; // Timer to restart caption request if no data comes for too long.

    this.lastEvent = undefined;
    this.lastStart = undefined;

    /* Left for posterity if at some point the window-placement API is supported in browsers
     this.fullscreenCaptioning = false;
     this.primaryScreen = undefined;
     this.secondaryScreen = undefined;
     */
    this.popup = undefined;
  }

  isCaptioning() {
    return this.captioning;
  }

  async askPermission() {
    let options = [
      { text: localization.accept, value: "ACCEPT" },
      { text: localization.abort, value: "ABORT" },
    ];
    let choice = await window.showChoice(
      localization.caption_warning,
      options,
      "warning"
    );
    if (choice.submit !== "ACCEPT") {
      return false;
    } else {
      return true;
    }
  }

  /**
   * Sets up the speech recognition of the browser and starts it. Also opens
   * a new window to present the recognized text in.
   */
  async startCaptioning() {
    let permission = await this.askPermission();
    if (!permission) {
      return;
    }
    this.speechRecog = new SpeechRecognitionImpl();
    this.speechRecog.continuous = true;
    this.speechRecog.interimResults = true;
    if (Decker.meta["speech-recognition-language"]) {
      let code = getLanguageCode(Decker.meta["speech-recognition-language"]);
      if (code) {
        this.speechRecog.lang = code;
      } else {
        console.error(
          "[LIVE CAPTIONING] 'speech-recognition-language' specified but unable to discern valid language code from it."
        );
      }
    }
    this.speechRecog.onstart = () => this.handleStart();
    this.speechRecog.onresult = (event) => this.handleResult(event);
    this.speechRecog.onerror = (event) => this.handleError(event);
    this.speechRecog.onend = () => this.handleEnd();

    const url = new URL(import.meta.url);
    const path = url.pathname.substring(0, url.pathname.lastIndexOf("/"));

    this.popup = window.open(
      `${path + "/live-captioning.html"}?${Date.now()}`, //Add time to back to prevent caching
      "reveal.js - Captioning",
      "width=1920,height=1080"
    );
    try {
      const response = await fetch(path + "/live-captioning.html", {
        cache: "no-cache",
      });
      const html = await response.text();
      this.popup.document.write(html);
      this.popup.onbeforeunload = () => {
        this.popup = undefined;
        // this.stopCaptioning(); // DO NOT STOP, other clients might be listening
      };
    } catch (error) {
      console.log(error);
    }

    let backend = window.Decker.meta["caption-server"];
    if (backend) {
      try {
        const response = await fetch(backend + "/api/session", {
          method: "POST",
        });
        const json = await response.json();
        if (json.session && json.token) {
          this.connection = json;
          if (!this.connection.server) {
            this.connection.server = backend;
          }
          this.connection.cors =
            window.location.origin !== new URL(this.connection.server).origin
              ? "cors"
              : "same-origin";
          this.showQRCode();
        }
      } catch (error) {
        console.error(error);
      }
    }

    this.speechRecog.start();
    this.captioning = true;
    this.record_button.classList.add("recording");
    this.record_button.title = localization.stop_captioning;
    this.record_button.setAttribute("aria-label", localization.stop_captioning);
  }

  async showQRCode() {
    const container = document.createElement("div");
    const qrcode = document.createElement("canvas");
    bwipjs.toCanvas(qrcode, {
      bcid: "qrcode",
      text: `${this.connection.server}/session/${this.connection.session}`,
      scale: 10,
      includetext: false,
      textxalign: "center",
      eclevel: "L",
    });
    container.appendChild(qrcode);
    const linkContainer = document.createElement("div");
    container.appendChild(linkContainer);
    const link = document.createElement("a");
    linkContainer.appendChild(link);
    link.innerText = `${this.connection.server}/session/${this.connection.session}`;
    link.href = `${this.connection.server}/session/${this.connection.session}`;
    link.target = "_blank";
    container.style.display = "flex";
    container.style.flexDirection = "column";
    container.style.gap = "1rem";
    container.style.alignItems = "center";
    let noop = await window.showInformation(
      localization.qrcode_message,
      container
    );
  }

  async showAbort() {
    const container = document.createElement("div");
    const qrcode = document.createElement("canvas");
    bwipjs.toCanvas(qrcode, {
      bcid: "qrcode",
      text: `${this.connection.server}/session/${this.connection.session}`,
      scale: 10,
      includetext: false,
      textxalign: "center",
      eclevel: "L",
    });
    container.appendChild(qrcode);
    const linkContainer = document.createElement("div");
    container.appendChild(linkContainer);
    const link = document.createElement("a");
    linkContainer.appendChild(link);
    link.innerText = `${this.connection.server}/session/${this.connection.session}`;
    link.href = `${this.connection.server}/session/${this.connection.session}`;
    link.target = "_blank";
    container.style.display = "flex";
    container.style.flexDirection = "column";
    container.style.gap = "1rem";
    container.style.alignItems = "center";
    let options = [
      { text: localization.stop, value: "true" },
      { text: localization.abort, value: "false" },
    ];
    return window.showDialog(
      "Untertitelung beenden?",
      container,
      options,
      "warning"
    );
  }

  /**
   * Stops the captioning and closes the popup if available.
   */
  stopCaptioning() {
    this.captioning = false;

    this.speechRecog.stop();
    if (this.popup) {
      this.popup.close();
      this.popup = undefined;
    }
    this.record_button.classList.remove("recording");
    this.record_button.title = localization.start_captioning;
    this.record_button.setAttribute(
      "aria-label",
      localization.start_captioning
    );
  }

  toggleCaptioning() {
    if (!this.captioning) {
      this.startCaptioning();
    } else {
      this.showAbort().then((data) => {
        if (data && data.submit === "true") {
          this.stopCaptioning();
        }
      });
    }
  }

  handleStart() {
    this.lastEvent = Date.now();
    this.lastStart = Date.now();
    this.defibrilator = setInterval(() => this.defibrilate(), RESTART_INTERVAL);
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "status",
          value: "start",
        })
      );
    }
  }

  /**
   * Stop the speechRecognition if no data comes in for a while.
   * The handleEnd() function should then restart the recognition as if an error had occurred.
   */
  defibrilate() {
    const now = Date.now();
    const timeSinceLastEvent = now - this.lastEvent;
    const timeSinceLastStart = now - this.lastStart;
    if (
      timeSinceLastEvent > RESTART_INTERVAL &&
      timeSinceLastStart > MINIMAL_RESTART_TIME
    ) {
      this.speechRecog.abort();
    }
  }

  /**
   * Updates the current span if result is not final,
   * or finalizes the current one and creates a new one.
   * @param {*} event
   */
  handleResult(event) {
    this.lastEvent = Date.now();
    for (var i = event.resultIndex; i < event.results.length; i++) {
      if (event.results[i][0].confidence > 0.1) {
        this.updateCaptionContent(event.results[i][0].transcript);
        if (event.results[i].isFinal) {
          this.finalizeCaptionContent(event.results[i][0].transcript);
        }
      }
    }
  }

  /**
   * If an error occurs we just finalize the current span and log the error.
   * @param {*} event
   */
  handleError(event) {
    if (!(event.error === "aborted")) {
      console.error(event);
      if (this.popup) {
        this.popup.postMessage(
          JSON.stringify({
            type: "status",
            value: "inactive",
          })
        );
      }
      this.finalizeCaptionContent(this.currentSpan?.innerHTML);
    }
  }

  /**
   * Finalize the current span and restart the recognition if we still
   * want to caption.
   */
  handleEnd() {
    clearInterval(this.defibrilator);
    this.finalizeCaptionContent(this.currentSpan?.innerHTML);
    if (this.captioning) {
      this.speechRecog.start();
    }
  }

  /**
   * Sends data to the popup to update their current span and also informs
   * the connected syncronization service of the update.
   * @param {*} text
   * @returns Nothing
   */
  updateCaptionContent(text) {
    if (!text) return;
    if (this.connection) {
      try {
        fetch(
          `${this.connection.server}/api/session/${this.connection.session}/update`,
          {
            mode: this.connection.cors,
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({ token: this.connection.token, text: text }),
          }
        );
      } catch (error) {
        console.error(error);
      }
    }
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "update",
          text: text,
        })
      );
      return;
    }
  }

  /**
   * Updates the content of the current span and creates it if none exists.
   * Also deletes the reference to it so a new one gets created in the next round.
   * Sends data to the popup and informs the syncronization service about the finalization.
   * @param {*} text
   * @returns Nothing
   */
  finalizeCaptionContent(text) {
    if (!text) return;
    if (this.connection) {
      try {
        fetch(
          `${this.connection.server}/api/session/${this.connection.session}/final`,
          {
            mode: this.connection.cors,
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({ token: this.connection.token, text: text }),
          }
        );
      } catch (error) {
        console.error(error);
      }
    }
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "final",
          text: text,
        })
      );
      return;
    }
  }

  addCCButton() {
    let anchors = this.reveal.getPlugin("ui-anchors");
    if (anchors) {
      anchors.placeButton(this.record_button, this.position);
    }
  }

  removeCCButton() {
    this.record_button.parentElement.removeChild(this.record_button);
  }
}

const plugin = () => {
  return {
    id: "live-captioning",
    init(reveal) {
      const instance = new LiveCaptioning(reveal);

      if (!SpeechRecognitionImpl) {
        console.error("SpeechRecognition not available in this browser.");
        return;
      }

      localization = {
        start_captioning: "Start Live Captioning",
        stop_captioning: "Stop Live Captioning",
        accept: "Accept",
        stop: "Stop Captioning",
        abort: "Cancel",
        qrcode_message: "Live Captioning",
        caption_warning:
          "In order to use the live captioning function you will use integrated plugins of your browser. \
          In case you are using the live captioning function the data necessary for transcription will be handled by the installed plugin. \
          Decker will not take responsibility for the use and processing of that data by the installed plugin.",
      };
      let lang = navigator.language;
      if (lang === "de") {
        localization = {
          start_captioning: "Live-Untertitelung aktivieren",
          stop_captioning: "Live-Untertitelung stoppen",
          accept: "Akzeptieren",
          stop: "Untertitelung beenden",
          abort: "Abbrechen",
          qrcode_message: "Live-Untertitel",
          caption_warning:
            "Sie können für die Live-Untertitelung die von Ihrem Browser bereitgestellten Plugins nutzen. \
            In diesem Fall wird die Verarbeitung der Daten durch das von Ihnen installierte Plugin und nicht durch Decker vorgenommen. \
            Decker übernimmt in diesem Fall keine Verantwortung für die Verarbeitung der Daten durch das von Ihnen installierte Plugin.",
        };
      }

      reveal.addEventListener("ready", () => {
        Decker.addPresenterModeListener((on) => {
          if (on) {
            instance.addCCButton();
          } else {
            instance.removeCCButton();
          }
        });
      });
    },
  };
};

export default plugin;
