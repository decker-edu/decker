export default setup;
import "./marked.min.js";

// config
let server;
let prompt;

// access to Reveal
let Reveal;

// HTML elements
let dialog;
let chatEl;
let promptEl;
let sendBtn;
let stopBtn;

// globals
let abortController = null;
let previous_response_id = null;

// localization
const germanLocalization = {
  send: "Senden",
  stop: "Stop",
  question: "Frage eingeben…",
  greeting:
    "Ich bin **Prof. Bot**, dein KI-basierter Tutor. Du kannst mir Fragen zu den Vorlesungsinhalten stellen. *Aber Vorsicht: Meine Antworten können auch falsch sein.*",
};
const englishLocalization = {
  send: "Send",
  stop: "Stop",
  question: "Enter question…",
  greeting:
    "I'm **Prof. Bot**, your AI-based tutor. You can ask questions related to the course material. *But be aware that my answers might be wrong.*",
};
const lang = Decker.meta.lang || navigator.language;
const l10n = lang === "de" ? germanLocalization : englishLocalization;

function setup(anchor, reveal) {
  // are we running in a slide deck?
  if (reveal) Reveal = reveal;

  // get server and prompt from config
  server = window.Decker?.meta?.chatty?.server;
  prompt = window.Decker?.meta?.chatty?.prompt;
  if (!server || !prompt) return;

  // setup GUI
  anchor.innerHTML = `
  <div id="chatty-dialog">
    <div id="chat" aria-live="polite"></div>
      <div class="row">
      <textarea id="prompt" autofocus placeholder="${l10n.question}"></textarea>
      <div class="col">
        <button id="send">${l10n.send}</button>
        <button id="stop" disabled>${l10n.stop}</button>
      </div>
    </div>
  </div>`;

  // get elements
  dialog = document.getElementById("chatty-dialog");
  chatEl = document.getElementById("chat");
  promptEl = document.getElementById("prompt");
  sendBtn = document.getElementById("send");
  stopBtn = document.getElementById("stop");

  // inject CSS
  const style = document.createElement("link");
  style.rel = "stylesheet";
  style.type = "text/css";
  style.href = import.meta.url.replace("chatty.js", "chatty.css");
  document.head.appendChild(style);

  // button callbacks
  sendBtn.onclick = send;
  stopBtn.onclick = () => abortController?.abort();

  // keyboard callbacks
  promptEl.onkeydown = (e) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      send();
    } else if (e.key === "Escape") {
      closeDialog();
    }
    e.stopPropagation();
  };
  promptEl.onkeypress = (e) => {
    e.stopPropagation(); // prevent key '?' from toggling help dialog
  };

  // details open callback (autofocus doesn't work on index page)
  const details = dialog.closest("details");
  if (details) {
    details.addEventListener("toggle", () => {
      if (details.open) {
        promptEl.focus();
      }
    });
  }

  // post initial bot message
  newMessage("bot").add(l10n.greeting);
}

function newMessage(role) {
  const wrap = document.createElement("div");
  wrap.className = `msg ${role}`;
  wrap.innerHTML = `<div class="bubble"><div class="content"></div></div>`;

  const content = wrap.querySelector(".content");
  content.add = (text) => {
    addToMessage(content, text);
  };

  chatEl.appendChild(wrap);
  chatEl.scrollTop = chatEl.scrollHeight;
  return content;
}

async function addToMessage(msg, text) {
  // protect math content
  const tokens = [];
  const pattern = /\\\([\s\S]*?\\\)|\\\[[\s\S]*?\\\]|\$\$[\s\S]*?\$\$/g;
  const protectedText = text.replace(pattern, (m) => {
    const key = `@@MATH_${tokens.length}@@`;
    tokens.push(m);
    return key;
  });

  // convert markdown text to html
  let html = marked.parse(protectedText, {
    mangle: false,
    headerIds: false,
  });

  // restore math content
  html = html.replace(/@@MATH_(\d+)@@/g, (_, i) => tokens[Number(i)]);

  // replace links to decks with proper hrefs
  html = html.replace(/(\S*?)deck\.md\b/g, (match, basename) => {
    return `<a href="${basename}deck.html">${basename}deck.html</a>`;
  });

  // add to DOM element
  msg.innerHTML = html;

  // links should open in new tabs
  const anchors = msg.querySelectorAll("a");
  anchors.forEach((anchor) => {
    anchor.target = "_blank";
  });

  // now run MathJax
  if (window.MathJax && window.MathJax.typesetPromise) {
    await MathJax.typesetPromise([msg]);
  }
}

async function send() {
  // user input from prompt element
  const userInput = promptEl.value.trim();
  if (!userInput) return;
  let input = userInput;

  // insert current slide deck and slide into the input
  if (Reveal) {
    const url = location.pathname;
    const filename = url.split("\\").pop().split("/").pop().split(".")[0];
    const deck = filename.replace("deck.html", "deck.md");
    const slide = Reveal.getCurrentSlide();
    const h1 = slide.querySelector("h1");
    if (deck && h1) {
      const title =
        h1.childElementCount > 1 ? h1.lastElementChild.innerText : h1.innerText;
      input = [
        {
          role: "developer",
          content: `The user is watching slide deck "${deck}". The current slide has the title "${title}"`,
        },
        {
          role: "user",
          content: userInput,
        },
      ];
    }
  }

  // adjust button states
  sendBtn.disabled = true;
  stopBtn.disabled = false;
  abortController = new AbortController();

  // add user question to chat and clear prompt
  newMessage("user").add(userInput);
  promptEl.value = "";

  // add bot message to chat; content will be filled below
  const botMsg = newMessage("bot");

  try {
    const response = await fetch(server.trim() + "/chatty", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        prompt: { id: prompt },
        input: input,
        previous_response_id: previous_response_id,
        stream: true,
      }),
      signal: abortController.signal,
    });

    if (!response.ok || !response.body) {
      const txt = await response.text().catch(() => String(response.status));
      botMsg.innerText = "[Error] " + txt;
      return;
    }

    previous_response_id = response.id;

    const reader = response.body.getReader();
    const decoder = new TextDecoder();
    let buffer = ""; // buffer gets decoded stream
    let mdText = ""; // accumulate response text

    while (true) {
      const { value, done } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });
      const parts = buffer.split("\n\n");
      buffer = parts.pop() || "";

      for (const part of parts) {
        const line = part.split("\n").find((l) => l.startsWith("data:"));
        if (!line) continue;
        const data = line.slice(5).trim();
        if (data === "[DONE]") continue;

        try {
          const evt = JSON.parse(data);

          if (
            evt.type === "response.output_text.delta" &&
            typeof evt.delta === "string"
          ) {
            mdText += evt.delta;
            await botMsg.add(mdText);
            chatEl.scrollTop = chatEl.scrollHeight;
          }

          // remember response ID
          previous_response_id = evt.response.id;
        } catch {}
      }
    }

    // console.log("DEBUG", mdText);
  } catch (err) {
    if (err.name !== "AbortError") {
      botMsg.innerText = "[Error] " + err.message;
    }
  } finally {
    sendBtn.disabled = false;
    stopBtn.disabled = true;
    abortController = null;
    promptEl.focus();
  }
}

function closeDialog() {
  const popover = dialog.closest("[popover]");
  const details = dialog.closest("details");

  // deck mode
  if (popover) {
    popover.hidePopover();
  }

  // index mode
  else if (details) {
    details.open = false;
    details.firstElementChild.focus();
  }
}
