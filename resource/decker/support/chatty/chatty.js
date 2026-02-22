export default setup;
import "./marked.min.js";

// config
let server;
let prompt;

// access to Reveal and slide with annottions
let Reveal;
let currentAnnotationSlide;

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
  greetingDeck: `Ich bin **Prof. Bot**, dein KI-basierter Tutor. Du kannst mir Fragen zu den Vorlesungsinhalten stellen.<br>
    Ich weiß, auf welcher Folie du gerade bist, so dass du mich zur aktuellen Folie befragen kannst. Wenn die aktuelle Folie extra Whiteboard-Seiten mit Annotationen enthält, kannst du mich auch zu diesen fragen.<br>
    **Aber Vorsicht: Meine Antworten können auch falsch sein.**`
};
const englishLocalization = {
  send: "Send",
  stop: "Stop",
  question: "Enter question…",
  greeting:
    "I'm **Prof. Bot**, your AI-based tutor. You can ask questions related to the course material. *But be aware that my answers might be wrong.*",
  greetingDeck: `I'm **Prof. Bot**, your AI-based tutor. You can ask questions related to the course material.<br>
    I know which slide your are on, so you can ask me about the current slide. If it contains additional whiteboard pages with annotations, you can also ask me about these.<br>
    **But be aware that my answers might be wrong.**`
};
const lang = Decker.meta.lang || navigator.language;
const l10n = lang === "de" ? germanLocalization : englishLocalization;

const useFirst = Decker?.meta?.chatty
  ? Decker.meta.chatty["use-first-annotation-page"]
  : false;

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
  newMessage("bot").add(
    Reveal
      ? window.Decker?.meta?.chatty?.greetingDeck ||
          window.Decker?.meta?.chatty?.greeting ||
          l10n.greetingDeck
      : window.Decker?.meta?.chatty?.greeting || l10n.greeting
  );
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
    headerIds: false
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
  // user input from prompt element (and optional slide info)
  const userInput = promptEl.value.trim();
  if (!userInput) return;
  const input = Reveal
    ? await combineUserInputAndSlideInfo(userInput)
    : userInput;

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
        stream: true
      }),
      signal: abortController.signal
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

async function combineUserInputAndSlideInfo(userInput) {
  const url = location.pathname;
  const fragment = location.hash;
  const filename = url.split("\\").pop().split("/").pop().split(".")[0];
  const deck = filename.replace("deck.html", "deck.md");
  const slide = Reveal.getCurrentSlide();
  const h1 = slide.querySelector("h1");

  if (!deck || !h1 || !fragment) return userInput;

  // we wil construct an array of inputs
  let input = [];

  // add deck and slide
  const title =
    h1.childElementCount > 1 ? h1.lastElementChild.innerText : h1.innerText;
  input.push({
    role: "user",
    content:
      `I am watching slide deck "${deck}". The current slide has the title "${title}"` +
      (fragment ? ` and fragment identifier ${fragment}.` : ".")
  });

  // do we have whiteboard annotations?
  let slideHasAnnotations = false;
  const annot = slide.querySelector("svg.whiteboard");
  if (annot) {
    // get page and whiteboard dimensions
    const pageHeight = parseInt(Reveal.getConfig().height);
    const annotHeight = annot.clientHeight;
    const numAnnotPages =
      Math.ceil(annotHeight / pageHeight) - (useFirst ? 0 : 1);

    // does this slide have extra whiteboard pages?
    if (numAnnotPages > 0) {
      slideHasAnnotations = true;
      // did we not send annotations already?
      if (slide != currentAnnotationSlide) {
        // remember slide, so we don't send slide annotations again
        currentAnnotationSlide = slide;

        // construct annotation info per page (I think this is not needed)
        // let content = [
        //   {
        //     type: "input_text",
        //     text:
        //       `In addition to the content from ${deck} the current slide also contains ${numAnnotPages} ` +
        //       (numAnnotPages > 1 ? "pages " : "page ") +
        //       "of hand-written annotations or drawings that are provided in the following image.",
        //   },
        // ];
        // const annotWidth = annot.clientWidth;
        // for (let top = pageHeight; top < annotHeight; top += pageHeight) {
        //   const bbox = { x: 0, y: top, width: annotWidth, height: pageHeight };
        //   const png = await svgToPng(annot, bbox);
        //   content.push({
        //     type: "input_image",
        //     image_url: png,
        //   });
        // }

        // Render as one big image (alternative to the code above).
        // Number of tokens is computed as follows (with integer division):
        //   (width+32-1)/32 * (height+32-1)/32
        // Since we render at half resolution, a whiteboard page at 1280x720
        // corresponds to 20*12=240 tokens. Images get scaled down if above 1536 tokens,
        // which would be 6 whiteboard pages.
        const png = await svgToPng(annot);
        let content = [
          {
            type: "input_text",
            text: `In addition to the content from ${deck} the current slide also contains additional hand-written annotations or drawings that are provided in the following image.`
          },
          { type: "input_image", image_url: png }
        ];

        // add annot content to input array
        input.push({
          role: "user",
          content: content
        });
      }
    }
  }

  if (!slideHasAnnotations) {
    input.push({
      role: "user",
      content: "The current slide does not contain hand-written annotations."
    });
  }

  // add user input
  input.push({
    role: "user",
    content: userInput
  });

  // return input array
  return input;
}

// render SVG annotations to PNG
async function svgToPng(svgElement, bbox) {
  return new Promise((resolve, reject) => {
    try {
      // get page dimensions, render at half resolution
      const pageWidth = parseInt(Reveal.getConfig().width);
      const pageHeight = parseInt(Reveal.getConfig().height);
      let width = pageWidth / 2;
      let height = pageHeight / 2;

      // if no bounding box was specified, render the whole SVG as one large page,
      // but subtract the first page (as it is no extra whiteboard page).
      // then also adjust width/height of PNG.
      if (!bbox) {
        const pageSkip = useFirst ? 0 : pageHeight;
        bbox = {
          x: 0,
          y: pageSkip,
          width: svgElement.clientWidth,
          height: svgElement.clientHeight - pageSkip
        };
        width = bbox.width / 2;
        height = bbox.height / 2;
      }

      // clone SVG, since we have to make some changes
      const svg = svgElement.cloneNode(true);

      // inject viewbox
      svg.setAttribute(
        "viewBox",
        `${bbox.x} ${bbox.y} ${bbox.width} ${bbox.height}`
      );

      // inject style, since CSS not known within SVG
      svg.querySelectorAll("path").forEach((e) => {
        e.style.stroke = "black";
        e.style.fill = "none";
      });

      // svg to data url
      const svgString = new XMLSerializer().serializeToString(svg);
      const blob = new Blob([svgString], {
        type: "image/svg+xml;charset=utf-8"
      });
      const url = URL.createObjectURL(blob);

      // Load svg into image element
      const img = new Image();
      img.onload = () => {
        const canvas = document.createElement("canvas");
        canvas.width = width;
        canvas.height = height;
        const ctx = canvas.getContext("2d");
        ctx.imageSmoothingEnabled = true;
        ctx.imageSmoothingQuality = "high";
        ctx.fillStyle = "white";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.drawImage(img, 0, 0, canvas.width, canvas.height);

        // get image as base64-encoded PNG
        const png = canvas.toDataURL("image/png");
        resolve(png);
      };

      img.onerror = (err) => {
        URL.revokeObjectURL(url);
        reject(err);
      };

      img.src = url;
    } catch (err) {
      reject(err);
    }
  });
}
