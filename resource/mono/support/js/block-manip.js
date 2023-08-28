import { jsx, htmlToElement } from "./utils.mjs";

let blockManip = false;

export default function initializeBlockManipulation() {
  downloadManipulations();
  Reveal.addKeyBinding(
    { keyCode: 77, key: "M", description: "Toggle block manipulation" },
    () => {
      if (!blockManip) enableBlockManip();
      else disableBlockManip();
    },
  );
}

let slides = document.querySelector("div.reveal div.slides");

function dec2hex(dec) {
  return dec.toString(16).padStart(2, "0");
}

function generateId(len) {
  var arr = new Uint8Array((len || 6) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, dec2hex).join("");
}

function startDragging(event) {
  let state = event.target.state;

  let slideTransform = slides.computedStyleMap().get("transform");
  state.slideScale = slideTransform[1].x.value;

  state.startX = event.clientX;
  state.startY = event.clientY;

  state.zIndex = state.block.style.zIndex;
  state.block.style.zIndex = 1000;

  let transform = state.block.computedStyleMap().get("transform");
  state.matrix = new CSSMatrixComponent(
    transform instanceof CSSTransformValue
      ? transform.toMatrix()
      : new DOMMatrix(),
  );

  state.dragging = true;
}

function drag(event) {
  let state = event.target.state;
  if (state.dragging) {
    let dx = (event.clientX - state.startX) / state.slideScale;
    let dy = (event.clientY - state.startY) / state.slideScale;

    let translate = new CSSTranslate(
      new CSSUnitValue(dx, "px"),
      new CSSUnitValue(dy, "px"),
    );

    state.block.style.transform = new CSSTransformValue([
      translate,
      state.matrix,
    ]);
  }
}

function stopDragging(event) {
  let state = event.target.state;
  let m = state.block.computedStyleMap().get("transform").toMatrix();
  state.block.setAttribute(
    "data-transform",
    `matrix(${m.a},${m.b},${m.c},${m.d},${m.e},${m.f})`,
  );
  state.block.style.zIndex = state.zIndex;
  state.dragging = false;
}

function enableBlockManip() {
  slides.manipulateSlide = Reveal.getCurrentSlide();

  Reveal.on("slidechanged", disableBlockManip);
  Reveal.configure({ hideInactiveCursor: false });

  let blocks = Array.from(slides.manipulateSlide.querySelectorAll("div.block"));
  let title = Array.from(slides.manipulateSlide.querySelectorAll("h1"));
  slides.manipulateBlocks = blocks.concat(title);

  for (let block of slides.manipulateBlocks) {
    if (!block.id) continue;

    block.classList.add("manipulatable");

    let { width, height } = block.getBoundingClientRect();
    let overlay = htmlToElement(jsx`
      <div class="block-overlay" style="width: ${width}; height: ${height};"></div>
      `);

    overlay.state = { block, overlay };

    overlay.addEventListener("mousedown", (event) => {
      startDragging(event);
      event.stopPropagation();
    });

    overlay.addEventListener("mousemove", (event) => {
      drag(event);
      event.stopPropagation();
    });

    overlay.addEventListener("mouseup", (event) => {
      stopDragging(event);
      event.stopPropagation();
    });

    overlay.addEventListener("mouseout", (event) => {
      stopDragging(event);
      event.stopPropagation();
    });

    block.append(overlay);
  }

  blockManip = true;
  Decker.flash.message("Block manipulation enabled ...");
}

let allManipulatedBlocks = {};

function disableBlockManip() {
  Reveal.off("slidechanged", disableBlockManip);
  Reveal.configure({ hideInactiveCursor: true });

  let overlays = slides.manipulateSlide.querySelectorAll("div.block-overlay");
  for (const o of overlays) o.remove();

  for (let element of slides.manipulateBlocks) {
    element.classList.remove("manipulatable");
    allManipulatedBlocks[element.id] = {
      transform: element.getAttribute("data-transform"),
      width: element.getBoundingClientRect().width,
      height: element.getBoundingClientRect().height,
    };
  }

  blockManip = false;
  slides.manipulateBlocks = undefined;
  slides.manipulateSlide = undefined;
  Decker.flash.message("Block manipulation disabled.");

  uploadManipulations();
}

function uploadManipulations() {
  let path = location.pathname;
  let base = path.substring(0, path.lastIndexOf("-"));
  let url = base + `-manip.json`;
  fetch(url, { method: "PUT", body: JSON.stringify(allManipulatedBlocks) })
    .then((r) => console.log("[] manipulation data uploaded to:", url))
    .catch((e) => {
      console.error(
        "[] cannot upload manipulation data to:",
        url,
        "reason:",
        e,
      );
    });
}

async function downloadManipulations() {
  let path = location.pathname;
  let base = path.substring(0, path.lastIndexOf("-"));
  let url = base + `-manip.json`;
  let manips = await fetch(url)
    .then((r) => {
      if (r.ok) return r.json();
      else throw new Error("[] cannot fetch: ", r.statusText);
    })
    .catch((e) => {
      console.error(
        "[] cannot download manipulation data from: ",
        url,
        "reason:",
        e,
      );
    });

  console.log(manips);

  for (let [id, values] of Object.entries(manips)) {
    let element = document.getElementById(id);
    if (element) {
      element.style.width = values.width;
      element.style.height = values.height;
      element.style.transform = values.transform;
    }
  }
}
