import { jsx, htmlToElement } from "./utils.mjs";

let blockManip = false;
let slides = document.querySelector("div.reveal div.slides");
let allManipulatedBlocks = {};

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

function dec2hex(dec) {
  return dec.toString(16).padStart(2, "0");
}

function generateId(len) {
  var arr = new Uint8Array((len || 6) / 2);
  window.crypto.getRandomValues(arr);
  return Array.from(arr, dec2hex).join("");
}

function startDragging(block, type, event) {
  let state = { block, type };

  console.log(event.target, state.type);

  state.startX = event.screenX;
  state.startY = event.screenY;
  state.zIndex = state.block.style.zIndex;

  let slideTransform = slides.computedStyleMap().get("transform");
  state.slideScale = slideTransform[1].x.value;

  state.block.style.zIndex = 1000;

  let transform = state.block.computedStyleMap().get("transform");
  state.matrix = new CSSMatrixComponent(
    transform instanceof CSSTransformValue
      ? transform.toMatrix()
      : new DOMMatrix(),
  );

  let { width, height } = state.block.getBoundingClientRect();
  state.width = new CSSUnitValue(width / state.slideScale, "px");

  document.dragState = state;
}

function drag(event) {
  let state = document.dragState;
  if (!state) return;

  let dx = (event.screenX - state.startX) / state.slideScale;
  let dy = (event.screenY - state.startY) / state.slideScale;

  if (state.type === "move") {
    let translate = new CSSTranslate(
      new CSSUnitValue(dx, "px"),
      new CSSUnitValue(dy, "px"),
    );

    state.block.style.transform = new CSSTransformValue([
      translate,
      state.matrix,
    ]);
  } else if (state.type === "resize") {
    console.log(state.width, dx);
    state.block.style.width = state.width.add(new CSSUnitValue(dx, "px"));
  }
}

function stopDragging(event) {
  let state = document.dragState;
  if (!state) return;

  if (state.type === "move") {
    let m = state.block.computedStyleMap().get("transform").toMatrix();
    state.block.setAttribute(
      "data-transform",
      `matrix(${m.a},${m.b},${m.c},${m.d},${m.e},${m.f})`,
    );
  } else if (state.type === "resize") {
  }

  state.block.style.zIndex = state.zIndex;
  document.dragState = null;
}

function enableBlockManip() {
  slides.manipulateSlide = Reveal.getCurrentSlide();

  Reveal.on("slidechanged", disableBlockManip);
  // Reveal.configure({ hideInactiveCursor: false });

  let blocks = Array.from(slides.manipulateSlide.querySelectorAll("div.block"));
  let title = Array.from(slides.manipulateSlide.querySelectorAll("h1"));
  slides.manipulateBlocks = blocks.concat(title);

  for (let block of slides.manipulateBlocks) {
    if (!block.id) continue;

    block.classList.add("manipulatable");

    let { width, height } = block.getBoundingClientRect();
    let overlay = htmlToElement(jsx`
      <div class="block-overlay" style="width: ${width}; height: ${height};">
        <div class="width-handle"></div>
      </div>
      `);
    let widthHandle = overlay.querySelector(".width-handle");

    overlay.addEventListener("mousedown", (event) => {
      startDragging(block, "move", event);
      event.stopPropagation();
    });

    widthHandle.addEventListener("mousedown", (event) => {
      startDragging(block, "resize", event);
      event.stopPropagation();
    });

    document.addEventListener(
      "mousemove",
      (event) => {
        drag(event);
        event.stopPropagation();
      },
      { capture: true },
    );

    document.addEventListener(
      "mouseup",
      (event) => {
        stopDragging(event);
        event.stopPropagation();
      },
      { capture: true },
    );

    block.append(overlay);
  }

  blockManip = true;
}

function disableBlockManip() {
  Reveal.off("slidechanged", disableBlockManip);
  // Reveal.configure({ hideInactiveCursor: true });

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
    .catch((_) => {
      // console.warn("[] cannot download manipulation data from: ", url);
      // return;
    });

  if (!manips) return;
   
  let slideTransform = slides.computedStyleMap().get("transform");
  let slideScale = slideTransform[1].x.value;

  for (let [id, values] of Object.entries(manips)) {
    let element = document.getElementById(id);
    if (element) {
      element.style.width = new CSSUnitValue(values.width / slideScale, "px");
      element.style.transform = values.transform;
    }
  }

  allManipulatedBlocks = manips;
}
