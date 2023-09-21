/* Flying focus taken from https://github.com/NV/flying-focus (MIT license) 
and adapted to meet the needs of Reveal.js (tak slide zoom into account).
Adaptations inspired by https://github.com/Q42/floating-focus-a11y (MIT license).
*/

let flyingFocus = null;
let target = null;
let keyDownTime = 0;

export function handleKeyboard(event) {
  const code = event.code;
  // Show animation only upon Tab or Arrow keys press.
  if (
    code === "Tab" ||
    code === "ArrowLeft" ||
    code === "ArrayRight" ||
    code === "ArrowDown" ||
    code === "ArrowUp" ||
    code === "Enter"
  ) {
    keyDownTime = Date.now();
  }
}

export function showFlyingFocus(event) {
  // if focus was changed, but not due to keyboard navigation: hide it.
  if (!isJustPressed()) {
    hideFlyingFocus();
    return;
  }

  // get focus target
  if (event.target.id === "flying-focus") {
    return;
  }
  target = event.target;

  // set new position of flying focus
  Object.assign(flyingFocus.style, rectOf(target));

  // adjust style of flying focus to target element
  Object.assign(flyingFocus.style, borderOf(target));

  // show flying focus
  flyingFocus.classList.add("flying-focus_visible");
}

export function hideFlyingFocus(event) {
  if (!event) return; // happens when navigating from navigation bar into slide
  if (flyingFocus) flyingFocus.classList.remove("flying-focus_visible");
}

// adjust position/size of flying focus
function positionFlyingFocus() {
  if (!flyingFocus || !target) return;
  requestAnimationFrame(() => {
    Object.assign(flyingFocus.style, rectOf(target));
  });
}

function isJustPressed() {
  return Date.now() - keyDownTime < 42;
}

// return bounding rectangle of given element
function rectOf(elem) {
  if (!elem.getBoundingClientRect) return;
  const rect = elem.getBoundingClientRect();

  const clientLeft =
    document.documentElement.clientLeft || document.body.clientLeft;
  const clientTop =
    document.documentElement.clientTop || document.body.clientTop;
  const scrollLeft = window.scrollX || window.pageXOffset;
  const scrollTop = window.scrollY || window.pageYOffset;

  let zoom = 1;
  for (let e = elem; e; e = e.parentElement) {
    zoom *= e.style.zoom || 1;
  }

  const top = zoom * rect.top + scrollTop - clientTop;
  const left = zoom * rect.left + scrollLeft - clientLeft;
  const width = zoom * rect.width;
  const height = zoom * rect.height;

  return {
    left: `${left - 1}px`,
    top: `${top - 1}px`,
    width: `${width + 1}px`,
    height: `${height + 1}px`,
  };
}

// return border style of given element
function borderOf(elem) {
  const style = window.getComputedStyle(elem);
  return {
    borderBottomLeftRadius: style.borderBottomLeftRadius,
    borderBottomRightRadius: style.borderBottomRightRadius,
    borderTopLeftRadius: style.borderTopLeftRadius,
    borderTopRightRadius: style.borderTopRightRadius,
    boxSizing: style.boxSizing,
  };
}

// setup event listeners
export function setupFlyingFocus() {
  // use uniq element name to decrease the chances of a conflict with website styles
  flyingFocus = document.createElement("flying-focus");
  flyingFocus.id = "flying-focus";
  document.body.appendChild(flyingFocus);

  window.addEventListener("keydown", handleKeyboard, false);
  document.addEventListener("focus", showFlyingFocus, true);
  document.addEventListener("blur", hideFlyingFocus, true);
  document.addEventListener("focusout", hideFlyingFocus, true);
  document.addEventListener("scroll", positionFlyingFocus, true);
  window.addEventListener("resize", positionFlyingFocus, true);
}

export default setupFlyingFocus;
