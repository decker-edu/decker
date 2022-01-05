"use strict";

(function () {
  let flyingFocus = null;
  let target = null;
  let keyDownTime = 0;

  document.addEventListener(
    "keydown",
    (event) => {
      let code = event.code;
      // Show animation only upon Tab or Arrow keys press.
      if (
        code === "Tab" ||
        code === "ArrowLeft" ||
        code === "ArrayRight" ||
        code === "ArrowDown" ||
        code === "ArrowUp"
      ) {
        keyDownTime = Date.now();
      }
    },
    false
  );

  document.addEventListener(
    "focus",
    (event) => {
      // only do flying focus on keyboard navigation
      if (!isJustPressed()) {
        return;
      }

      // get focus target
      if (event.target.id === "flying-focus") {
        return;
      }
      target = event.target;

      // create flying focus on first call
      if (!flyingFocus) {
        flyingFocus = document.createElement("flying-focus"); // use uniq element name to decrease the chances of a conflict with website styles
        flyingFocus.id = "flying-focus";
        document.body.appendChild(flyingFocus);
      }

      // set new position of flying focus
      Object.assign(flyingFocus.style, rectOf(target));

      // adjust style of flying focus to target element
      Object.assign(flyingFocus.style, borderOf(target));

      // show flying focus
      flyingFocus.classList.add("flying-focus_visible");
    },
    true
  );

  document.addEventListener(
    "blur",
    (event) => {
      if (flyingFocus) flyingFocus.classList.remove("flying-focus_visible");
    },
    true
  );

  document.addEventListener("scroll", handleScrollResize, true);
  window.addEventListener("resize", handleScrollResize, true);

  function handleScrollResize() {
    if (!flyingFocus || !target) return;

    requestAnimationFrame(() => {
      Object.assign(flyingFocus.style, rectOf(target));
    });
  }

  function isJustPressed() {
    return Date.now() - keyDownTime < 42;
  }

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
      left: `${left}px`,
      top: `${top}px`,
      width: `${width}px`,
      height: `${height}px`,
    };
  }

  function borderOf(elem) {
    const style = window.getComputedStyle(elem);
    return {
      padding: style.padding,
      borderBottomLeftRadius: style.borderBottomLeftRadius,
      borderBottomRightRadius: style.borderBottomRightRadius,
      borderTopLeftRadius: style.borderTopLeftRadius,
      borderTopRightRadius: style.borderTopRightRadius,
    };
  }
})();
