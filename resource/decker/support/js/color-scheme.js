/**
 * this module allows to set light mode and dark mode
 * through either system-wide preferences or by
 * setting them explicitly through JS
 *
 * @author Mario Botsch
 */

// Determine current color preference and then set
// either "light" or "dark" as class on <html> element.
function updateHTML() {
  const system =
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  const storage = sessionStorage.getItem("color-mode");
  const mode = storage ? storage : system;
  if (mode === "dark") {
    document.documentElement.classList.add("dark");
    document.documentElement.classList.remove("light");
  } else {
    document.documentElement.classList.add("light");
    document.documentElement.classList.remove("dark");
  }
}

export function toggleColor() {
  if (document.documentElement.classList.contains("dark")) {
    document.documentElement.classList.remove("dark");
    document.documentElement.classList.add("light");
    sessionStorage.setItem("color-mode", "light");
  } else {
    document.documentElement.classList.remove("light");
    document.documentElement.classList.add("dark");
    sessionStorage.setItem("color-mode", "dark");
  }
}

// Set up color scheme handling.
export function initialize() {
  // get setting (light, dark, auto)
  const setting = window.Decker?.meta?.colorscheme || "auto";
  switch (setting) {
    case "light":
    case "dark":
      document.documentElement.classList.add(setting);
      break;

    case "auto":
    default:
      if (window.matchMedia) {
        const query = window.matchMedia("(prefers-color-scheme: dark)");
        query.addEventListener("change", updateHTML);
      }
      updateHTML();
      break;
  }
}

// finally call initialize
initialize();
