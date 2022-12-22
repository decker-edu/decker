/**
 * this module allows to set light mode and dark mode
 * through either system-wide preferences or by
 * setting them explicitly through JS
 *
 * @author Mario Botsch
 */

// returns "light", "dark", or "system"
export function getPreference() {
  const storage = localStorage.getItem("color-mode");
  return storage ? storage : "system";
}

// Given "light", "dark", or "system" as argument,
// the preference is stored in localStorage.
// Afterwards, updateHTML is called, which sets the class
// "light" or "dark" on the <html> element.
export function setPreference(mode) {
  if (mode === "dark") {
    localStorage.setItem("color-mode", "dark");
  } else if (mode === "light") {
    localStorage.setItem("color-mode", "light");
  } else if (mode === "system") {
    localStorage.removeItem("color-mode");
  }
  updateHTML();
}

// Determine current color preference and then set
// either "light" or "dark" as class on <html> element.
function updateHTML() {
  const system =
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  const storage = localStorage.getItem("color-mode");
  const mode = storage ? storage : system;
  if (mode === "dark") {
    document.documentElement.classList.add("dark");
    document.documentElement.classList.remove("light");
  } else {
    document.documentElement.classList.add("light");
    document.documentElement.classList.remove("dark");
  }
}

// Set up color scheme handling.
export function initialize() {
  if (window.matchMedia) {
    const query = window.matchMedia("(prefers-color-scheme: dark)");
    query.addEventListener("change", updateHTML);
  }
  updateHTML();
}

// finally call initialize
initialize();
