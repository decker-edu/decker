"use strict";

if (Decker) {
  Decker.anchor = (meta) => {
    let anchor = document.getElementById(
      new URL(meta.url).hash.slice(1)
    );
    if (!anchor) console.log("Cannot find anchor for Javascript execution.");
    return anchor;
  };
} else {
  console.log("window.Decker needs to be already defined.");
}

function isElectron() {
  // Renderer process
  if (
    typeof window !== "undefined" &&
    typeof window.process === "object" &&
    window.process.type === "renderer"
  ) {
    return true;
  }

  // Main process
  if (
    typeof process !== "undefined" &&
    typeof process.versions === "object" &&
    !!process.versions.electron
  ) {
    return true;
  }

  // Detect the user agent when the `nodeIntegration` option is set to true
  if (
    typeof navigator === "object" &&
    typeof navigator.userAgent === "string" &&
    navigator.userAgent.indexOf("Electron") >= 0
  ) {
    return true;
  }

  return false;
}

// List of predicates that all must return true for a requested reload to
// actually be performed.
let reloadInhibitors = [];

// Adds a reload inhibitor.
function addReloadInhibitor(predicate) {
  reloadInhibitors.push(predicate);
}

// Removes a reload inhibitor.
function removeReloadInhibitor(predicate) {
  reloadInhibitors.splice(
    reloadInhibitors.find((p) => p === predicate),
    1
  );
}

// Opens a web socket connection and listens to reload requests from the server.
// If all of the registered inhibitors return true, the reload is performed.
function openReloadSocket() {
  if (location.hostname == "localhost" || location.hostname == "0.0.0.0") {
    var socket = new WebSocket("ws://" + location.host + "/reload");
    socket.onmessage = function (event) {
      if (event.data.startsWith("reload!")) {
        console.log("Reload requested.");
        let reload = reloadInhibitors.reduce((a, p) => a && p(), true);
        if (reload) {
          console.log("Reload authorized.");
          window.location.reload();
        } else {
          console.log("Reload inhibited.");
        }
      }
    };
  }
}

window.addEventListener("load", openReloadSocket);
