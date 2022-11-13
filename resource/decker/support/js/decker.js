export default initializeDecker;

function initializeDecker(metaUrl) {
  // Provides the Decker meta data and some functions globally. Eventually, this
  // module should be imported by every script that needs access. For now, this is a
  // global variable.

  // We need to fetch this asynchronously because code following this function needs
  // access to the global Decker variable.
  let req = new XMLHttpRequest();
  req.open("GET", metaUrl, false /* fetch synchronously */);
  req.send();
  let meta = JSON.parse(req.responseText);

  window.Decker = {
    // The Decker meta data as passed in from the template.
    meta: meta,

    // Find the document anchor object from within an executable Javascript code
    // block module.
    anchor: (meta) => {
      let anchor = document.getElementById(new URL(meta.url).hash.slice(1));
      if (!anchor) console.log("Cannot find anchor for Javascript execution.");
      return anchor;
    },

    // Do something for the Electron app.
    isElectron: () => {
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
    },

    // List of predicates that all must return true for a requested reload to
    // actually be performed.
    // Adds a reload inhibitor.
    addReloadInhibitor: (predicate) => {
      reloadInhibitors.push(predicate);
    },

    // Removes a reload inhibitor.
    removeReloadInhibitor: (predicate) => {
      reloadInhibitors.splice(
        reloadInhibitors.find((p) => p === predicate),
        1
      );
    },
    tripleClick: (callback) => {
      let pushCount = 0;
      let lastPush = null;

      return () => {
        let now = Date.now();
        if (lastPush && now - lastPush < 500) {
          pushCount++;
        } else {
          pushCount = 1;
        }
        lastPush = now;

        if (pushCount == 3) {
          pushCount = 0;
          callback();
        }
      };
    },
  };

  // Finally, opens a web socket connection and listens to reload requests from the server.
  // If all of the registered inhibitors return true, the reload is performed.
  let reloadInhibitors = [];
  window.addEventListener("load", () => {
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
  });
}
