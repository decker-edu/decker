/**
 * @author Erich Schubert
 */
const Plugin = {
  id: "thebe",
  init: (deck) => {
    if (!Decker.meta["thebe"]) return; // not configured
    return new Promise(function (resolve) {
      if (document.querySelectorAll(Decker.meta.thebe["selector"] || '[data-executable]').length > 0 && window.thebe === undefined) {
        console.log("Loading Thebe.", document.querySelectorAll('[data-executable]').length, "cells.");
        // Thebe-lite needs to be loaded before core, if enabled
        if (Decker.meta.thebe["useJupyterLite"]) {
          // Jupyterlite configuration for thebe-lite
          console.log("Loading thebe-lite");
          let conf = document.createElement("script");
          conf.setAttribute("type", "application/json");
          conf.setAttribute("id", "jupyter-config-data");
          conf.text=JSON.stringify(Decker.meta["jupyter"]);
          document.body.append(conf);
          // Load thebe-lite
          let script = document.createElement("script");
          script.setAttribute("type", "text/javascript");
          script.setAttribute("src", Decker.meta.supportPath + "/vendor/thebe/thebe-lite.min.js");
          document.body.append(script);
        }
        // Load Thebe
        let script = document.createElement("script");
        script.setAttribute("type", "text/javascript");
        script.setAttribute("src", Decker.meta.supportPath + "/vendor/thebe/index.js");
        document.body.append(script);
        // Thebe CSS
        let style = document.createElement("link");
        style.setAttribute("rel", "stylesheet");
        style.setAttribute("href", Decker.meta.supportPath + "/vendor/thebe/thebe.css");
        document.body.append(style);
        // Workaround for CodeMirror 5 scaling issues:
        let scaleCodeMirror = function(cm) {
          let scale = deck.getScale();
          cm.CodeMirror.display.sizer.style.transform="scale("+(1./scale)+")";
          cm.CodeMirror.display.sizer.style.transformOrigin="0 0 0";
          cm.CodeMirror.display.sizer.style.fontSize=(100*scale)+"%";
          let minh = parseFloat(cm.CodeMirror.display.sizer.style.minHeight);
          if (minh > 0) {
            cm.CodeMirror.display.sizer.style.minHeight=""; //
            cm.CodeMirror.display.sizer.style.height=Math.ceil(minh / scale)+"px"; // not when NaN
          }
          cm.CodeMirror.display.heightForcer.style.height="0"; // Disable, breaks short fields
          cm.CodeMirror.display.heightForcer.style.top="0"; // Disable, breaks short fields
        };
        // Bootstrap thebe on finished loading
        script.addEventListener('load', () => {
          if (document.querySelectorAll('div.thebe-cell').length > 0) return; // Already started
          thebe.bootstrap(Decker.meta["thebe"]);
          // Add custom buttons and status widget
          // Add a status indicator to each input box
          document.querySelectorAll('.thebe-controls').forEach((e) => {
            if (Decker.meta["thebe"]["requestKernel"]===false) {
              let start_button = document.createElement("button");
              start_button.classList.add('thebe-button', 'thebe-start-button')
              start_button.innerText = "start";
              start_button.setAttribute("title", "Start the kernel.");
              start_button.onclick = async() => {
                if (!window.thebe.session) {
                  await window.thebe.server.ready;
                  const session = await window.thebe.server.startNewSession(window.thebe.notebook.rendermime);
                  if (session != null) {
                    window.thebe.notebook.attachSession(session);
                    window.thebe.session = session;
                  }
                }
              };
              e.prepend(start_button);
            }
            let kernel_status = document.createElement("div");
            kernel_status.setAttribute("class", "thebe-status");
            e.append(kernel_status);
          });
          // Status indicator
          thebe.on("status", function(evt, data) {
            // To show/hide buttons that are not applicable
            document.querySelectorAll('.thebe-controls').forEach((e) => {
              e.classList.forEach((c) => { if (c.startsWith("session-")) { e.classList.remove(c) }} );
              e.classList.add("session-"+data.status);
            });
            // Update status indicator
            document.querySelectorAll('.thebe-status').forEach((e) => {
              e.innerText = data["status"];
              if (data["status"] == "server-ready") e.innerText = "no session started";
            });
            if (data["status"] == "attached") {
              document.querySelectorAll('.thebe-start-button').forEach((e) => { e.style.display = "none"; });
            }
            // Apply CodeMirror scaling hack
            if (data.status == "launching") {
              // In case a cell gets the DOM recreated:
              document.querySelectorAll('.CodeMirror').forEach((x)=>{
                scaleCodeMirror(x);
                x.CodeMirror.on("update", (event) => scaleCodeMirror(x));
              });
            }
          });
          // Reveal resize events
          deck.on('resize', (event) => document.querySelectorAll('.CodeMirror').forEach(scaleCodeMirror));
        });
      } else {
        console.log("Not enabling thebe: no data-executable fragments found");
      }
      resolve();
    });
  },
};

export default Plugin;

