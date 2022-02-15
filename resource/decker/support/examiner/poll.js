export { pollSession };

import bwipjs from "https://cdnjs.cloudflare.com/ajax/libs/bwip-js/3.0.4/bwip-js.mjs";

var session = null;

function pollSession(url, onready = null, onclose = null) {
  session = {
    id: null,
    clientUrl: null,
    socket: null,
    ui: null,
    onReady: onready,
    onClose: onclose,
  };

  return new Promise((resolve, error) => {
    session.socket = new WebSocket(Decker.meta["poll-server"]);

    session.socket.addEventListener("open", (e) => {
      console.log("Poll:", "server connected.");
    });

    session.socket.addEventListener("error", (e) => {
      error("Poll:", "ERROR: ", "Cannot connect to:", url);
    });

    session.socket.addEventListener("close", (e) => {
      console.log("Poll:", "ERROR: ", "Server went away.");
      if (session.onClose) session.onClose();
    });

    session.socket.addEventListener("message", (e) => {
      let message = JSON.parse(e.data);
      console.log("Poll:", "Received message:", message);
      if (message.error) {
        console.log("Poll:", "Server error:", message.error);
      } else if (message.key != null) {
        let clientUrl = new URL(Decker.meta.supportPath + "/poll.html", location.href);
        session.id = message.key;
        session.clientUrl = `${clientUrl}#${session.id}`;

        resolve({
          sessionId: () => {
            return { id: session.id, url: session.clientUrl };
          },

          fillQRCode: (canvas) => {
            bwipjs.toCanvas(canvas, {
              bcid: "qrcode", // Barcode type
              text: url, // Text to encode
              scale: 20, // Somehow gives 1000^2 pixels
              includetext: false, // Show human-readable text
              textxalign: "center", // Always good to set this
              eclevel: "L",
            });
          },

          poll: (choices, votes, callbacks) => {
            session.ui = callbacks;
            session.socket.send(
              JSON.stringify({ tag: "Start", choices: choices, votes: votes })
            );
          },

          stop: () => {
            session.socket.send(
              JSON.stringify({ tag: "Stop", choices: ["ignored", "anyways"] })
            );
          },
        });
      } else {
        switch (message.quiz.state) {
          case "Ready":
            console.log("Poll: Ready");
            if (session.onReady) session.onReady();
            break;

          case "Active":
            console.log("Poll: Active");
            session.ui.onActive(message.participants, message.quiz.choices);
            break;

          case "Finished":
            console.log("Poll: Finished");
            session.ui.onFinished(message.participants, message.quiz.choices);
            session.ui = null;
            break;
          default:
            console.log("Received unknown message: ", message);
        }
      }
    });
  });
}
