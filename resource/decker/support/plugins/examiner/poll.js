// Manages a persistent connection to a polling server.
export { pollSession };

// This might be overkill, but it is the only generator in a proper ES6 module i
// could find.
import bwipjs from "./bwip.js";

var session = null;

// Creates a new polling session. Opens a web-socket connection to serverUrl and
// uses clientUrl as the polling client. If clientUrl is null the client
// provided by the server is used. onready is called when the session is
// established, onclose when the server terminates the session.
function pollSession({
  serverUrl,
  clientBaseUrl,
  onready = null,
  onclose = null,
  clientCss = null,
} = {}) {
  session = {
    id: null,
    socket: null,
    ui: null,
    heartbeat: null,
    onReady: onready,
    onClose: onclose,
  };

  return new Promise((resolve, error) => {
    //    console.log("[Examiner]: Creating Websocket for:", serverUrl);
    session.socket = new WebSocket(serverUrl);
    session.heartbeat = null;

    session.socket.addEventListener("open", (e) => {
      if (clientCss)
        session.socket.send(
          JSON.stringify({ tag: "ClientCss", clientCss: clientCss })
        );
      session.heartbeat = setInterval(() => {
        session.socket.send(JSON.stringify({ tag: "Beat" }));
      }, 10000);
    });

    session.socket.addEventListener("error", (e) => {
      console.error("[Examiner][Poll]", "Cannot connect to ", serverUrl);
      if (session.heartbeat) {
        clearInterval(session.heartbeat);
        session.heartbeat = null;
      }
    });

    session.socket.addEventListener("close", (e) => {
      console.error("[Examiner][Poll]", "Server went away.");
      if (session.onClose) session.onClose();
      if (session.heartbeat) {
        clearInterval(session.heartbeat);
        session.heartbeat = null;
      }
    });

    session.socket.addEventListener("message", (e) => {
      let message = undefined;
      try {
        let message = JSON.parse(e.data);
      } catch (error) {
        console.error("[Examiner][Poll] Unable to parse message:\n", e.data);
        return;
      }
      if (message.error) {
        console.error("[Examiner][Poll]", "Server error:", message.error);
      } else if (message.key != null) {
        session.id = message.key;
        session.clientUrl = clientBaseUrl
          ? `${clientBaseUrl}#${session.id}`
          : `${client(serverUrl)}#${session.id}`;

        resolve({
          // Returns the 4 digit session id and the client url for this session.
          sessionId: () => {
            return {
              id: session.id,
              url: session.clientUrl,
            };
          },

          // Constructs a qr code from the client url in the provided canvas element.
          fillQRCode: (canvas) => {
            bwipjs.toCanvas(canvas, {
              bcid: "qrcode", // Barcode type
              text: session.clientUrl, // Text to encode
              scale: 20, // Somehow gives 1000^2 pixels
              includetext: false,
              textxalign: "center",
              eclevel: "L", // Makes for smaller qr code sizes
            });
          },

          // Starts a new poll. choices is an array of strings that signify the
          // choices the clients have. Each entry results in a button on the
          // client. Example: ["A", "B", "C"]. votes is the number of choices
          // the client should be able to select. Callbacks is an object
          // containing two callbacks, onActive and onFinished.
          // onActive(participants, votes) is called whenever the state of an
          // active poll changes, where participants is the number of clients in
          // this session and a votes is an object containing the number of
          // votes for each choice in the active poll. Example: {25, ["A": 12,
          // "B": 9]}. onFinished(participants, votes) is called with the final
          // results when the poll has stopped.
          poll: (choices, solution, votes, callbacks, selection) => {
            session.ui = callbacks;
            let options = {
              tag: "Start",
              choices: choices,
              solution: solution,
              votes: votes,
            };
            if (selection) options.winnerselection = selection;
            session.socket.send(JSON.stringify(options));
          },

          // Terminates the active poll.
          stop: () => {
            session.socket.send(JSON.stringify({ tag: "Stop" }));
          },

          // Resets the session.
          reset: () => {
            session.socket.send(JSON.stringify({ tag: "Reset" }));
          },
          close: () => {
            session.socket.send(JSON.stringify({ tag: "Reset" }));
            session.socket.close();
            session.socket = null;
          },
        });
      } else {
        switch (message.quiz.state) {
          case "Ready":
            if (session.onReady) session.onReady();
            break;

          case "Active":
            if (session.ui)
              session.ui.onActive(
                message.participants,
                message.quiz.choices,
                message.quiz.complete
              );
            break;

          case "Finished":
            if (session.ui)
              session.ui.onFinished(
                message.participants,
                message.quiz.choices,
                message.quiz.complete
              );
            session.ui = null;
            break;

          default:
            console.error(
              "[Examiner][Poll] Received unknown message: ",
              message
            );
        }
      }
    });
  });
}

function client(str) {
  let url = new URL(str);
  if (url.protocol == "ws:") url.protocol = "http";
  else url.protocol = "https";
  url.pathname = url.pathname.replace(/quiz$/, "client");
  return url;
}
