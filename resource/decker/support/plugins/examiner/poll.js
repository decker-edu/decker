// Manages a persistent connection to a polling server.
export { pollSession };

// This might be overkill, but it is the only generator in a proper ES6 module i
// could find.
import bwipjs from "https://cdnjs.cloudflare.com/ajax/libs/bwip-js/3.0.4/bwip-js.mjs";

var session = null;

// Creates a new polling session. Opens a web-socket connection to serverUrl and
// uses clientUrl as the polling client. If clientUrl is null the client
// provided by the server is used. onready is called when the session is
// established, onclose when the server terminates the session.
function pollSession({
  serverUrl,
  clientBaseUrl,
  clientCss = "",
  onready = null,
  onclose = null,
} = {}) {
  session = {
    id: null,
    socket: null,
    ui: null,
    onReady: onready,
    onClose: onclose,
  };

  return new Promise((resolve, error) => {
    session.socket = new WebSocket(serverUrl);

    session.socket.addEventListener("open", (e) => {
      console.log("Poll:", "server connected.");
      if (clientCss)
        session.socket.send(JSON.stringify({ tag: "ClientCss", clientCss: clientCss }));
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
        session.id = message.key;
        session.clientUrl = clientBaseUrl
          ? `${clientBaseUrl}#${session.id}`
          : `${client(serverUrl)}#${session.id}`;

        resolve({
          // Returns the 4 digit session id and the client url for this session.
          sessionId: () => {
            return { id: session.id, url: session.clientUrl };
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
          poll: (choices, votes, callbacks = { choices: [], votes: 1 }) => {
            session.ui = callbacks;
            session.socket.send(
              JSON.stringify({
                tag: "Start",
                choices: choices,
                votes: votes,
              })
            );
          },

          // Terminates the active poll.
          stop: () => {
            session.socket.send(JSON.stringify({ tag: "Stop" }));
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

function client(str) {
  let url = new URL(str);
  if (url.protocol == "ws:") url.protocol = "http";
  else url.protocol = "https";
  url.path += "/client";
  return url;
}
