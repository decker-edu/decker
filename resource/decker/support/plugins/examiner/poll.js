// Manages a persistent connection to a polling server.
export { pollSession };

// This might be overkill, but it is the only generator in a proper ES6 module i
// could find.
import bwipjs from "./bwip.js";

let session = null;

// For manual debugging because the browser's debug functions can not simulate
// Sockets losing their connection ...
window.closeQuizzerConnection = function () {
  if (session) {
    session.socket.close();
  }
};

let heartbeatWithoutResponse = 0;

function setupHeartbeat() {
  session.heartbeat = setInterval(() => {
    if (heartbeatWithoutResponse === 1) {
      if (session.onWarning) {
        session.onWarning();
      }
    } else if (heartbeatWithoutResponse === 2) {
      // Pull the ripchord and close the conenction
      console.log("Missed two heartbeats: closing connection");
      heartbeatWithoutResponse++;
      session.socket.close();
      session.socket = null;
      if (session.onClose) {
        session.onClose();
      }
    }
    if (heartbeatWithoutResponse < 2) {
      session.socket.send(JSON.stringify({ tag: "Beat" }));
    }
    heartbeatWithoutResponse++;
  }, 10000);
}

function reconnect(objectToSend) {
  console.log("[POLL]", "attempting to reconnect to quiz server");
  if (session.secret) {
    // Use the secret to reconnect to existing session
    session.socket = new WebSocket(
      `${session.serverUrl}/${session.id}/${session.secret}`
    );
  } else {
    // If for some reason we have no secret at least try to create a new server session
    session.socket = new WebSocket(`${session.serverUrl}`);
  }
  // We have to reattach all the listeners to the new socket
  session.socket.addEventListener("open", handleOpenPostReconnect);
  if (objectToSend) {
    session.socket.addEventListener(
      "open",
      () => {
        session.socket.send(JSON.stringify(objectToSend));
      },
      { once: true }
    );
  }
  session.socket.addEventListener("error", handleError);
  session.socket.addEventListener("close", handleClose);
  session.socket.addEventListener("message", handleMessagePostReconnect);
}

function handleOpenPostReconnect(event) {
  setupHeartbeat();
}

function handleError(event) {
  console.error("Poll:", "Cannot connect to server: " + session.serverUrl);
  if (session.heartbeat) {
    clearInterval(session.heartbeat);
    session.heartbeat = null;
  }
}

/* Closing a websocket involves it sending a close handshake.
 * If both participants agree to the close handshake the connection
 * is considered closed. If we have no network connection, this
 * handshake is never performed properly and after about a minute
 * the connection is considered closed. I do not want to wait this long,
 * so the heartbeat ripchord already sets up the system to allow a reconnect.
 */
function handleClose(event) {
  console.error("Poll:", "Server went away.");
  if (session.socket) {
    if (session.onClose) session.onClose();
    if (session.heartbeat) {
      clearInterval(session.heartbeat);
      session.heartbeat = null;
    }
    session.socket = null;
  }
}

async function handleMessagePostReconnect(event) {
  heartbeatWithoutResponse = 0;
  const message = JSON.parse(event.data);
  if (message.error) {
    // Error from Server
    console.error("Poll:", "Server error:", message.error);
  } else if (message.key) {
    // Key Delivery from Server, happens both on a new session and a reconnect
    if (message.key !== session.id) {
      session.id = message.key;
    }
    if (message.secret && message.secret !== session.secret) {
      session.secret = message.secret;
    }
  } else if (message.quiz) {
    // Quiz State from Server
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
        console.error("Received unknown message: ", message);
    }
  } else {
    console.log("received message had no expected data:");
    console.log(message);
  }
}

// Creates a new polling session. Opens a web-socket connection to serverUrl and
// uses clientUrl as the polling client. If clientUrl is null the client
// provided by the server is used. onready is called when the session is
// established, onclose when the server terminates the session.
function pollSession({
  serverUrl,
  clientBaseUrl,
  onready = null,
  onclose = null,
  onwarning = null,
  clientCss = null,
} = {}) {
  session = {
    serverUrl: serverUrl,
    clientUrl: null,
    id: null,
    secret: null,
    socket: null,
    ui: null,
    heartbeat: null,
    onReady: onready,
    onClose: onclose,
    onWarning: onwarning,
  };

  window.qsession = session;

  return new Promise((resolve, error) => {
    session.socket = new WebSocket(serverUrl);
    session.heartbeat = null;

    session.socket.addEventListener("open", (e) => {
      if (clientCss) {
        session.socket.send(
          JSON.stringify({ tag: "ClientCss", clientCss: clientCss })
        );
      }
      setupHeartbeat();
    });

    session.socket.addEventListener("error", handleError);

    session.socket.addEventListener("close", handleClose);

    // This is not very DRY but this promise has to be resolved somewhere ...
    session.socket.addEventListener("message", (e) => {
      heartbeatWithoutResponse = 0;
      let message = JSON.parse(e.data);
      if (message.error) {
        console.error("Poll:", "Server error:", message.error);
      } else if (message.key) {
        session.id = message.key;
        if (message.secret) {
          session.secret = message.secret;
        }
        session.clientUrl = clientBaseUrl
          ? `${clientBaseUrl}#${session.id}`
          : `${client(serverUrl)}#${session.id}`;

        resolve({
          // Returns the 4 digit session id and the client url for this session.
          getData: () => {
            return {
              id: session.id,
              secret: session.secret,
              url: session.clientUrl,
              socket: session.socket,
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
            if (!session.socket) {
              reconnect(options);
            } else {
              session.socket.send(JSON.stringify(options));
            }
          },

          // Terminates the active poll.
          stop: () => {
            const message = { tag: "Stop" };
            if (!session.socket) {
              reconnect(message);
            } else {
              session.socket.send(JSON.stringify(message));
            }
          },

          // Resets the session.
          reset: () => {
            const message = { tag: "Reset" };
            if (!session.socket) {
              reconnect(message);
            } else {
              session.socket.send(JSON.stringify(message));
            }
          },
          // Close session if socket is still open.
          close: () => {
            if (session.socket) {
              session.socket.send(JSON.stringify({ tag: "Reset" }));
              session.socket.close();
            }
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
            console.error("Received unknown message: ", message);
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
