export default class Client {
  events = ["error", "ready", "result", "state", "connection"];

  readyCallbacks = [];
  resultCallbacks = [];
  stateCallbacks = [];
  connectionCallbacks = [];
  errorCallbacks = [];

  missedPing = 0;
  outgoingMS = 0;
  incomingMS = 0;

  constructor() {
    const websocketURL =
      Decker.meta.quizzer.socket || "ws://localhost:3001/api/websocket";
    const client = this;
    const socket = new WebSocket(websocketURL);
    socket.addEventListener("error", async (event) => {
      client.handleError(event);
    });
    socket.addEventListener("open", async (event) => {
      client.handleOpen(event);
    });
    socket.addEventListener("message", async (event) => {
      client.handleMessage(event);
    });
    this.socket = socket;
  }

  async handleError(event) {
    console.log("error");
    this.socket = undefined;
    for (const callback of this.errorCallbacks) {
      callback();
    }
  }

  async handleClose(event) {
    console.log("close");
    this.socket = undefined;
    for (const callback of this.errorCallbacks) {
      callback();
    }
  }

  async handleOpen(event) {
    if (!this.session) {
      try {
        await this.acquireNewSession();
      } catch (error) {
        this.handleError();
      }
    }
    try {
      await this.hostSession();
    } catch (error) {
      this.session = null;
      this.secret = null;
    }
    await this.startPinger();
    for (const callback of this.readyCallbacks) {
      callback(this.session, this.secret);
    }
  }

  async handleMessage(event) {
    if (event.data) {
      const json = JSON.parse(event.data);
      if (json.type === "state") {
        if (json.state.result) {
          for (const callback of this.resultCallbacks) {
            callback(json.state.result);
          }
        }
        let connections = 0;
        let done = 0;
        if (json.state.connections) {
          connections = json.state.connections;
        }
        if (json.state.done) {
          done = json.state.done;
        }
        for (const callback of this.stateCallbacks) {
          callback(connections, done);
        }
      } else if (json.type === "pong") {
        this.missedPing = 0;
        this.outgoingMS = performance.now() - this.lastPing;
        for (const callback of this.connectionCallbacks) {
          callback(undefined, this.outgoingMS, this.incomingMS);
        }
      } else if (json.type === "ping") {
        if (json.ms) {
          this.incomingMS = json.ms;
          for (const callback of this.connectionCallbacks) {
            callback(undefined, this.outgoingMS, this.incomingMS);
          }
        }
        this.sendPong();
      }
    }
  }

  on(event, callback) {
    if (!this.events.includes(event) || !callback) {
      return;
    }
    if (event === "ready") {
      this.readyCallbacks.push(callback);
    }
    if (event === "result") {
      this.resultCallbacks.push(callback);
    }
    if (event === "state") {
      this.stateCallbacks.push(callback);
    }
    if (event === "connection") {
      this.connectionCallbacks.push(callback);
    }
    if (event === "error") {
      this.errorCallbacks.push(callback);
    }
  }

  tryReconnect() {
    if (this.socket && this.socket.readyState === this.socket.OPEN) {
      console.error(
        "[QUIZZER CLIENT] Reconnect was called while the socket was still open."
      );
      return;
    }
    const websocketURL =
      Decker.meta.quizzer.socket || "ws://localhost:3001/api/websocket";
    const client = this;
    const socket = new WebSocket(websocketURL);
    socket.addEventListener("error", async (event) => {
      client.handleError(event);
    });
    socket.addEventListener("open", async (event) => {
      client.handleOpen(event);
    });
    socket.addEventListener("message", async (event) => {
      client.handleMessage(event);
    });
    socket.addEventListener("close", async (event) => {
      client.handleClose(event);
    });
    this.socket = socket;
  }

  async acquireNewSession() {
    const backend = Decker.meta.quizzer.backend || "http://localhost:3000";
    try {
      const response = await fetch(`${backend}/api/session`, {
        method: "POST",
      });
      const json = await response.json();
      this.session = json.id;
      this.secret = json.secret;
    } catch (error) {
      this.session = null;
      this.secret = null;
      console.error(error);
      throw new Error("Unable to POST a new session.");
    }
  }

  async hostSession() {
    if (this.socket.readyState !== this.socket.OPEN) {
      throw new Error("Socket unavailable");
    }
    try {
      this.socket.send(
        JSON.stringify({
          type: "connect",
          session: this.session,
          secret: this.secret,
        })
      );
    } catch (error) {
      console.error(error);
    }
  }

  async startPinger() {
    const client = this;
    this.pinger = setInterval(() => {
      client.missedPing++;
      client.sendPing();
    }, 1000);
  }

  async sendPing() {
    if (this.socket.readyState !== this.socket.OPEN) {
      console.error("[QUIZZER SOCKET] Error: The Socket is unavailable.");
      this.emergencyClose();
      return;
    }
    if (this.missedPing > 2) {
      console.error("[QUIZZER SOCKET] Missed third ping. Closing connection.");
      this.emergencyClose();
    } else {
      this.lastPing = performance.now();
      this.socket.send(JSON.stringify({ type: "ping" }));
    }
  }

  emergencyClose() {
    if (this.pinger) {
      clearInterval(this.pinger);
      this.pinger = undefined;
    }
    if (this.socket) {
      console.error("[QUIZZER SOCKET] The connection is being force-closed.");
      this.socket.close();
      this.socket = null;
      for (const callback of this.connectionCallbacks) {
        callback("CLOSED", undefined, undefined);
      }
    }
  }

  async sendPong() {
    if (this.socket.readyState !== this.socket.OPEN) {
      throw new Error("Socket unavailable");
    }
    this.socket.send(JSON.stringify({ type: "pong" }));
  }

  sendQuiz(quiz) {
    if (!this.socket || this.socket.readyState !== this.socket.OPEN) {
      throw new Error("Socket Unavailable");
    }
    this.socket.send(JSON.stringify({ type: "quiz", quiz: quiz }));
  }

  requestEvaluation() {
    if (this.socket.readyState !== this.socket.OPEN) {
      throw new Error("Socket Unavailable");
    }
    this.socket.send(JSON.stringify({ type: "evaluate" }));
  }
}
