export default class Client {
  events = ["ready", "result", "state"];

  readyCallbacks = [];
  resultCallbacks = [];
  stateCallbacks = [];

  constructor() {
    const websocketURL =
      Decker.meta.quizzer.socket || "ws://localhost:3001/api/websocket";
    try {
      const client = this;
      const socket = new WebSocket(websocketURL);
      socket.addEventListener("open", async (event) => {
        await this.acquireNewSession();
        await this.hostSession();
        for (const callback of this.readyCallbacks) {
          callback(this.session, this.secret);
        }
      });
      socket.addEventListener("message", (event) => {
        if (event.data) {
          const json = JSON.parse(event.data);
          if (json.type === "state") {
            if (json.state.result) {
              for (const callback of client.resultCallbacks) {
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
          }
        }
      });
      this.socket = socket;
    } catch (error) {
      console.error(error);
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
      // Handle Error
    }
  }

  async hostSession() {
    if (this.socket.readyState !== this.socket.OPEN) {
      throw new Error("Socket Unavailable");
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
      // Handle POST
    }
  }

  sendQuiz(quiz) {
    if (this.socket.readyState !== this.socket.OPEN) {
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
