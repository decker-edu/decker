import { io } from "./libraries/socket.io/socket.io.esm.min.js";

export default class Client {
  events = ["error", "ready", "result", "state", "connection"];

  readyCallbacks = [];
  resultCallbacks = [];
  stateCallbacks = [];
  connectionCallbacks = [];
  errorCallbacks = [];

  pinger = undefined;

  constructor() {
    const socketURL = Decker.meta.quizzer.socket || "ws://localhost:3000";
    const iosocket = io(socketURL);
    iosocket.on("connect", () => {
      console.log("socket.io: connect");
      this.handleConnect();
    });
    iosocket.on("disconnect", (reason) => {
      console.log("socket.io: disconnect");
      for (const callback of this.errorCallbacks) {
        callback("disconnect", reason);
      }
    });
    iosocket.on("state", (connections, done, result) => {
      console.log(connections, done, result);
      if (result) {
        for (const callback of this.resultCallbacks) {
          callback(result);
        }
      } else {
        for (const callback of this.stateCallbacks) {
          callback(connections, done);
        }
      }
    });
    iosocket.on("error", (message) => {
      console.error(`[socket.io error] ${message}`);
      for (const callback of this.connectionCallbacks) {
        callback("Error", undefined);
      }
    });
    this.pinger = setInterval(() => {
      const start = performance.now();
      iosocket.volatile.emit("ping", () => {
        const ms = performance.now() - start;
        for (const callback of this.connectionCallbacks) {
          callback(undefined, ms);
        }
      });
    }, 1000);
    this.iosocket = iosocket;
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
    for (const callback of this.errorCallbacks) {
      callback();
    }
  }

  async handleConnect() {
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
    for (const callback of this.readyCallbacks) {
      callback(this.session, this.secret);
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
    if (!this.iosocket.active) {
      console.error("socket dead");
    }
    try {
      this.iosocket.emit("attach", this.session, this.secret);
    } catch (error) {
      console.error(error);
    }
  }

  close() {}

  sendQuiz(quiz) {
    this.iosocket.emit("quiz", quiz);
  }

  requestEvaluation() {
    this.iosocket.emit("evaluate");
  }
}
