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
    let socketURL = Decker.meta.quizzer?.url || "ws://localhost:3000/";
    if (socketURL.slice(-1) !== "/") {
      socketURL = socketURL + "/";
    }
    const surl = new URL(socketURL);
    const subpath = surl.pathname;
    const iosocket = io(`${surl.protocol}//${surl.host}`, {
      path: subpath + "socket.io",
    });
    iosocket.on("connect", () => {
      this.handleConnect();
    });
    iosocket.on("connect_error", (error) => {
      console.error("socket.io:", error.message);
      for (const callback of this.errorCallbacks) {
        callback("connect", undefined);
      }
    });
    iosocket.on("disconnect", (reason) => {
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
      for (const callback of this.errorCallbacks) {
        callback("unknown");
      }
    });
    this.pinger = setInterval(() => {
      const start = performance.now();
      iosocket.volatile.emit("ping", () => {
        const ms = performance.now() - start;
        for (const callback of this.connectionCallbacks) {
          callback(ms);
        }
      });
    }, 1000);
    this.iosocket = iosocket;
  }

  async handleConnect() {
    if (!this.session) {
      try {
        await this.acquireNewSession();
      } catch (error) {
        for (const callback of this.errorCallbacks) {
          callback("get session");
        }
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
    let backend = Decker.meta.quizzer?.url || "http://localhost:3000/";
    if (backend.slice(-1) !== "/") {
      backend = backend + "/";
    }
    try {
      const response = await fetch(`${backend}api/session`, {
        method: "POST",
      });
      const json = await response.json();
      this.session = json.id;
      this.secret = json.secret;
    } catch (error) {
      this.session = null;
      this.secret = null;
      console.error(error);
      for (const callback of this.errorCallbacks) {
        callback("get session", undefined);
      }
      throw new Error("Unable to POST a new session.");
    }
  }

  async hostSession() {
    if (!this.iosocket.active) {
      console.error("socket dead");
    }
    try {
      this.iosocket.emit(
        "attach",
        this.session,
        this.secret,
        (session, error) => {
          if (error) {
            this.session = undefined;
          } else {
            if (session === "host") {
              return;
            } else {
              this.session = undefined;
              // Session exists but secret was wrong: How?
            }
          }
        }
      );
    } catch (error) {
      console.error(error);
    }
  }

  sendQuiz(quiz) {
    const networkQuiz = {
      type: quiz.type,
      choices: [],
    };
    for (const choice of quiz.choices) {
      const networkChoice = {
        votes: choice.votes,
        options: [],
        categories: [],
      };
      for (const option of choice.options) {
        const networkOption = {
          letter: option.letter,
          correct: option.correct,
          reason: undefined,
        };
        if (quiz.type === "assignment") {
          networkOption.reason = option.reason;
        }
        if (quiz.type === "selection" || quiz.type === "freetext") {
          networkOption.label = option.label;
        }
        networkChoice.options.push(networkOption);
      }
      if (choice.categories) {
        for (const category of choice.categories) {
          const networkCategory = { number: category.number };
          networkChoice.categories.push(networkCategory);
        }
      }
      networkQuiz.choices.push(networkChoice);
    }
    this.iosocket.emit("quiz", networkQuiz);
  }

  requestEvaluation() {
    this.iosocket.emit("evaluate");
  }
}
