import { io } from "./libraries/socket.io/socket.io.esm.min.js";

export default class Client {
  events = ["error", "ready", "result", "participants", "state", "pong"];

  readyCallbacks = [];
  resultCallbacks = [];
  stateCallbacks = [];
  pongCallbacks = [];
  participantsCallbacks = [];
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

    /**
     * Handle a new or recovered connection.
     */
    iosocket.on("connect", () => this.handleConnect());
    /**
     * If we get a connect_error, we are probably offline as the
     * endpoint is unreachable.
     */
    iosocket.on("connect_error", (error) => {
      console.error("socket.io:", error.message);
      for (const ecb of this.errorCallbacks) {
        ecb("connect", undefined);
      }
    });
    /**
     * On disconnect, check why we disconnected. If it is not a manual disconnect,
     * report disconnection.
     */
    iosocket.on("disconnect", (reason) => {
      if (reason === "io client disconnect") {
        return;
      }
      for (const ecb of this.errorCallbacks) {
        ecb("disconnect", reason);
      }
      if (!iosocket.active) {
        iosocket.connect();
      }
    });
    /**
     * On participants, update tally span.
     */
    iosocket.on("participants", (connections, done) => {
      for (const callback of this.participantsCallbacks) {
        callback(connections, done);
      }
    });
    /**
     * On result, publish results.
     */
    iosocket.on("result", (result) => {
      for (const callback of this.resultCallbacks) {
        callback(result);
      }
    });
    /**
     * On internal error: Log.
     */
    iosocket.on("error", (message) => {
      console.error(`[socket.io error] ${message}`);
      for (const ecb of this.errorCallbacks) {
        ecb("server error", message);
      }
    });
    /**
     * Create the ping interval to observe latency.
     */
    this.pinger = setInterval(() => {
      const start = performance.now();
      iosocket.volatile.emit("ping", () => {
        const ms = performance.now() - start;
        for (const callback of this.pongCallbacks) {
          callback(ms);
        }
      });
    }, 1000);
    this.iosocket = iosocket;
  }

  /**
   * Handles the Socket.IO connect event.
   * If we are a recovered socket, do nothing.
   * @returns
   */
  async handleConnect() {
    if (this.iosocket.recovered) {
      return;
    }
    if (!this.session) {
      try {
        await this.acquireNewSession();
      } catch (error) {
        console.error(error);
        for (const ecb of this.errorCallbacks) {
          ecb("get session");
        }
      }
    }
    try {
      await this.hostSession();
      for (const callback of this.readyCallbacks) {
        callback(this.session, this.secret);
      }
    } catch (error) {
      this.session = null;
      this.secret = null;
      if (error === "no session") {
        for (const ecb of this.errorCallbacks) {
          ecb("session lost");
        }
      }
      if (error === "wrong secret") {
        for (const ecb of this.errorCallbacks) {
          ecb("session lost");
        }
      }
    }
  }

  /**
   * Registers event callbacks.
   * Available events are: ready, result, state (unused), participatns, pong and error
   */
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
    if (event === "participants") {
      this.participantsCallbacks.push(callback);
    }
    if (event === "pong") {
      this.pongCallbacks.push(callback);
    }
    if (event === "error") {
      this.errorCallbacks.push(callback);
    }
  }

  /**
   * Sends a POST request to the server to create a new session for us.
   * The response contains the session id and the secret to authenticate as the host.
   */
  async acquireNewSession() {
    let backend = Decker.meta.quizzer?.url || "http://localhost:3000/";
    if (backend.slice(-1) !== "/") {
      backend = backend + "/";
    }
    try {
      const response = await fetch(`${backend}api/session`, {
        method: "POST",
      });
      if (response.ok) {
        const json = await response.json();
        this.session = json.id;
        this.secret = json.secret;
      } else {
        throw new Error("session response");
      }
    } catch (error) {
      this.session = null;
      this.secret = null;
      throw new Error("fetch session");
    }
  }

  /**
   * Request to host the given session by providing the given secret.
   * Sets this socket connection as the host on the server.
   * @returns
   */
  async hostSession() {
    return new Promise((resolve, reject) => {
      this.iosocket.emit(
        "attach",
        this.session,
        this.secret,
        (session, error) => {
          if (error) {
            reject(error);
          } else {
            resolve();
          }
        }
      );
    });
  }

  /**
   * Create a network object from the current quiz.
   * Network Quiz Objects lack certain data like labels that may not be renderable
   * on the client.
   * @param {*} quiz
   */
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

  /**
   * Disconnect the socket in anticipation that its reference will be discarded.
   */
  destroy() {
    this.iosocket.disconnect();
  }
}
