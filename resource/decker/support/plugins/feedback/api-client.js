class RESTClient {
  base = "localhost:8080";
  cors = false;

  constructor(base) {
    this.base = base;
    this.cors = window.location.origin !== new URL(base).origin;
  }

  async getVersion() {
    return fetch(this.base + "/version", {
      method: "GET",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
    }).then((response) => response.json());
  }

  async getToken(deck) {
    return fetch(`${this.base}/token?deckid=${deck}`, {
      method: "GET",
      mode: this.cors ? "cors" : "same-origin",
      credentials: this.cors ? "omit" : "include",
      cache: "no-store",
    }).then((response) => response.json());
  }

  async getLogin(credentials) {
    const response = await fetch(this.base + "/login", {
      method: "PUT",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      body: JSON.stringify(credentials),
    });
    // For some reason the backend always returns a 200, even if the login failed.
    if (response.ok) {
      try {
        return await response.json();
      } catch (error) {
        console.error(
          "Parsing of login data failed. Credentials are probably invalid."
        );
        throw "Login failed.";
      }
    } else {
      throw "Login failed.";
    }
  }

  async getComments(deck, slide, token) {
    let data = { deck, slide, token };
    return fetch(this.base + "/comments", {
      /* Need to use put, because server does not accept data in
             request body of GET. */
      method: "PUT",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      body: JSON.stringify(data),
    }).then((response) => response.json());
  }

  async submitComment(deck, slide, token, markdown, id, location) {
    let data = { deck, slide, token, markdown, id, location };
    return fetch(this.base + "/comments", {
      method: "POST",
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    }).then((response) => response.json());
  }

  async deleteComment(key, token) {
    let data = { key, token };
    return fetch(this.base + "/comments", {
      method: "DELETE",
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    });
  }

  async voteComment(vote) {
    return fetch(this.base + "/vote", {
      method: "PUT",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      body: JSON.stringify(vote),
    });
  }

  async postAnswer(comment, token, markdown, link) {
    let data = { comment, markdown, link, token };
    return fetch(this.base + "/answers", {
      method: "POST",
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    });
  }

  async deleteAnswer(key, token) {
    let data = { key, token };
    return fetch(this.base + "/answers", {
      method: "DELETE",
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    });
  }
}

export default RESTClient;
