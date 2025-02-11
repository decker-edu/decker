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
    const response = await fetch(`${this.base}/token`, {
      method: "GET",
      mode: this.cors ? "cors" : "same-origin",
      credentials: this.cors ? "omit" : "include",
      cache: "no-store",
    });
    if (response.ok) {
      return await response.json();
    } else {
      console.log(response);
      throw "acquire token failed";
    }
  }

  async getLogin(credentials) {
    const response = await fetch(this.base + "/session", {
      method: "POST",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(credentials),
    });
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
    const deckURI = encodeURIComponent(deck);
    const slideURI = encodeURIComponent(slide);
    let response;
    if (!slide) {
      response = await fetch(this.base + `/comments/${deckURI}`, {
        method: "GET",
        headers: {
          Authorization: `Bearer ${token}`,
        },
        mode: this.cors ? "cors" : "same-origin",
        cache: "no-store",
      });
    } else {
      response = await fetch(this.base + `/comments/${deckURI}/${slideURI}`, {
        method: "GET",
        headers: {
          Authorization: `Bearer ${token}`,
        },
        mode: this.cors ? "cors" : "same-origin",
        cache: "no-store",
      });
    }
    if (response.ok) {
      return await response.json();
    } else {
      throw "error fetching comments";
    }
  }

  async submitComment(deck, slide, token, markdown, id, location) {
    const deckURI = encodeURIComponent(deck);
    const slideURI = encodeURIComponent(slide);
    let data = {
      deck: deckURI,
      slide: slideURI,
      token: token,
      comment: markdown,
      id: id,
      location: location,
    };
    const response = await fetch(
      this.base + `/comments/${deckURI}/${slideURI}`,
      {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
        mode: this.cors ? "cors" : "same-origin",
        body: JSON.stringify(data),
      }
    );
    if (response.ok) {
      return response.json();
    } else {
      throw "can not submit comment";
    }
  }

  async deleteComment(commentID, token) {
    return fetch(this.base + `/comments/${commentID}`, {
      method: "DELETE",
      headers: { Authorization: `Bearer ${token}` },
      mode: this.cors ? "cors" : "same-origin",
    });
  }

  async postVote(vote) {
    return fetch(this.base + `/votes/${vote.comment}`, {
      method: "POST",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      headers: {
        Authorization: `Bearer ${vote.voter}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify(vote),
    });
  }

  async deleteVote(vote) {
    return fetch(this.base + `/votes/${vote.comment}`, {
      method: "POST",
      mode: this.cors ? "cors" : "same-origin",
      cache: "no-store",
      headers: {
        Authorization: `Bearer ${vote.voter}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify(vote),
    });
  }

  async postAnswer(comment, token, markdown, link) {
    let data = { comment, markdown, link, token };
    return fetch(this.base + `/answers`, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    });
  }

  async deleteAnswer(key, token) {
    let data = { key, token };
    return fetch(this.base + `/answers/${key}`, {
      method: "DELETE",
      headers: {
        Authorization: `Bearer ${token}`,
      },
      mode: this.cors ? "cors" : "same-origin",
      body: JSON.stringify(data),
    });
  }
}

export default RESTClient;
