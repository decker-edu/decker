let anchor = Decker.anchor(import.meta);
let pollId = location.hash.slice(1);
let pollServer = `${Decker.meta["poll-server"]}/${pollId}`;

anchor.innerHTML = `
  <h2>Poll #${pollId} on ${Decker.meta["poll-server"]}</h2>
  <p class="instruct"></p>
  <div class="buttons"></div>
`;

window.addEventListener("load", () => {
  let socket = new WebSocket(pollServer);

  socket.addEventListener("open", (e) => {
    console.log("Poll: connected to session", pollId);
  });

  socket.addEventListener("error", (e) => {
    console.log("Poll: ERROR connecting to session", pollId);
  });

  socket.addEventListener("close", (e) => {
    console.log("Poll: ERROR lost connection to session", pollId);
  });

  socket.addEventListener("message", (e) => {
    let message = JSON.parse(e.data);
    let buttons = anchor.querySelector("div.buttons");
    let instruct = anchor.querySelector("p.instruct");

    console.log("Poll:", "message from server:", message);
    switch (message.tag) {
      case "Idle":
        while (buttons.firstChild) {
          buttons.removeChild(buttons.firstChild);
        }
        anchor.classList.remove("polling");
        break;
      case "Begin":
        for (let name of message.choices) {
          instruct.innerHTML =
            message.votes == 1
              ? `Welches Schweinderl hätten's denn gern?`
              : `Welche ${message.votes} Schweinderl hätten's denn gern?`;
          let button = document.createElement("button");
          button.setAttribute("id", `B${name}`);
          button.setAttribute("label", name);
          button.textContent = name;
          button.addEventListener("click", (_) => {
            for (let button of buttons.children)
              button.classList.remove("choice");
            button.classList.add("choice");
            socket.send(JSON.stringify({ choice: [name] }));
          });
          buttons.appendChild(button);
        }
        anchor.classList.add("polling");
        break;
      case "End":
        buttons.classList.add("ended");
        for (let button of buttons.children) {
          button.setAttribute("disabled", true);
        }
        break;
    }
  });
});
