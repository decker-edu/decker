"use strict";

var Poll = (() => {
  const server = "polls.hci.informatik.uni-wuerzburg.de";
  var socket = null; var poll = null; var timer = null;
  var pollState = "not-init";
  var admin = false;
  var results, canvas, email, lgn, pwd, loggedIn, error;
  var pollNum = 0;
  var labels = [];
  const alphabet = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ'];
  const qrdiv = document.createElement("div");
  qrdiv.id = "poll-overlay";
  qrdiv.classList.add("loading");
  document.querySelector(".reveal").appendChild(qrdiv);

  // Open a websocket to server and build QR code for poll
  function openPoll() {
    socket = new WebSocket("wss://" + server + "/poll");

    socket.onopen = () => {
      document.querySelectorAll(".countdown").forEach((timer) => {
        timer.innerHTML = timer.classList.contains("timed")
          ? clockTime(timer)[0] + ":" + clockTime(timer)[1]
          : "VOTE NOW";
      });
    };

    socket.onmessage = (event) => {
      let message = JSON.parse(event.data);
      console.log("message from server ", message);

      if (message.tag) {
        Reveal.removeKeyBinding(65);
        Reveal.addKeyBinding(
          {
            keyCode: 65,
            key: "A",
            description: "Toggle Audience Response Poll",
          },
          switchPollState
        );
        Reveal.addKeyBinding(
          {
            keyCode: 84,
            key: "t",
            description: "Toggle Audience Response Results",
          },
          showResults
        );
        buildCode(message.tag);
      }

      if (message.state !== undefined) {
        switch (message.state) {
          case "Ready":
            pollState = "idle";
            break;
          case "Active":
            updateVotes(message.choices, canvas);
            break;
          case "Finished":
            canvas.classList.add("finished");
            socket.send(JSON.stringify({ tag: "Reset" }));
            break;
          case "NotFound":
            error.innerText = "User account not found.";
            break;
          case "LoggedIn":
            loggedIn = true;
            admin = true;
            [lgn, pwd].forEach((el) => {
              el.value = "";
            });
            document.querySelector("#login-div").classList.remove("active");
            document
              .querySelectorAll(".fa-qrcode")
              .forEach((el) => el.classList.add("admin"));
            document.querySelector(".fa-sign-in-alt").classList.add("hidden");
            break;
        }
      }
    };

    socket.onerror = (error) => {
      console.log("Websocket connection error: ", error);
    };
  }

  // Parse poll duration (seconds) to clock time
  function clockTime(timer) {
    var duration = parseInt(timer.getAttribute("data-seconds"));
    var min = parseInt(duration / 60, 10);
    var sec = parseInt(duration % 60, 10);

    min = min < 10 ? "0" + min : min;
    sec = sec < 10 ? "0" + sec : sec;

    return [min, sec, duration];
  }

  // Given Poll ID from server, build QR Code & login div
  function buildCode(pollID) {
    const pollAddr = "https://" + server + "/poll.html/" + pollID;

    const link = document.createElement("a");
    link.setAttribute("href", pollAddr);
    link.innerText = pollAddr;

    const lgnDiv = document.createElement("div");
    lgnDiv.id = "login-div";

    lgn = document.createElement("input");
    lgn.type = "text";
    lgn.placeholder = "Email Address";

    pwd = document.createElement("input");
    lgn.type = "text";
    pwd.placeholder = "Password";
    pwd.type = "password";

    const btn = document.createElement("button");
    btn.type = "button";
    btn.innerText = "Submit";
    btn.addEventListener("click", () => {
      email = lgn.value;
      socket.send(
        JSON.stringify({ tag: "Login", name: lgn.value, pwd: pwd.value })
      );
      error.innerText = "";
    });

    const cancel = document.createElement("button");
    cancel.type = "button";
    cancel.innerText = "Cancel";
    cancel.addEventListener("click", () => {
      document.querySelector("#login-div").classList.remove("active");
      [lgn, pwd].forEach((el) => {
        el.value = "";
      });
    });

    const btnDiv = document.createElement("div");
    [btn, cancel].forEach((el) => btnDiv.appendChild(el));

    error = document.createElement("div");
    error.id = "errorDiv";
    error.innerText = "";

    [lgn, pwd, btnDiv, error].forEach((el) => {
      lgnDiv.appendChild(el);
    });

    const i = document.createElement("i");
    i.classList.add("fas", "fa-sign-in-alt", "gears");
    [link, i, lgnDiv].forEach((el) => {
      qrdiv.appendChild(el);
    });

    i.addEventListener("click", () => {
      lgnDiv.classList.add("active");
    });

    const size = parseInt(qrdiv.style.width, 10) || 600;
    new QRCode(qrdiv, {
      text: pollAddr,
      width: size,
      height: size,
      colorDark: "#000000",
      colorLight: "#ffffff",
      correctLevel: QRCode.CorrectLevel.H,
    });
  }

  // 'a' to start / stop poll - also stopped when timer ends
  function switchPollState() {
    let overlay = document.querySelector("#poll-overlay");
    if (overlay.classList.contains("active"))
      overlay.classList.remove("active");

    switch (pollState) {
      case "not_init":
        console.error("Cannot start poll before socket is connected.");
        break;
      case "idle":
        if (poll !== null) {
          startPoll();
          pollState = "started";
        } else {
          alert("Please navigate to question before starting poll.");
        }
        break;
      case "started":
        if (!timer.classList.contains("timed")) {
          stopPoll();
          pollState = "idle";
        }
        break;
      default:
        console.error("Error with poll.");
        break;
    }
  }

  // On slide transition update vars and add letters to choices
  function updateSlide() {
    labels = [];
    let sl = Reveal.getCurrentSlide();
    if (sl.classList.contains("poll")) {
      poll = sl;
      canvas = sl.querySelector("canvas");
      results = sl.querySelector(".poll_results");
      Array.from(sl.querySelectorAll('.choice_ltr')).sort().forEach(
        (c,i) => { c.innerText = alphabet[i] + "."; });
    }
  }

  // Re-write chart labels to letters and add to quiz choices
  function writeLabels() {
    let ch = JSON.parse(canvas.innerHTML.replace('<!-- ','').replace(' -->',''));
    labels = alphabet.slice(0,Object.keys(ch.data.labels).length);
    canvas.chart.data.labels = labels;
    canvas.chart.update();
  }

  // 't' to show poll results
  function showResults() {
    if (labels && labels.length === 0) { writeLabels(); } 
    results = Reveal.getCurrentSlide().querySelector(".poll_results");
    results.classList.toggle("active");
    handleResults();
  }

  // Push choices to server with Start tag
  function startPoll() {
    pollNum++;
    var choices = [];
    timer = poll.querySelector(".countdown");
    startTimer();

    poll.querySelectorAll("ul.choices li").forEach((choice) => {
      choices.push(choice.lastChild.textContent);
    });
    socket.send(JSON.stringify({ tag: "Start", choices: choices }));
  }

  function startTimer() {
    if (timer.classList.contains("timed")) {
      var duration = Math.floor(timer.getAttribute("data-seconds")) - 1;

      var pollTimer = setInterval(() => {
        if (duration > 0) {
          var min = Math.floor(duration / 60);
          var sec = Math.floor(duration % 60);
          min = min < 10 ? "0" + min : min;
          sec = sec < 10 ? "0" + sec : sec;
          timer.innerHTML = `${min}:${sec}`;
          if (duration < 6) {
            timer.classList.add("hurry");
          }
        } else {
          timer.classList.remove("timed");
          clearInterval(pollTimer);
          switchPollState();
          return;
        }
        duration -= 1;
      }, 1000);
    }
    timer.classList.add("active");
  }

  // Update chart and list of votes by response
  function updateVotes(choices, canvas) {
    let votes = [];
    let sorted = Object.keys(choices).sort().reduce((result,key) => {
      result[key] = choices[key];
      return result;
    }, {});
    Object.values(sorted).forEach((val) => {
      votes.push(val);
    });
    canvas.chart.data.datasets[0].data = votes;
    canvas.chart.update();
  }

  // Send Stop to server and clear poll values
  function stopPoll() {
    timer.classList.remove("active");
    timer.classList.remove("hurry");
    if (socket == null) return;

    poll.querySelectorAll("ul.choices li").forEach((choice) => {
      choice.classList.remove("started");
    });
    document.querySelector("#poll-overlay").classList.remove("active");
    let question = poll.querySelector("h1").textContent;
    let date = new Date().toUTCString();
    let correct = poll.querySelector(".correct").innerText;

    socket.send(
      JSON.stringify({
        tag: "Stop",
        num: String(pollNum),
        date: date,
        question: question,
        correct: correct,
        email: admin ? email : "",
      })
    );
    poll = null;
    timer = null;
  }

  // Allow dragging of results
  function handleResults() {
    var pos1 = 0,
      pos2 = 0,
      pos3 = 0,
      pos4 = 0;
    results.onmousedown = dragMouseDown;

    function dragMouseDown(e) {
      e = e || window.event;
      e.preventDefault();
      results.style.cursor = "move";
      pos3 = e.clientX;
      pos4 = e.clientY;
      document.onmouseup = closeDragElement;
      document.onmousemove = elementDrag;
    }

    function elementDrag(e) {
      e = e || window.event;
      e.preventDefault();
      pos1 = pos3 - e.clientX;
      pos2 = pos4 - e.clientY;
      pos3 = e.clientX;
      pos4 = e.clientY;
      results.style.top = results.offsetTop - pos2 + "px";
      results.style.left = results.offsetLeft - pos1 + "px";
    }

    function closeDragElement() {
      results.style.cursor = "";
      document.onmouseup = null;
      document.onmousemove = null;
    }
  }

  return {
    init: () => {
      return new Promise((resolve) => {
        Reveal.addEventListener("slidechanged", e => { updateSlide(); });
        Reveal.removeKeyBinding(67);
        Reveal.addKeyBinding(
          {
            keyCode: 67,
            key: "C",
            description: "Toggle Audience Response Code",
          },
          () => {
            qrdiv.classList.toggle("active");
            if (socket == null) openPoll();
          }
        );
        document.onclose = () => {
          socket.close();
        };
        resolve();
      });
    },
  };
})();

Reveal.registerPlugin("Poll", Poll);
