"use strict";

var Poll = (() => {
    return {
        init: () => { 
            return new Promise(resolve => {   
                Reveal.removeKeyBinding( 65 );
                Reveal.removeKeyBinding( 67 );
                Reveal.addKeyBinding( { keyCode: 65, key: "A", description: "poll Response Poll" }, switchPollState );
                Reveal.addKeyBinding( { keyCode: 67, key: "C", description: "Toggle Poll Link" }, () => {
                  document.querySelector('#poll-overlay').classList.toggle('active');
                } );
                openPoll();
                window.onbeforeunload = function() {
                  socket.send(JSON.stringify({ "tag": "Close", "addr": Reveal.getConfig().pollEmail || "" }));
                  socket.close();
                  return "Email results?";
                };
                resolve();
            });
        }
    }
})();

const server = Reveal.getConfig().pollServer || "https://polls.hci.informatik.uni-wuerzburg.de";
var socket = null; var pollID = null; var poll = null; var timer = null;
var pollState = "not-init";
var canvas, qrdiv;
var choices = []; 

// Open a websocket to server and build QR code for poll
function openPoll() {
    if (socket != null) return;
    socket = new WebSocket("ws://" + server + "/poll", 41080);
  
    socket.onopen = () => { 
      document.querySelectorAll('.countdown').forEach(timer => {
        timer.innerHTML = 
          timer.classList.contains('timed') ? 
            clockTime(timer)[0] + ":" + clockTime(timer)[1] : 
            "VOTE NOW";
      });
    };

    socket.onmessage = event => { 
      let message = JSON.parse(event.data);
      // console.log("message from server ", message);

      if (message.key !== undefined && pollID == null) { 
        pollID = message.key;
        buildCode();
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
            socket.send(JSON.stringify( {"tag": "Reset"} ));
            break;
        }
      }
    }  

    socket.onerror = error => {
      console.log("Websocket connection error: ", error);
    }  
}

// Parse poll duration (seconds) to clock time
function clockTime(timer) {
  var duration = parseInt(timer.getAttribute('data-seconds'));
  var min = parseInt(duration / 60, 10);
  var sec = parseInt(duration % 60, 10);

  min = min < 10 ? "0" + min : min;
  sec = sec < 10 ? "0" + sec : sec;

  return [min,sec,duration];
}

// Given Poll ID from server, build QR Code  
function buildCode() {
    const pollAddr = "http://" + server + "/poll.html#" + pollID; 
    qrdiv = document.createElement('div');
    qrdiv.id = "poll-overlay";
    
    const link = document.createElement('a');
    link.setAttribute('href', pollAddr);    
    link.innerText = pollAddr;
    qrdiv.appendChild(link);

    const size = parseInt(qrdiv.style.width, 10) || 600;
    new QRCode(qrdiv, {
      text:         pollAddr,
      width:        size,
      height:       size,
      colorDark:    "#000000",
      colorLight:   "#ffffff",
      correctLevel: QRCode.CorrectLevel.H
    });
    document.querySelector('.reveal').appendChild(qrdiv);
}

// 'a' to start / stop poll - also stopped when timer ends
function switchPollState() {
  // hide QR code
  let overlay = document.querySelector('#poll-overlay');
  if (overlay.classList.contains('active')) overlay.classList.remove('active');
  
  switch (pollState) {
    case "not_init":
      console.error('Cannot start poll before socket is connected.')
    break;
    case "idle":
      let sl = Reveal.getCurrentSlide();
      if (sl.classList.contains('poll')) {
        poll = sl;
        startPoll();
        pollState = "started";
      } else { 
          alert('Please navigate to question before starting poll.'); 
      }
      break;
    case "started":
      if (!timer.classList.contains('timed')) {
        stopPoll();
        pollState = "idle";
      }
      break;
    default:
      console.error("Error with poll.");
      break;
  }
}

// Push question and choices to server
function startPoll() {
  timer = poll.querySelector('.countdown');
  canvas = poll.nextElementSibling.querySelector('canvas');
  startTimer();

  poll.querySelectorAll('ul.choices li').forEach(choice => {
    choices.push(choice.innerText);
  });
  
  socket.send(JSON.stringify( 
    { "tag": "Start"
    , "choices": choices} ));
}

function startTimer() {
  if (timer.classList.contains('timed')) {
    var duration = Math.floor(timer.getAttribute('data-seconds')) - 1;

    var pollTimer = setInterval(() => {
      if (duration > 0) {
        var min = Math.floor(duration / 60);
        var sec = Math.floor(duration % 60);
        min = min < 10 ? "0" + min : min;
        sec = sec < 10 ? "0" + sec : sec;
        timer.innerHTML = `${min}:${sec}`;
        if (duration < 6) {
          timer.classList.add('hurry');
        }
      } else {
          timer.classList.remove('timed');
          clearInterval(pollTimer);
          switchPollState();
          return;
      }
      duration -= 1;
    }, 1000);
  }
  timer.classList.add('active');
}

// Update chart and list of votes by response
function updateVotes(choices, canvas) {
  let votes = []; let labels = [];
  for (const key in choices) {
    labels.push(key); 
    votes.push(choices[key]); 
  }
  canvas.chart.data.labels = labels;
  canvas.chart.data.datasets[0].data = votes;  
  canvas.chart.update();
}

// Send Stop to server and clear poll values
function stopPoll() {
  timer.classList.remove('active');
  timer.classList.remove('hurry');
  if (socket == null) return;
  
  poll.querySelectorAll('ul.choices li').forEach(choice => {
      choice.classList.remove('started');
  });
  document.querySelector('#poll-overlay').classList.remove('active');

  socket.send(JSON.stringify( 
    { "tag": "Stop"
    , "question": poll.querySelector('h1').textContent} )); 

    poll = null; timer = null; choices = [];
} 

Reveal.registerPlugin( 'Poll', Poll );