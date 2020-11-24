"use strict";

var RevealQuiz = (function(){

    // state of ballot server: "not_init", "open", "closed"
    var serverState;

    // state of ballot
    var ballotState;

    // timer for updating #votes
    var timerVotes, timerStatus;

    // get config
    var config = Reveal.getConfig().quiz || {};

    // get quiz server
    var server = config.server || "http://graphics.uni-bielefeld.de:8080";

    // the chart object
    var myChart;


    // get path of script -> used for loading audio files
    var path = scriptPath();
    function scriptPath() {
        // obtain plugin path from the script element
        // !!! script has to be named "quiz.js" !!!
        var path;
        if (document.currentScript) {
            path = document.currentScript.src.slice(0, -7);
        } else {
            var sel = document.querySelector('script[src$="/quiz.js"]')
            if (sel) {
                path = sel.src.slice(0, -7);
            }
        }
        return path;
    }


    // generate DIV for chart
    var chart_div = document.createElement( 'div' );
    chart_div.classList.add( 'overlay' );
    chart_div.classList.add( 'visible' );
    chart_div.setAttribute( 'data-prevent-swipe', '' );
    chart_div.style.visibility      = 'hidden';
    chart_div.style.zIndex          = "34";
    chart_div.style.position        = "absolute";
    chart_div.style.left            = "auto";
    chart_div.style.top             = "auto";
    chart_div.style.right           = "10px";
    chart_div.style.bottom          = "10px";
    chart_div.style.width           = "420px";
    chart_div.style.height          = "320px";
    chart_div.style.margin          = "auto";
    chart_div.style.padding         = "5px";
    chart_div.style.textAlign       = "center"
    chart_div.style.border          = "3px solid #2a9ddf";
    chart_div.style.borderRadius    = "10px";
    chart_div.style.boxShadow       = "3px 5px 5px grey";
    chart_div.style.backgroundColor = 'rgba(255,255,255,1.0)';
    chart_div.style.transition      = 'none';
    chart_div.style.transformOrigin = 'bottom right';
    document.querySelector(".reveal").appendChild( chart_div );


    // generate canvas for the actual chart
    var chart = document.createElement( 'canvas' );
    chart.width        = "400";
    chart.height       = "300";
    chart.style.top    = "0px";
    chart.style.left   = "0px";
    chart.style.width  = "400px";
    chart.style.height = "300px";
    chart_div.appendChild( chart );


    // generate label for #votes
    var votes_div = document.createElement( 'div' );
    votes_div.classList.add( 'overlay' );
    votes_div.classList.add( 'visible' );
    votes_div.setAttribute( 'data-prevent-swipe', '' );
    votes_div.style.visibility      = 'hidden';
    votes_div.style.zIndex          = "33";
    votes_div.style.position        = "absolute";
    votes_div.style.left            = "auto";
    votes_div.style.top             = "auto";
    votes_div.style.right           = "10px";
    votes_div.style.bottom          = "10px";
    votes_div.style.width           = "auto";
    votes_div.style.height          = "auto";
    votes_div.style.margin          = "auto";
    votes_div.style.padding         = "5px";
    votes_div.style.color           = "black";
    votes_div.style.fontSize        = "20px";
    votes_div.style.textAlign       = "center";
    votes_div.style.border          = "3px solid #2a9ddf";
    votes_div.style.borderRadius    = "10px";
    votes_div.style.boxShadow       = "3px 5px 5px grey";
    votes_div.style.backgroundColor = 'rgba(255,255,255,1.0)';
    votes_div.style.cursor          = "help";
    votes_div.style.transformOrigin = 'bottom right';
    document.querySelector(".reveal").appendChild( votes_div );



    // generate QR code for DIVs with class/id "quiz-qr"
    var head     = document.querySelector( 'head' );
    var qrscript = document.createElement("script");
    qrscript.type   = "text/javascript";
    qrscript.src    = path + "qrcode.min.js";
    qrscript.onload = function() {
        var e = document.getElementById("quiz-qr");
        if (e) e.classList.add("quiz-qr");
        Array.from(document.getElementsByClassName("quiz-qr")).forEach(function(e){
            var size = parseInt(e.style.width, 10) || 300;
            var qrcode = new QRCode(e, {
                text:         server,
                width:        size,
                height:       size,
                colorDark:    "#000000",
                colorLight:   "#ffffff",
                correctLevel: QRCode.CorrectLevel.H
            });
            qrcode.makeCode(server);
        });
    }
    head.appendChild( qrscript );


    // write server URL in elements with class/id 'quiz-url'
    var e = document.getElementById("quiz-url");
    if (e) e.classList.add("quiz-url");
    Array.from(document.getElementsByClassName("quiz-url")).forEach(function(e){
        e.innerHTML = server;
    });



    // load WWM jingles
    var jingleQuestion = new Audio(path+'/wwm-question.mp3');
    var jingleAnswer   = new Audio(path+'/wwm-answer.mp3');


    // how many answers on current slide?
    var numAnswers = 0;


    // what to do on slide change
    function slideChanged()
    {
        // if not presenter displayw slide preview
        if (!Reveal.isSpeakerNotes())
        {
            // stop sounds
            jingleQuestion.pause();
            jingleAnswer.pause();

            // if quiz is active -> close it
            // (but only then, since it requires authentication)
            if (serverState == "open")
            {
                closeBallot();
            }

            // hide stuff
            hideVotes();
            hideChart();

            // remove timers
            clearInterval(timerVotes);
            clearInterval(timerStatus);

            // reset state
            ballotState = "not_init";
            numAnswers  = 0;

            // is this a quiz slide? -> find answers (new version)
            var answers = Reveal.getCurrentSlide().querySelectorAll('.reveal .quiz ul>li>input[type="checkbox"]');
            if (answers.length)
            {
                numAnswers = answers.length;

                // hide answers' right/wrong classification
                for (var i = 0; i < answers.length; i++)
                {
                    var li = answers[i].parentElement;
                    li.classList.remove('show-right');
                    li.classList.remove('show-wrong');
                    li.addEventListener('click', function() {
                        var correct = this.querySelector('input:checked');
                        this.classList.add( correct ? "show-right" : "show-wrong" );
                    }, false);
                }
            }
        }
    }



    // ballot stuff (needs authentication) -----------------------------------

    // start new ballot
    function startBallot()
    {
        var xhr = new XMLHttpRequest();
        xhr.open('post', server + '/init/' + numAnswers, false);
        xhr.withCredentials = true;
        xhr.send(null);
        jingleQuestion.currentTime = 0;
        jingleQuestion.play();
    }

    // close ballot
    function closeBallot()
    {
        var xhr = new XMLHttpRequest();
        xhr.open('post', server + '/close', true);
        xhr.withCredentials = true;
        xhr.send(null);
    }



    // #votes stuff ----------------------------------------------------------

    // show votes div
    function showVotes()
    {
        votes_div.style.visibility = 'visible';
        timerVotes  = setInterval(getVotes, 1000);
        timerStatus = setInterval(getStatus, 1000);
    }

    // hide votes div
    function hideVotes()
    {
        votes_div.style.visibility = 'hidden';
        clearInterval(timerVotes);
        clearInterval(timerStatus);
    }

    // get number of votes from server
    function getVotes()
    {
        var xhr = new XMLHttpRequest();
        xhr.open('get', server + '/count', true);
        xhr.setRequestHeader('Content-Type', 'text/plain');
        xhr.addEventListener('load', function() { drawVotes(JSON.parse(xhr.response)); });
        xhr.send();
    }

    // draw number of votes
    function drawVotes(nVotes)
    {
        votes_div.innerHTML = nVotes;
    }


    // get status from server
    function getStatus()
    {
        var xhr = new XMLHttpRequest();
        xhr.open('get', server + '/status', true);
        xhr.setRequestHeader('Content-Type', 'text/plain');
        xhr.addEventListener('load', function() { drawStatus(JSON.parse(xhr.response)); });
        xhr.send();
    }

    // draw number of votes
    function drawStatus(s)
    {
        serverState = s;
        if (s == "open")
        {
            votes_div.style.border = "3px solid green";
        }
        else
        {
            votes_div.style.border = "3px solid red";
        }
    }



    // chart stuff -----------------------------------------------------------

    // hide chart
    function hideChart()
    {
        chart_div.style.visibility = 'hidden';
    }


    // show/hide chart of answers
    function showChart()
    {
        // hide #votes
        votes_div.style.visibility = 'hidden';

        // setup new result request and add trigger drawChart
        var xhr = new XMLHttpRequest();
        xhr.open('get', server + '/result', true);
        xhr.setRequestHeader('Content-Type', 'text/plain');
        xhr.addEventListener('load', function() { drawChart(JSON.parse(xhr.response)); });
        xhr.send();
    }


    // draw chart; result is JSON string of results
    function drawChart(result)
    {
        // play sound
        jingleAnswer.currentTime = 0;
        jingleAnswer.play();

        // resize and show
        chart_div.style.visibility = 'visible';

        // DEBUG 
        //result = [10, 26, 5, 7];

        // how many answers?
        var n = 0;
        for (var i = 0; i < result.length; i++)
        {
            n += result[i];
        }

        // convert answers to percentages
        for (var i = 0; i < result.length; i++)
        {
            result[i] /= n;
        }

        // destroy chart if it was created before (strictly required!)
        if (myChart) 
        {
            myChart.destroy();
        }


        // get labels as subset of this array
        var labels = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'].slice(0, result.length);

        // (re)create chart
        var ctx = chart.getContext('2d');
        myChart = new Chart(ctx, {
            type: 'bar',
            data: {
                labels: labels,
                datasets: [{
                    data: result,
                    backgroundColor: '#2a9ddf'
                }]
            },
            options: {
                animation: { duration: 3000 },
                legend:    { display: false },
                title:     { display: false },
                tooltips:  { enabled: false },
                scales: {
                    yAxes: [{
                        gridLines: { display: false },
                        ticks: { beginAtZero: true }
                    }]
                }
            }
        });
    }


    // ballot states
    function switchBallotState()
    {
        //console.log("old ballot state: " + ballotState);

        switch (ballotState)
        {
            case "not_init":
                if (numAnswers)
                {
                    startBallot();
                    showVotes();
                    ballotState = "open";
                }
                else
                {
                    // just to trigger authentication
                    closeBallot();
                }
                break;

            case "open":
                if (serverState != "open")
                {
                    startBallot();
                }
                else
                {
                    closeBallot();
                    hideVotes();
                    showChart();
                    ballotState = "chart";
                }
                break;

            case "chart":
                hideChart();
                ballotState = "done";
                break;

            case "done":
                showChart();
                ballotState = "chart";
                break;

            default:
                console.error("this should not happen");
                hideChart();
                hideVotes();
                ballotState = "not_init";
                break;
        }
    }


    function resize()
    {
        var s = Reveal.getScale();
        chart_div.style.transform  = 'scale('+ s +')';
        votes_div.style.transform  = 'scale('+ s +')';
    }


    return {
        init: function() { 
            return new Promise( function(resolve) {
                
                // setup keyboard shortcut
                Reveal.removeKeyBinding( 81 );
                Reveal.addKeyBinding( { keyCode: 81, key: 'Q', description: 'Toggle Quiz' }, switchBallotState );

                // add event listener
                Reveal.addEventListener( 'slidechanged', slideChanged );
                Reveal.addEventListener( 'resize', resize );

                resolve();
            });
        }
    }

})();

Reveal.registerPlugin( 'quiz', RevealQuiz );

