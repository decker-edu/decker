// I am not exactly sure why this is needed 
// but without it e.g. the math in matching questions is not reloaded
if (typeof Reveal === 'undefined') {
    console.error("quiz.js has to be loaded after reveal.js");
}
else {
    if (Reveal.isReady()) {
        quiz();
    } else {
        Reveal.addEventListener("ready", quiz);
    }
}


function quiz() {
    blanktextButtons();
    initialMatchings = initMatching();
    matchings(initialMatchings);
    multipleChoice();
    freetextAnswerButtons();
}


// For a given blanktext HTML Element returns a Map containing all wrong and correct selects and blanks
function blanktextCorrect(blanktext) {
    var selects = blanktext.getElementsByClassName("blankSelect");
    const blanks = blanktext.getElementsByClassName("blankInput");
    var wrongSelects = [];
    var correctSelects = [];
    var wrongBlanks = [];
    var correctBlanks = [];

    for (let s of selects) {
        const correct = s.options[s.selectedIndex].getAttribute("answer");
        if (correct == "true") {
            correctSelects.push(s);
        }
        else {
            wrongSelects.push(s);
        }
    }

    for (let b of blanks) {
        const correct = b.getAttribute("answer").trim();
        if (b.value.toLowerCase().trim() == correct.toLowerCase()) {
            correctBlanks.push(b);
        }
        else {
            wrongBlanks.push(b);
        }
    }
    const ret = new Map([["correctSelects", correctSelects], ["wrongSelects", wrongSelects], ["wrongBlanks", wrongBlanks], ["correctBlanks", correctBlanks]]);
    return ret;

}

function blanktextButtons() {
    var btButtons = document.getElementsByClassName("btAnswerButton");
    for (i = 0; i < btButtons.length; i++) {
        const button = btButtons[i];
        button.onclick = function () {
            blanktext = this.closest(".blankText");

            var results = blanktextCorrect(blanktext);
            var correctSelects = results.get("correctSelects");
            var wrongSelects = results.get("wrongSelects");
            var correctBlanks = results.get("correctBlanks");
            var wrongBlanks = results.get("wrongBlanks");
            console.log(wrongSelects.toString());

            for (let w of wrongSelects) {

                console.log(w.options.toString());
                w.style.backgroundColor = "rgb(255, 122, 122)";
                for (let o of w.options) {
                    if (o.getAttribute("answer") == "true") {
                        o.textContent += " ✓";
                    } else {
                        o.textContent += " ✗";
                    }
                }
            }

            for (let c of correctSelects) {
                c.style.backgroundColor = "rgb(151, 255, 122)";
                for (let o of c.options) {
                    if (o.getAttribute("answer") == "true") {
                        o.textContent += " ✓";
                    } else {
                        o.textContent += " ✗";
                    }
                }
            }

            for (let w of wrongBlanks) {
                w.style.backgroundColor = "rgb(255, 122, 122)";
                w.value += " (" + w.getAttribute("answer") + ")";
                w.setAttribute("size", w.value.length);
                w.disabled = true;
            }

            for (let c of correctBlanks) {
                c.style.backgroundColor = "rgb(151, 255, 122)";
                c.setAttribute("size", c.value.length);
                c.disabled = true;
            }
            this.disabled = true;
        }
    }
}

// Save the initial state of matching questions for retry and show solution buttons
function initMatching() {
    // Manual deep copy of the initial states of all matching questions
    const m = document.getElementsByClassName("matching");
    var initialMatchings = [];
    for (let i of m) {
        // Replace reveal.js data-src with src to avoid lazy loading
        var imgs = i.getElementsByTagName("img");
        for (let img of imgs) {
            src = img.getAttribute("data-src");
            if (src) {
                img.setAttribute("src", src);
                img.removeAttribute("data-src");
            }
        }
        var node = i.cloneNode(true);
        initialMatchings.push(node);
    }
    return initialMatchings;
}

// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings(initialMatchings) {
    var dropzones = document.getElementsByClassName("dropzone");
    var draggables = document.getElementsByClassName("draggable");

    for (i = 0; i < dropzones.length; i++) {
        dropzones[i].id = "drop".concat(i.toString());
        dropzones[i].addEventListener("drop", drop);
        dropzones[i].addEventListener("dragover", allowDrop);

        for (let child of dropzones[i].children) {
            if (!child.classList.contains("draggable")) {
                child.setAttribute("style", "pointer-events:none");
            }
        }
    }

    for (i = 0; i < draggables.length; i++) {
        draggables[i].id = "drag".concat(i.toString());
        draggables[i].addEventListener("dragstart", drag);

        // disable children (e.g. images) from being dragged themselves
        for (let child of draggables[i].children) {
            child.setAttribute('draggable', false);
            child.className = "draggableChild";
        }
    }
    // Order of execution here is important. 
    // matchingAnswerButton has to be first so the sample solution is in the correct order. Very dubious hack
    matchingAnswerButtons(initialMatchings);
    shuffleDraggables();
    retryButtons(initialMatchings);
}

// Configure retryButtons
function retryButtons(initialMatchings) {
    var buttons = document.getElementsByClassName("retryButton");

    for (i = 0; i < buttons.length; i++) {
        const initial = initialMatchings[i].cloneNode(true);

        buttons[i].onclick = function () {
            var curr = this.closest(".matching");
            curr.parentNode.replaceChild(initial, curr);
            // Call matchings once again to reset everything. e.g the shuffling etc
            matchings(initialMatchings);
        }
    }
}

// Shuffle draggables so the correct pairings aren't always directly below each other
function shuffleDraggables() {
    var dragzones = document.getElementsByClassName("dragzone");
    for (let container of dragzones) {
        var elementsArray = Array.prototype.slice.call(container.getElementsByClassName('draggable'));
        elementsArray.forEach(function (element) {
            container.removeChild(element);
        })
        shuffleArray(elementsArray);
        elementsArray.forEach(function (element) {
            container.appendChild(element);
        })
    }
}

// Fisher-Yates (aka Knuth) Shuffle (from stackoverflow)
function shuffleArray(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {
        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}

//   Provides the functionality of the "show solution" button for matching questions
function matchingAnswerButtons(initialMatchings) {
    var answerButtons = document.getElementsByClassName("matchingAnswerButton");

    for (let button of answerButtons) {
        button.onclick = function () {
            // Hack to get the index
            const j = Array.prototype.slice.call(answerButtons).indexOf(this);

            // Get the initial and current states of the dragzones
            var initialDragzone = initialMatchings[j].getElementsByClassName("dragzone")[0].cloneNode(true);
            var matchingField = this.closest(".matching");
            var currDragzone = matchingField.getElementsByClassName("dragzone")[0];

            var dropzones = matchingField.getElementsByClassName("dropzone");

            for (let drop of dropzones) {
                var draggables = drop.getElementsByClassName("draggable");

                // Alert if there's any empty dropzone (i.e. not all pairs are completed)
                if (draggables.length == 0) {
                    alert("Please complete all pairs.");
                    return;
                }
            }

            // Color the dropzones green/red depending on correct pairing
            for (let drop of dropzones) {
                var first = drop.getElementsByClassName("draggable")[0];
                if (first.id.replace("drag", "") == drop.id.replace("drop", "")) {
                    drop.style.backgroundColor = "rgb(151, 255, 122)";
                    first.setAttribute("draggable", false);
                }
                else {
                    drop.style.backgroundColor = "rgb(255, 122, 122)";
                    first.setAttribute("draggable", false);
                }
            }
            // Color the sample solutions green
            for (let drag of initialDragzone.children) {
                drag.style.backgroundColor = "rgb(151, 255, 122)";
                drag.setAttribute("draggable", false);
            }
            // replace the empty dropzone with the correct/sample solution
            //matchingField.replaceChild(initialDragzone, currDragzone);

            this.nextSibling.disabled = true;
            this.disabled = true;
        }
    }
}

// Functions for dragging and dropping in the matching questions 
function allowDrop(ev) {
    ev.preventDefault();
}

function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.id);
}

function drop(ev) {
    ev.preventDefault();
    var data = ev.dataTransfer.getData("text");
    if (ev.target.className == "draggable") {
        return false;
    }
    ev.target.appendChild(document.getElementById(data));
    ev.target.disabled = true;
}

/*
Handles Multiple choice questions
(Choosing/clicking and coloring of answers. Showing correct solutions etc)
*/
function multipleChoice() {
    const surveys = document.getElementsByClassName("survey");
    let survey_num = 0;
    for (let survey of surveys) {
        survey.setAttribute("data-survey-num", survey_num);
        const local_survey_num = survey_num;
        survey_num += 1;
        var answerButton = survey.getElementsByClassName("mcAnswerButton")[0];
        const answers = survey.getElementsByTagName("li");
        let defBorder = answers[0].style.border;

        let answer_num = 0;
        // highlight chosen answer(s)
        for (let answer of answers) {
            const local_answer_num = answer_num;

            answer.addEventListener("click", function () {
                if (this.style.border == defBorder) {
                    this.style.border = "thick solid black";
                }
                else {
                    this.style.border = defBorder;
                }
            });
            answer_num += 1;
        }

        // Show correct solutions, lock all interaction with answers
        // Popup if no box was selected
        answerButton.onclick = function () {
            let answered = false;
            for (let answer of answers) {
                if (answer.style.border == defBorder) {
                    continue;
                }
                else {
                    answered = true;
                }
            }

            if (answered) {
                this.disabled = true;
                for (let answer of answers) {
                    var answer_div = answer.getElementsByClassName("answer")[0];
                    const is_right = answer_div.classList.contains("right");
                    answer.style.backgroundColor = (is_right) ? "#97ff7a" : "#ff7a7a";
                    const tooltips = answer.getElementsByClassName("tooltip");
                    for (let tooltip of tooltips) {
                        tooltip.style.display = "inline";
                    }
                    answer.style.pointerEvents = "none";
                }
            }
            else {
                alert("No answer chosen!");
                return false;
            }
        };
    }

}

/*
Provides the functionality for the solution button of free text questions
*/
function freetextAnswerButtons() {
    const answerButtons = document.getElementsByClassName('freetextAnswerButton');
    for (let button of answerButtons) {
        button.onclick = function () {
            var questionField = this.parentElement.getElementsByClassName('freetextInput')[0];
            // Has the user entered anything?
            if (questionField.value) {
                var answer = questionField.getAttribute("answer").trim();
                if (questionField.value.toLowerCase().trim() == answer.toLowerCase()) {
                    questionField.style.backgroundColor = "rgb(151, 255, 122)";
                }
                else {
                    questionField.style.backgroundColor = "rgb(255, 122, 122)";
                    questionField.value += " (" + answer + ")";
                }
                questionField.setAttribute("size", questionField.value.length);
                questionField.disabled = true;
                this.disabled = true;
            }
            else {
                alert("No answer entered!");
                return false;
            }
        }
    }
}
