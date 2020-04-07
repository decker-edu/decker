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
    quizMI();
    quizMC();
    quizIC();
    quizFT();
    blanktextButtons();
    freetextAnswerButtons();
}


function quizMI() {
    var miQuestions = document.querySelectorAll(".qmi,.quiz-mi,.quiz-match-items");

    for (let mi of miQuestions) {
        shuffleMatchItems(mi);
        matchings(mi);
        matchingAnswerButton(mi);
    }
}

/*
Handles Multiple choice questions
(Choosing/clicking and coloring of answers. Showing correct solutions etc)
*/
function quizMC() {
    var mcQuestions = document.querySelectorAll(".qmc,.quiz-mc,.quiz-multiple-choice");

    for (let mc of mcQuestions) {

        const answers = mc.getElementsByTagName("li");
        const answerButton = mc.getElementsByClassName("solutionButton")[0];

        let defBorder = answers[0].style.border;

        for (let answer of answers) {

            answer.addEventListener("click", function () {
                if (this.style.border == defBorder) {
                    this.style.border = "thick solid black";
                }
                else {
                    this.style.border = defBorder;
                }
            });
        }

        answerButton.addEventListener("click", function () {
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
                    const is_right = answer.classList.contains("correct");
                    answer.style.backgroundColor = (is_right) ? "#97ff7a" : "#ff7a7a";

                    // tooltip display needs improvement
                    // const tooltips = answer.getElementsByClassName("tooltip");
                    // for (let tooltip of tooltips) {
                    //     tooltip.style.display = "inline-block";
                    // }
                    answer.style.pointerEvents = "none";
                }
            }

            else {
                alert("No answer chosen!");
                return false;
            }
        }
        );
    }
}




function quizFT() {
    var ftQuestions = document.querySelectorAll(".qft,.quiz-ft,.quiz-free-text");

    for (let ft of ftQuestions) {
        const solutions = ft.getElementsByClassName("solutionList")[0];
        const button = ft.getElementsByClassName("solutionButton")[0];
        const input = ft.getElementsByTagName("input")[0];

        button.onclick = function () {
            if (input.value == "") {
                alert("No answer entered!");
            } else {
                const answer = input.value;
                for (let s of solutions) {
                    // Iterate through the solutions
                    // Compare with the entered answer string
                    // When and how to show tooltips?
                }
            }

        }
    }
}

function quizIC() {
    var icQuestions = document.querySelectorAll(".qic,.quiz-ic,.quiz-insert-choices");

    for (let ic of icQuestions) {
        const button = ic.getElementsByClassName("solutionButton")[0];
        const selects = ic.getElementsByTagName("select");

        button.onclick = function () {
            for (let s of selects) {
                if (s.options[s.selectedIndex].className == "correct") {
                    s.style.backgroundColor = "rgb(151, 255, 122)";

                }
                else {
                    s.style.backgroundColor = "rgb(255, 122, 122)";
                }

                for (let o of s.options) {
                    if (o.className == "correct") {
                        o.textContent += " ✓";
                    } else {
                        o.textContent += " ✗";
                    }
                }
            }


        }
    }
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

// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings(matchQuestion) {
    const dropzones = matchQuestion.getElementsByClassName("bucket");
    const draggables = matchQuestion.getElementsByClassName("matchItem");

    for (i = 0; i < dropzones.length; i++) {
        dropzones[i].addEventListener("drop", drop);
        dropzones[i].addEventListener("dragover", allowDrop);

        for (let child of dropzones[i].children) {
            if (!child.classList.contains("matchItem")) {
                child.setAttribute("style", "pointer-events:none");
            }
        }
    }

    for (i = 0; i < draggables.length; i++) {
        draggables[i].addEventListener("dragstart", drag);

        // disable children (e.g. images) from being dragged themselves
        for (let child of draggables[i].children) {
            child.setAttribute('draggable', false);
            child.className = "draggableChild";
        }
    }
}

// Shuffle matchItems so the correct pairings aren't always directly below each other
function shuffleMatchItems(matchQuestion) {

    // Fisher-Yates Shuffle
    function shuffleArray(array) {
        for (let i = array.length - 1; i > 0; i--) {
            let j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }

    const matchItems = matchQuestion.getElementsByClassName("matchItems");
    for (let container of matchItems) {
        container.addEventListener("drop", drop);
        container.addEventListener("dragover", allowDrop);
        const elementsArray = Array.prototype.slice.call(container.getElementsByClassName('matchItem'));
        elementsArray.forEach(function (element) {
            container.removeChild(element);
        })
        shuffleArray(elementsArray);
        elementsArray.forEach(function (element) {
            container.appendChild(element);
        })
    }
}




function matchingAnswerButton(matchQuestion) {
    // A paragraph that will be shown on hover and tells whether an item is correct
    function solution(tooltip) {
        const para = document.createElement("p");
        const node = document.createTextNode("(" + tooltip + ")");
        para.appendChild(node);
        para.className = "solution";

        return (para);
    }

    const answerButton = matchQuestion.getElementsByClassName("solutionButton")[0];

    answerButton.onclick = function () {

        const buckets = matchQuestion.getElementsByClassName("bucket");
        const remainingItems = matchQuestion.getElementsByClassName("matchItems")[0].children;
        for (let bucket of buckets) {
            const matchItems = bucket.getElementsByClassName("matchItem");
            if (matchItems.length == 0) {
                alert("Please complete all pairs.");
                return;
            }
        }

        for (let rem of remainingItems) {
            const matchId = rem.getAttribute("data-bucketid")
            rem.setAttribute("draggable", false);
            if (matchId == null) {
                rem.style.backgroundColor = "yellow";
                rem.append(solution("distractor"));
            } else {
                rem.style.backgroundColor = "rgb(255, 122, 122)";
                rem.append(solution("Bucket " + matchId));
            }
        }

        for (let bucket of buckets) {
            const droppedItems = bucket.getElementsByClassName("matchItem");
            const bucketId = bucket.getAttribute("data-bucketid");
            for (let matchItem of droppedItems) {

                matchItem.setAttribute("draggable", false);
                const matchId = matchItem.getAttribute("data-bucketid");
                if (matchId == null) {
                    matchItem.style.backgroundColor = "yellow";
                    matchItem.append(solution("distractor"));
                } else if (matchId == bucketId) {
                    // green
                    matchItem.style.backgroundColor = "rgb(151, 255, 122)";
                }
                else {
                    // red
                    matchItem.style.backgroundColor = "rgb(255, 122, 122)";
                    matchItem.append(solution("Bucket " + matchId));

                }
            }
        }
        this.disabled = true;
    }


}

// Functions for dragging and dropping in the matching questions 
function allowDrop(ev) {
    ev.preventDefault();
}

var elements = [];
function drag(event) {
    var index = elements.indexOf(event.target);
    if (index == -1) {
        // not already existing in the array, add it now
        elements.push(event.target);
        index = elements.length - 1;
    }

    event.dataTransfer.setData('index', index);
}

function drop(event) {
    event.preventDefault();
    var element = elements[event.dataTransfer.getData('index')];
    if (event.target.className == "draggable") {
        return false;
    }

    event.target.appendChild(element);
    event.target.disabled = true;
}

/*
Provides the functionality for the solution button of free text questions
*/
function freetextAnswerButtons() {
    const answerButtons = document.getElementsByClassName('freetextAnswerButton');
    for (let button of answerButtons) {
        button.onclick = function () {
            clickStuff(this);
        }
    }
}

function clickStuff(button) {
    var questionField = button.parentElement.getElementsByClassName('freetextInput')[0];
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
