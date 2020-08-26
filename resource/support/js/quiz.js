"use strict";

var RevealQuiz = (() => {
    return {
        init: function() { 
            return new Promise( function(resolve) {
                quizMI();
                quizMC();
                quizIC();
                quizFT();
                resolve();
            });
        }
    }
})();

/**
 * Multiple choice questions
 * Listen for selections
 */
function quizMC() {
    var mcQuestions = document.querySelectorAll(".qmc,.quiz-mc,.quiz-multiple-choice");

    for (var question of mcQuestions) {
        var answers = question.getElementsByTagName("li");

        for (var answer of answers) {
            answer.addEventListener("click", function () {
                var correct = this.classList.contains("correct");
                // toggle answer on click
                this.classList.forEach(c => {
                    c.match(/show-/g) ? this.classList.remove(c) : this.classList.add(correct ? "show-right" : "show-wrong");
                });
            });
        }

        hideSolutionButton(question);
        // TODO: add function to show solution and reset
    }
}

/**
 * Free Text questions
 * Listen for <enter> key in input field - show if correct / incorrect
 * Listen for click of solution button - show correct response
 */
function quizFT() {
    const ftQuestions = document.querySelectorAll(".qft,.quiz-ft,.quiz-free-text");
    
    for (let question of ftQuestions) {
        const solutions = question.querySelector(".solutionList");
        const choices = solutions.getElementsByTagName('li');
        const solutionDiv = question.querySelector('.solutionDiv');
        const input = question.querySelector("input");
        var buffer = [];

        // Add correct solution to solution div
        for (let c of choices) {
            if (c.classList.contains('correct')) {
                solutionDiv.appendChild(c.cloneNode(true)); 
            }
        }

        // Add click listeners to solution, reset buttons
        const solutionButton = question.querySelector('.solutionButton');
        const resetButton = question.querySelector('.resetButton');
        solutionButton.addEventListener('click', showSolution);
        resetButton.addEventListener('click', resetQuestion);    

        // TODO: discuss with group functionality/use
        // for now, default to show solution button 
        // hideSolutionButton(question);
        question.querySelector('.resetButton').classList.add('hidden');

        // Listen for enter, delete, backspace in input field
        input.addEventListener("keydown", (e) => {
            buffer.push(e.key.toLowerCase());
            if (buffer[buffer.length-1] === buffer[buffer.length-2]) { return; };
            if (e.keyCode === 13) { checkInput() }
            if (e.keyCode === 8 || e.keyCode === 46) { resetQuestion() }
        });

        // Check value of input field against solutions
        function checkInput() {
            event.preventDefault();
            const answer = input.value.toLowerCase().trim();
            const checked = checkAnswer(solutions, answer);
            input.classList.add(checked.correct ? "show-right" : "show-wrong");

            // Display the tooltip/solution box for any expected answer, correct or incorrect
            input.addEventListener("mouseover", () => {
                if (checked.predef) { solutions.classList.add("solved") }
            });
            input.addEventListener("mouseout", () => {
                solutions.classList.remove("solved");
            });
        }
        // Show the solution div 
        function showSolution() {
            solutionDiv.classList.add('solved');
            this.classList.add('disabled');
            resetButton.classList.remove('hidden');
        }
        // Return to original state
        function resetQuestion() {
            for (let c of choices) { c.classList.remove('solved'); }
            solutionDiv.classList.remove('solved');
            input.classList.remove("show-right", "show-wrong");
            input.value = "";
            solutionButton.classList.remove('disabled');
            resetButton.classList.add('hidden');
        }
    }
}

/**
 * Select Choice question
 * Listen for selections - show if correct / incorrect
 */
function quizIC() {
    var icQuestions = document.querySelectorAll(".qic,.quiz-ic,.quiz-insert-choices");

    for (let question of icQuestions) {
        const selects = question.getElementsByTagName("select");
        const tipDiv = question.querySelector(".tooltip-div");

        for (let sel of selects) {
            const solutionList = sel.nextElementSibling;

            // Listen for selections - color appropriately
            sel.addEventListener('change', function() {
                tipDiv.innerHTML = "";
                sel.classList.add('solved');
                let ind = sel.selectedIndex;
                let answer = sel.options[ind].innerText;
                let checked = checkAnswer(solutionList, answer);

                sel.classList.remove("show-right","show-wrong");
                sel.classList.add(checked.correct ? "show-right" : "show-wrong");

                var answers = solutionList.getElementsByTagName('li');
                var tip = answers.item(sel.selectedIndex - 1).querySelector('.tooltip');
                var cln = tip.cloneNode(true);
                tipDiv.appendChild(cln);
            })

            // Show tooltip box on mouseover
            sel.addEventListener("mouseover", function() {
                if (sel.classList.contains('solved')) {
                    tipDiv.classList.add('solved');
                }
            });
            sel.addEventListener("mouseleave", () => {
                tipDiv.classList.remove('solved');
            });
        }

        hideSolutionButton(question);
        // TODO: Add function to show solution, reset
    }
}

/**
 * Matching Item questions
 * Build drag and drop or if 'plain' build select drop-down
 */
function quizMI() {
    var miQuestions = document.querySelectorAll(".qmi,.quiz-mi,.quiz-match-items");
    for (var question of miQuestions) {
        shuffleMatchItems(question);
        buildDragDrop(question);
    }
}

/********************
 * Helper Functions
 ********************/

/**
 * Show solution button if specified in yaml
 * @param {Element} question 
 */
function hideSolutionButton(question) {
    question.querySelector('.solutionButton').classList.add(question.classList.contains('show-solution') ? 'display' : 'hidden');
    question.querySelector('.resetButton').classList.add('hidden');
}

/**
 * @param {string} answer - The input answer
 * @param {Element} solutionList 
 * 
 * Iterate over solutionList, check if answer is equivalent to at least one correct solution.
 * Returns two booleans - 
 *   correct: whether the given answer is correct
 *   predef: whether the given answer is equivalent to one of the predefined possible answers
 * Predefined answers can be correct or wrong.
 * Tooltip will also show for expected wrong answers!
 */
function checkAnswer(solutionList, answer) {
    const solutions = solutionList.getElementsByTagName("li");

    for (var s of solutions) {
        const is_right = s.classList.contains("correct");
        // Get only the solution text and not the tooltip div
        const solution = s.innerHTML.replace(/(<div)(.|[\r\n])*(<\/div>)/, "").toLowerCase().trim();
        if (answer == solution) {
            s.classList.add("solved");
            return {correct: (is_right ? true : false), predef: true};
        } 
    }
    return { correct: false, predef: false };
}

/**
 * Shuffle matchItems so the correct pairings aren't always directly below each other
 * @param {Element} question 
 */
function shuffleMatchItems(question) {

    // Fisher-Yates Shuffle
    const shuffleArray = array => {
        for (var i = array.length - 1; i > 0; i--) {
            var j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }

    const matchItems = question.querySelector(".matchItems");
    const elementsArray = Array.prototype.slice.call(matchItems.getElementsByClassName('matchItem'));

    elementsArray.map(element => { matchItems.removeChild(element) })
    shuffleArray(elementsArray);
    elementsArray.map(element => { matchItems.appendChild(element) });
}

/**
 * Construct drag and drop listeners for Matching questions
 * @param {Element} question 
 */
function buildDragDrop(question) {
    const dropzones = question.getElementsByClassName("bucket");
    const draggables = question.getElementsByClassName("matchItem");

    const matchItems = question.querySelector(".matchItems");
    matchItems.addEventListener("drop", drop);
    matchItems.addEventListener("dragover", e => e.preventDefault());

    for (var i = 0; i < dropzones.length; i++) {
        dropzones[i].addEventListener("drop", drop);
        dropzones[i].addEventListener("dragover", e => e.preventDefault());

        for (var child of dropzones[i].children) {
            if (!child.classList.contains("matchItem")) {
                child.classList.add("draggableChild");
            }
        }
    }

    for (var i = 0; i < draggables.length; i++) {
        draggables[i].addEventListener("dragstart", drag);

        // disable children (e.g. images) from being dragged themselves
        for (var child of draggables[i].children) {
            child.setAttribute('draggable', false);
            child.classList.add("draggableChild");
        }
    }
    matchingAnswerButton(question);
}

/**
 * Correct matching questions on button click
 * @param {Element} question 
 */
function matchingAnswerButton(question) {
    const answerButton = question.querySelector(".solutionButton");

    answerButton.addEventListener('click', () => {
        const buckets = question.getElementsByClassName("bucket");
        const remainingItems = question.querySelector(".matchItems").children;
        const bucketsDiv = question.querySelector(".buckets");
        const assignedItems = bucketsDiv.getElementsByClassName("matchItem");

        if (assignedItems.length == 0) {
            alert("You haven't assigned any items!");
            return;
        }

        for (var rem of remainingItems) {
            const matchId = rem.getAttribute("data-bucketid")
            rem.classList.remove("show-right","show-wrong");
            rem.classList.add(matchId == null ? "show-right" : "show-wrong");
        }

        for (var bucket of buckets) {
            const droppedItems = bucket.getElementsByClassName("matchItem");
            const bucketId = bucket.getAttribute("data-bucketid");
            for (var matchItem of droppedItems) {
                matchItem.classList.remove("show-right","show-wrong");

                const matchId = matchItem.getAttribute("data-bucketid");
                matchItem.classList.add(matchId == bucketId ? "show-right" : "show-wrong");
                // if (matchId == null) {
                //     matchItem.classList.add("show-wrong");
                // } else if (matchId == bucketId) {
                //     matchItem.classList.add("show-right");
                // }
                // else {
                //     matchItem.classList.add("show-wrong");
                // }
            }
        }
    })
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
    if (event.target.classList.contains("matchItem")) {
        event.target.parentNode.appendChild(element);
        return;
    }

    event.target.appendChild(element);
}


Reveal.registerPlugin( 'quiz', RevealQuiz );

