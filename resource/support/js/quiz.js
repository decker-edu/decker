"use strict";

var RevealQuiz = (() => {
    return {
        init: () => { 
            return new Promise(resolve => {
                quizMI();
                quizMC();
                quizIC();
                quizFT();
                resolve();
            });
        }
    }
})();

function quizMC() {
    for (let question of document.querySelectorAll(".qmc,.quiz-mc,.quiz-multiple-choice")) {
        for (let answer of question.getElementsByTagName("li")) {
            // remove tooltip if empty to avoid grey dot
            const tip = answer.querySelector('.tooltip');
            if (tip.childElementCount === 0) { tip.remove(); }
            answer.addEventListener("click", function () {
                const correct = this.classList.contains("correct");
                // toggle answer on click
                this.classList.forEach(c => {
                    c.match(/show-/g) ? this.classList.remove(c) : this.classList.add(correct ? "show-right" : "show-wrong");
                });
            });
        }
    }
}

function quizFT() {
    for (let question of document.querySelectorAll(".qft,.quiz-ft,.quiz-free-text")) {
        const solutions = question.querySelector(".solutionList");
        const input = question.querySelector("input");

        // Listen for enter, delete, backspace in input field
        var buffer = [];
        input.addEventListener("keydown", e => {
            buffer.push(e.key.toLowerCase());
            if (buffer[buffer.length-1] === buffer[buffer.length-2]) { return; };
            if (e.keyCode === 13) { checkInput() }
            if (e.keyCode === 8 || e.keyCode === 46) { resetQuestion() }
        });

        // Check value of input field against solutions
        function checkInput() {
            const checked = checkAnswer(solutions, input.value.toLowerCase().trim());
            input.classList.remove("show-right", "show-wrong");
            input.classList.add(checked.correct ? "show-right" : "show-wrong");

            // Display the tooltip/solution box for any expected answer, correct or incorrect
            input.addEventListener("mouseover", () => {
                if (checked.predef) { solutions.classList.add("solved"); }
            });
            input.addEventListener("mouseout", () => {
                solutions.classList.remove("solved");
            });
        }

        // Add click listeners to solution, reset buttons
        const plain = question.classList.contains('plain') ? true : false;
        const solutionButton = question.querySelector('.solutionButton');
        const resetButton = question.querySelector('.resetButton');
        solutionButton.addEventListener('click', showSolution);
        resetButton.addEventListener('click', resetQuestion);    
        question.querySelector('.resetButton').classList.add(plain ? 'disabled' : 'hidden'); 
    
        const choices = solutions.getElementsByTagName('li');
        const solutionDiv = question.querySelector('.solutionDiv');

        // Populate solutionDiv to reserve space - hide if fancy style
        for (let c of choices) {
            if (c.classList.contains('correct')) { solutionDiv.appendChild(c.cloneNode(true)); }
        }
        if (!plain) { solutionDiv.classList.add('hidden'); }

        // Handle click of solution button
        function showSolution() {
            if (plain) {
                solutionDiv.classList.add('solved');    
                this.classList.add('disabled');
                resetButton.classList.remove('disabled');
            } else {
                solutions.classList.add('solved');
                for (let c of choices) {
                    if (c.classList.contains('correct')) { c.classList.add("solved"); }
                }
                // Hide tooltip box after 3 seconds
                setTimeout(() => {
                    solutions.classList.remove("solved");
                    Array.from(solutions.getElementsByTagName("li")).map(x => {
                        x.classList.remove("solved");
                    });
                }, 3000);
            }
        }

        // Return to original state
        function resetQuestion() {
            for (let c of choices) { c.classList.remove('solved'); }
            solutionDiv.classList.remove('solved');
            input.classList.remove("show-right", "show-wrong");
            input.value = "";
            solutionButton.classList.remove('disabled');
            resetButton.classList.add('disabled');
        }
    }
}

function quizIC() {
    const icQuestions = document.querySelectorAll(".qic,.quiz-ic,.quiz-insert-choices");

    for (let question of icQuestions) {
        const selects = question.getElementsByTagName("select");
        const tipDiv = question.querySelector(".tooltip-div");

        for (let sel of selects) {
            const solutionList = sel.nextElementSibling;

            // Listen for selections - color appropriately
            sel.addEventListener('change', () => {
                tipDiv.innerHTML = "";
                sel.classList.add('solved');
                const ind = sel.selectedIndex;
                const answer = sel.options[ind].innerText;
                const checked = checkAnswer(solutionList, answer);

                sel.classList.remove("show-right","show-wrong");
                sel.classList.add(checked.correct ? "show-right" : "show-wrong");

                const answers = solutionList.getElementsByTagName('li');
                const tip = answers.item(sel.selectedIndex - 1).querySelector('.tooltip');
                const cln = tip.cloneNode(true);
                tipDiv.appendChild(cln);
            })

            // Show tooltip box on mouseover
            sel.addEventListener("mouseover", () => {
                if (sel.classList.contains('solved')) { 
                    if (tipDiv.firstElementChild.innerHTML !== "") { tipDiv.classList.add('solved'); } } });
            sel.addEventListener("mouseleave", () => { tipDiv.classList.remove('solved') });
        }
    }
}

function quizMI() {
    const miQuestions = document.querySelectorAll(".qmi,.quiz-mi,.quiz-match-items");
    for (let question of miQuestions) {
        shuffleMatchItems(question);
        question.classList.contains('plain') ? buildPlainMatch(question) : buildDragDrop(question);    
    }
}

/********************
 * Helper Functions
 ********************/

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

    for (let s of solutions) {
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

        for (let child of dropzones[i].children) {
            if (!child.classList.contains("matchItem")) {
                child.classList.add("draggableChild");
            }
        }
    }

    for (var i = 0; i < draggables.length; i++) {
        draggables[i].addEventListener("dragstart", drag);

        // disable children (e.g. images) from being dragged themselves
        for (let child of draggables[i].children) {
            child.setAttribute('draggable', 'false');
            child.classList.add("draggableChild");
        }
    }
    const answerButton = question.querySelector(".solutionButton");
    matchingAnswerButton(question, answerButton);
}

/**
 * Correct matching questions on button click
 * @param {Element} question 
 */
function matchingAnswerButton(question, button) {
    button.addEventListener('click', () => {
        const buckets = question.getElementsByClassName("bucket");
        const remainingItems = question.querySelector(".matchItems").children;
        const bucketsDiv = question.querySelector(".buckets");
        const assignedItems = bucketsDiv.getElementsByClassName("matchItem");

        if (assignedItems.length == 0) {
            alert("You haven't assigned any items!");
            return;
        }

        // color remaining items that have not been dragged
        for (let rem of remainingItems) {
            const matchId = rem.getAttribute("data-bucketid");
            rem.classList.remove("show-right","show-wrong");
            rem.classList.add(matchId == null ? "show-right" : "show-wrong");
        }

        // color remaining items that have been dragged
        for (let bucket of buckets) {
            const droppedItems = bucket.getElementsByClassName("matchItem");
            const bucketId = bucket.getAttribute("data-bucketid");
            for (let matchItem of droppedItems) {
                matchItem.classList.remove("show-right","show-wrong");

                const matchId = matchItem.getAttribute("data-bucketid");
                matchItem.classList.add(matchId == bucketId ? "show-right" : "show-wrong");
            }
        }
    })
}

/**
 * Construct select tags with options for Matching questions
 * @param {Element} question 
 */
function buildPlainMatch(question) {
    const matchItems = question.querySelector('.matchItems');
    const buckets = question.querySelector('.buckets');
    const selectTag = buildSelect(matchItems, buckets);

    // const matchDiv = document.createElement('div');
    // matchDiv.classList.add('matchDiv');
    // matchItems.parentNode.insertBefore(matchDiv, matchItems);        
    // [matchItems, buckets].forEach(ele => { matchDiv.appendChild(ele); });

    for (let bucket of buckets.querySelectorAll('.bucket')) {
        buckets.removeChild(bucket);

        const matchQuestion = document.createElement('div');
        matchQuestion.classList.add("matchQuestion");
        matchItems.appendChild(matchQuestion);

        const lab = document.createElement('label');
        lab.setAttribute('data-value', bucket.classList.contains('distractor') ? '0' : bucket.getAttribute('data-bucketId'));
        lab.innerHTML = bucket.innerHTML;
        [lab,selectTag.cloneNode(true)].forEach(ele => { matchQuestion.appendChild(ele); });
    }

    question.querySelector('.solutionButton').addEventListener('click', () => {
        const qns = matchItems.querySelectorAll('.matchQuestion');
        for (let q of qns) {
            const sel = q.querySelector('select');
            sel.classList.remove("show-right","show-wrong");
            const idCorrect = sel.previousElementSibling.getAttribute('data-value');
            // color individual options based on correctness
            for (let opt of sel.querySelectorAll('option')) {
                opt.classList.add(opt.value == idCorrect ? 'show-right' : 'show-wrong');
            }
            const idSelected = sel.options[sel.selectedIndex].value;
            sel.classList.add(idCorrect == idSelected ? "show-right" : "show-wrong");
        }   
    });
    // No solutionDiv or tooltips because none defined in MD
}

/**
 * Build and append option tags from tags with class 'matchItem'
 * @param {Element} matchItems 
 * @param {Element} buckets 
 */
function buildSelect(matchItems, buckets) {
    const answers = matchItems.querySelectorAll('.matchItem');
    const sel = document.createElement('select');
    
    const blankOpt = document.createElement('option');
    blankOpt.innerText = '...';
    blankOpt.value = '0';
    sel.appendChild(blankOpt);
    for (let i = 0; i < answers.length; i++) {
        const opt = document.createElement('option');
        const char = String.fromCharCode(i + 65) + ".";
        opt.innerHTML = char; 
        opt.value = answers[i].getAttribute('data-bucketId') || '0'; 
        sel.appendChild(opt);
        buckets.appendChild(answers[i]); 
    }
    return sel;
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
    const element = elements[event.dataTransfer.getData('index')];
    if (event.target.classList.contains("matchItem")) {
        event.target.parentNode.appendChild(element);
        return;
    }

    event.target.appendChild(element);
}


Reveal.registerPlugin( 'quiz', RevealQuiz );