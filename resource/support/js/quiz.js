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
}


function quizMI() {
    var miQuestions = document.querySelectorAll(".qmi,.quiz-mi,.quiz-match-items");
    console.log(miQuestions.length);
    for (let mi of miQuestions) {
        shuffleMatchItems(mi);
        matchings(mi);
        matchingAnswerButton(mi);
    }
}

/**
* Handles Multiple choice questions
* (Choosing/clicking and coloring of answers. Showing correct solutions etc)
*/
function quizMC() {
    var mcQuestions = document.querySelectorAll(".qmc,.quiz-mc,.quiz-multiple-choice");

    for (let mc of mcQuestions) {

        const answers = mc.getElementsByTagName("li");

        for (let answer of answers) {

            answer.addEventListener("click", function () {
                const is_right = this.classList.contains("correct");
                const tooltip = answer.querySelectorAll(".tooltip")[0];
                if (tooltip.textContent) {
                    tooltip.style.visibility = "visible";
                }

                if (is_right) {
                    this.style.backgroundColor = "#aaffaa";
                    this.style.border = "5px solid black";
                }
                else {
                    this.style.backgroundColor = "#ffaaaa";
                    this.style.border = "2px dotted black";
                }

                this.addEventListener("mouseover", function () {
                    if (tooltip.textContent) {
                        tooltip.style.visibility = "visible";
                    }
                });
                this.addEventListener("mouseout", function () {
                    tooltip.style.visibility = "hidden";
                });
            });
        }
    }

}
/**
 * @param {string} answer - The input answer
 * @param {HTMLElement} solutionList 
 * 
 * Iterate over solutionList. 
 * check if given answer is equivalent to at least one of the correct solutions
 * returns two booleans
 * correct: whether the given answer is correct
 * predef: whether the given answer is equivalent to one of the predefined possible answers
 * those predefined answers can be correct or wrong
 * This way, the tooltip will also show for expected wrong answers!
 */
function handleSolutionList(solutionList, answer) {
    const solutions = solutionList.getElementsByTagName("li");

    for (let s of solutions) {
        const is_right = s.classList.contains("correct");
        // Get only the solution text and not the tooltip div
        const solution = s.innerHTML.replace(/(<div)(.|[\r\n])*(<\/div>)/, "").toLowerCase().trim();
        if (is_right && answer == solution) {
            s.style.display = "block";
            s.classList.add("solved");
            return { correct: true, predef: true };
        } else if (!is_right && answer == solution) {
            s.style.display = "block";
            s.classList.add("solved");
            return { correct: false, predef: true };
        }
    }
    return { correct: false, predef: false };
}

/**
 * Compare the value entered in the input field to the solutions provided in the solutionList
 * If a correct solution is entered 
 * @param {HTMLElement} input  -- The input field
 * @param {HTMLElement} solutions  -- The solutionList
 */
function inputEvent(input, solutions) {
    input.addEventListener("keydown", function (event) {
        if (event.keyCode === 13) {
            event.preventDefault();

            const answer = input.value.toLowerCase().trim();
            const handled = handleSolutionList(solutions, answer);

            //Change the appearance of the input element
            this.style.backgroundColor = (handled.correct) ? "#aaffaa" : "#ffaaaa";
            this.style.border = (handled.correct) ? "5px solid black" : "2px dotted black";

            // Display the tooltip/solution box
            // Show the tooltip box for any expected answer. be it correct or wrong
            solutions.style.display = (handled.predef) ? "inline-block" : "none";
            this.addEventListener("mouseover", function () {
                if (handled.predef) {
                    solutions.style.display = "inline-block";
                }
            });
            this.addEventListener("mouseout", function () {
                solutions.style.display = "none";

            });
        }
        else {
            return false;
        }
    });
}
/**
 * Handle FreeText questions
 */
function quizFT() {
    var ftQuestions = document.querySelectorAll(".qft,.quiz-ft,.quiz-free-text");

    for (let ft of ftQuestions) {
        const solutions = ft.getElementsByClassName("solutionList")[0];
        const solutionButton = ft.getElementsByClassName("solutionButton")[0];
        const input = ft.getElementsByTagName("input")[0];
        inputEvent(input, solutions);

        solutionButton.onclick = function () {
            solutions.style.display = "inline-block";


            // Hide tooltip box after 3 seconds
            setTimeout(function () {
                solutions.style.display = "none";
                // Hide solutions only if they haven't been solved yet
                Array.from(solutions.getElementsByTagName("li")).map(x => {
                    if (!x.classList.contains("solved")) {
                        x.style.display = "none";
                    }
                })

            }, 3000)

            for (let l of solutions.getElementsByTagName("li")) {
                if (l.classList.contains("correct")) {
                    l.style.display = "block";
                }

            }


        }
    }
}

/**
 * Handles InsertChoices questions
 * 
 */
function quizIC() {
    var icQuestions = document.querySelectorAll(
        ".qic,.quiz-ic,.quiz-insert-choices"
    );

    for (let ic of icQuestions) {
        // const button = ic.getElementsByClassName("solutionButton")[0];
        const selects = ic.getElementsByTagName("select");
        // const inputs = ic.getElementsByTagName("input");

        const tipDiv = ic.querySelector(".tooltip-div");
        for (let sel of selects) {
            const solutions = sel.nextElementSibling;
            sel.addEventListener("change", function () {
                const selected = sel.options[sel.selectedIndex];
                const is_right = selected.classList.contains("correct");
                sel.style.backgroundColor = is_right ? "#aaffaa" : "#ffaaaa";
                sel.style.border = is_right ? "5px solid black" : "2px dotted black";
            });
            // Show tooltip box on mouseover
            sel.addEventListener("mouseover", function () {
                // Hide all other tooltips/solutions
                Array.from(solutions.getElementsByTagName("li")).map(
                    (x) => (x.style.display = "none")
                );
                // Display only current choice tooltip
                var choice = solutions.getElementsByTagName("li")[sel.selectedIndex - 1].querySelector(".tooltip");
                var cln = choice.cloneNode(true);
                cln.style.display = "block";
                tipDiv.appendChild(cln);
            });
            // hide on mouseout
            sel.addEventListener("mouseout", function () {
                tipDiv.innerHTML = "";
            });
        }

        // Handle correctness check and tooltip display for all input fields
        // for (let i of inputs) {
        //     const solutions = i.nextElementSibling;
        //     inputEvent(i, solutions);
        // }

        // // Show all entire solution/tooltip boxes
        // button.onclick = function () {
        //     const solutionLists = ic.getElementsByClassName("solutionList");
        //     for (let s of solutionLists) {
        //         // s.style.visibility = "visible";
        //         s.style.display = "inline-block";

        //         for (let l of s.getElementsByTagName("li")) {
        //             l.style.display = "block";
        //         }

        //         setTimeout(function () {
        //             // s.style.visibility = "hidden";
        //             s.style.display = "none";
        //             Array.from(s.getElementsByTagName("li")).map(
        //                 (x) => (x.style.display = "none")
        //             );
        //         }, 3000);
        //     }
        // };
    }
}


// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings(matchQuestion) {
    const dropzones = matchQuestion.getElementsByClassName("bucket");
    const draggables = matchQuestion.getElementsByClassName("matchItem");
    const itemCount = matchQuestion.querySelectorAll(".matchItem:not(.distractor)").length;

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
            if (child.tagName !== "a") {
                child.style.pointerEvents = "none";
            }
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

    const matchItems = matchQuestion.getElementsByClassName("matchItems")[0];
    matchItems.addEventListener("drop", drop);
    matchItems.addEventListener("dragover", allowDrop);
    const elementsArray = Array.prototype.slice.call(matchItems.getElementsByClassName('matchItem'));
    elementsArray.forEach(function (element) {
        matchItems.removeChild(element);
    })
    shuffleArray(elementsArray);
    elementsArray.forEach(function (element) {
        matchItems.appendChild(element);
    })
}



/**
 * Check correctness of current state of a matching questions on button click
 * Which elements have been dropped correctly etc?
 * @param {HTMLElement} matchQuestion 
 */
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
        const bucketsDiv = matchQuestion.getElementsByClassName("buckets")[0];
        const assignedItems = bucketsDiv.getElementsByClassName("matchItem");
        if (assignedItems.length == 0) {
            alert("You haven't assigned any items!");
            return;
        }

        for (let rem of remainingItems) {
            const matchId = rem.getAttribute("data-bucketid")
            const hasTooltip = rem.getElementsByClassName("solution").length > 0;
            if (matchId == null) {
                rem.style.backgroundColor = "#aaffaa";
                rem.style.border = "3px solid black";

                if (!hasTooltip) {
                    rem.append(solution("distractor"));
                }
            } else {
                rem.style.backgroundColor = "#ffaaaa";

                // if (!hasTooltip) {
                // rem.append(solution("Bucket " + matchId));
                // }
            }
        }

        for (let bucket of buckets) {
            const droppedItems = bucket.getElementsByClassName("matchItem");
            const bucketId = bucket.getAttribute("data-bucketid");
            for (let matchItem of droppedItems) {
                const hasTooltip = matchItem.getElementsByClassName("solution").length > 0;

                const matchId = matchItem.getAttribute("data-bucketid");
                if (matchId == null) {
                    matchItem.style.backgroundColor = "#ffaaaa";
                    if (hasTooltip) {
                        matchItem.removeChild(matchItem.getElementsByClassName("solution")[0]);
                        // matchItem.append(solution("distractor"));
                    }
                } else if (matchId == bucketId) {
                    // green
                    matchItem.style.backgroundColor = "#aaffaa";
                    matchItem.style.border = "3px solid black";
                }
                else {
                    // red
                    matchItem.style.backgroundColor = "#ffaaaa";
                    matchItem.style.border = "2px dotted black";
                    // if (!hasTooltip) {
                    // matchItem.append(solution("Bucket " + matchId));
                    // }


                }
            }
        }
    }
}

// TODO: to call from drop() (Work in Progress!)
// This is the start of an implementation that shows the solutions for matching once certain amounts of elements have been moved from the source area to the target area/buckets
function matchingSolutions(matchQuestion) {
    const numberSource = matchQuestion.querySelectorAll(".matchItem:not(.distractor)").length;
    const numberTargets = matchQuestion.querySelectorAll(".bucket:not(.distractor)").length;
    console.log("check for solution");

    const buckets = matchQuestion.getElementsByClassName("bucket");
    const remainingItems = matchQuestion.getElementsByClassName("matchItems")[0].querySelectorAll(".matchItem:not(.distractor)");
    const bucketsDiv = matchQuestion.getElementsByClassName("buckets")[0];
    const assignedItems = bucketsDiv.getElementsByClassName("matchItem");

    if (numberSource > numberTargets) {
        console.log("more items");
        if (remainingItems.length == 0) {
            console.log("all assigned");
        }
    } else if (numberSource < numberTargets) {
        console.log("more buckets");
    }
    else {
        console.log("same number");
        if (remainingItems.length == 0) {
            console.log("all assigned");
        }
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
    if (event.target.classList.contains("matchItem")) {
        return false;
    }

    event.target.appendChild(element);

    // TODO: Call to a function that checks which solutions have been assigned correctly 
    // matchingSolutions(event.target.closest(".qmi,.quiz-mi,.quiz-match-items"));
    // event.target.disabled = true;
}
