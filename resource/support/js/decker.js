if (typeof Reveal === 'undefined')
{
    console.error("decker.js has to be loaded after reveal.js");
}
else
{
    if (Reveal.isReady()) {
        deckerStart();
    } else {
        Reveal.addEventListener("ready", deckerStart);
    }
}


// Fix some decker-specific things after Reveal
// has been initialized
function deckerStart() {
  fixAutoplayWithStart();
  // makeVertical();
  quizModule.quiz();
  currentDate();
  addSourceCodeLabels();
}


function fixAutoplayWithStart() {
  for (let vid of document.getElementsByTagName("video")) {
    vid.addEventListener('play', (e) => {
      const timeRegex = /#t=(\d+)/;
      const matches = e.target.currentSrc.match(timeRegex);
      if (matches !== null && matches.length > 0) {
        e.target.currentTime = matches[1];
      }
    });
  }
}

// Replace date string on title slide with current date 
// if string provided for date in yaml header is "today"
function currentDate() {
  var date = document.getElementsByClassName("date")[0];
  if (!date) return;
  var dateString = date.textContent;

  var today = new Date().toISOString().substr(0, 10);

  if (dateString === "today") {
    date.textContent = today;
  }
}

// Allows printPdf() function to be called as onclick event directly from HTML elements
window.printPdf = function () {
  url = window.location.href;
  url = url.replace(".html", ".html?print-pdf");
  var printWindow = window.open(url);

  printWindow.onload = function () {
    printWindow.print();
    printWindow.onfocus = function () { printWindow.close(); }
  };
}

function makeVertical() {
  const subsections = document.getElementsByClassName("sub");
  const subsection_bundles = [];
  for (let i = 0; i < subsections.length; i++) {
    const subsection = subsections[i];
    if (subsection.nodeName !== "SECTION") {
      continue;
    }
    const bundle = [subsection];
    while (
      i + 1 < subsections.length &&
      subsection.nextElementSibling === subsections[i + 1]
    ) {
      i += 1;
      bundle.push(subsections[i]);
    }
    subsection_bundles.push(bundle);
  }

  for (let bundle of subsection_bundles) {
    const supersection = document.createElement("section");
    supersection.classList.add("slide");
    supersection.classList.add("level1");
    const section = bundle[0].previousElementSibling;
    section.parentNode.insertBefore(supersection, section);
    supersection.appendChild(section);
    for (let subsection of bundle) {
      supersection.appendChild(subsection);
    }
  }
  Reveal.sync();
  Reveal.setState(Reveal.getState());
}


function addSourceCodeLabels() {
  $("div.sourceCode[label]").each(function () {
    $("<div/>")
      .addClass("language-label")
      .text($(this).attr("label"))
      .prependTo($(this).children('pre'));
  });
}
