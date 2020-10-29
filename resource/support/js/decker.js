if (typeof Reveal === 'undefined') {
  console.error("decker.js has to be loaded after reveal.js");
}
else {
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
  currentDate();
  addSourceCodeLabels();
  prepareTaskLists();
  prepareFullscreenIframes();
}


function prepareTaskLists() {
  for (let cb of document.querySelectorAll('.reveal ul.task-list>li>input[type="checkbox"]')) {
    var li = cb.parentElement;
    li.classList.add(cb.checked ? "task-yes" : "task-no");
  }
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
  var date = document.querySelector(".date");
  if (!date) return;
  var dateString = date.textContent.trim();

  var today = new Date().toISOString().substr(0, 10);

  if (dateString === "today") {
    date.textContent = today;
  }
}

function makeVertical() {
  const subsections = Array.from(document.getElementsByClassName("sub")).filter(s => s.nodeName === "SECTION");
  const subsection_bundles = [];
  for (let i = 0; i < subsections.length; i++) {
    const subsection = subsections[i];
    const bundle = [subsection];
    var subtemp = subsection;
    while (
      i + 1 < subsections.length &&
      subtemp.nextElementSibling === subsections[i + 1]
    ) {
      i += 1;
      bundle.push(subsections[i]);
      subtemp = subtemp.nextElementSibling;
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


function prepareCodeHighlighting() {
  for (let code of document.querySelectorAll('pre>code')) {
    var pre = code.parentElement;

    // if line numbers to be highlighted are specified...
    if (pre.hasAttribute("data-line-numbers")) {
      // ...copy them from <pre> to <code>
      code.setAttribute("data-line-numbers", pre.getAttribute("data-line-numbers"));
    }
    // otherwise, if we specified .line-numbers...
    else if (pre.classList.contains("line-numbers")) {
      // ...set empty attribute data-line-numbers, 
      // so reveal adds line numbers w/o highlighting
      code.setAttribute("data-line-numbers", "");
    }

    // construct caption
    if (pre.hasAttribute("data-caption")) {
      var parent = pre.parentElement;
      var figure = document.createElement("figure");
      var caption = document.createElement("figcaption");
      var content = pre.getAttribute("data-caption");

      parent.insertBefore(figure, pre);
      figure.appendChild(pre);
      figure.appendChild(caption);
      caption.innerHTML = content.trim();
    }
  }
}

// wrap iframe demos in a div that offers a fullscreen button.
// only do this if the browser supports the Fullscreen API.
// don't do this for Safari, since its webkit-prefixed version
// does not work propertly: one cannot put an iframe to fullscreen
// if the slides are in fullscreen already (which is the standard
// presentation setting).
// we wrap the div in any case to make the css simpler.
function prepareFullscreenIframes() {
  for (let iframe of document.querySelectorAll('iframe.decker')) {
    // wrap div around iframe
    var parent = iframe.parentElement;
    var div = document.createElement("div");
    div.classList.add("fs-container");
    div.style.width = iframe.style.width || "100%";
    div.style.height = iframe.style.height || "100%";
    if (iframe.classList.contains("stretch")) {
      div.classList.add("stretch");
      iframe.classList.remove("stretch");
    }
    parent.insertBefore(div, iframe);
    div.appendChild(iframe);

    // iframe should be full width/height within div
    iframe.style.width = "100%";
    iframe.style.height = "100%";

    // if fullscreen API is not supported then don't add the button
    if (!div.requestFullscreen) continue;

    // add fullscreen button
    var btn = document.createElement("button");
    btn.classList.add("fs-button");
    btn.innerHTML = '<i class="fas fa-expand-arrows-alt" style="font-size:20px"></i>';
    div.btn = btn;
    div.appendChild(btn);

    // handle button click: enter/exit fullscreen
    btn.onclick = function () {
      var doc = window.document;
      var container = this.parentElement;
      if (doc.fullscreenElement == container)
        doc.exitFullscreen();
      else
        container.requestFullscreen();
    };

    // handle fullscreen change: adjust button icon
    div.onfullscreenchange = function () {
      var doc = window.document;
      this.btn.innerHTML =
        doc.fullscreenElement == this ?
          '<i class="fas fa-compress-arrows-alt"></i>' :
          '<i class="fas fa-expand-arrows-alt"></i>';
    };
  }
}


function isElectron() {
  // Renderer process
  if (typeof window !== 'undefined' && typeof window.process === 'object' && window.process.type === 'renderer') {
    return true;
  }

  // Main process
  if (typeof process !== 'undefined' && typeof process.versions === 'object' && !!process.versions.electron) {
    return true;
  }

  // Detect the user agent when the `nodeIntegration` option is set to true
  if (typeof navigator === 'object' && typeof navigator.userAgent === 'string' && navigator.userAgent.indexOf('Electron') >= 0) {
    return true;
  }

  return false;
}
