function initIndexPage() {
  const mode = Decker.meta.index?.mode || "insert";

  if (mode === "insert") {
    insertAdditionalLinks();
  }
  if (mode === "modal") {
    modalLinks();
  }
  loadSources();
}

async function resourceExists(url) {
  return fetch(url, { method: "HEAD" })
    .then((r) => {
      return r.status === 200;
    })
    .catch((_) => {
      return false;
    });
}

async function setupModeLinks(container, url) {
  const links = Decker.meta.index?.links || [];
  let title = "Unbekannter Titel";
  const decks = Decker.meta.decks["by-title"];
  for (const deck of decks) {
    if (url.href.endsWith(deck.url)) {
      title = deck.title;
    }
  }

  if (links.includes("presenter")) {
    const presenterLink = document.createElement("a");
    presenterLink.href = url.pathname + "?presenter";
    presenterLink.classList.add("fas", "fa-chalkboard-teacher");
    presenterLink.setAttribute(
      "title",
      navigator.language === "de"
        ? "Im Präsentationsmodus öffnen"
        : "Access in presenter mode"
    );
    presenterLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? "Im Präsentationsmodus öffnen"
        : "Access in presenter mode"
    );
    container.appendChild(presenterLink);
  }
  if (links.includes("handout")) {
    const handoutLink = document.createElement("a");
    handoutLink.href = url.pathname + "?handout";
    handoutLink.classList.add("handout-link");
    handoutLink.setAttribute(
      "title",
      navigator.language === "de"
        ? `${title} in Handout-Darstellung öffnen`
        : `Access ${title} in handout mode`
    );
    handoutLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? `${title} in Handout-Darstellung öffnen`
        : `Access ${title} in handout mode`
    );
    container.appendChild(handoutLink);
  }

  if (links.includes("a11y")) {
    const a11yLink = document.createElement("a");
    a11yLink.href = url.pathname + "?a11y";
    a11yLink.classList.add("fas", "fa-universal-access");
    a11yLink.setAttribute(
      "title",
      navigator.language === "de"
        ? "In barrierearmer Darstellung öffnen"
        : "Access in accessibility mode"
    );
    a11yLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? "In barrierearmer Darstellung öffnen"
        : "Access in accessibility mode"
    );
    container.appendChild(a11yLink);
  }

  if (links.includes("pdf")) {
    const exists = await resourceExists(url.pathname.replace(".html", ".pdf"));
    if (exists) {
      const pdfLink = document.createElement("a");
      pdfLink.href = url.pathname.replace(".html", ".pdf");
      pdfLink.classList.add("fas", "fa-file-pdf");
      pdfLink.setAttribute(
        "title",
        navigator.language === "de"
          ? "PDF Export des Foliensatzes herunterladen"
          : "Download presentation PDF"
      );
      pdfLink.setAttribute(
        "aria-label",
        navigator.language === "de"
          ? "PDF Export des Foliensatzes herunterladen"
          : "Download presentation PDF"
      );
      container.appendChild(pdfLink);
    }
  }
}

function insertAdditionalLinks() {
  const selector = Decker.meta.index?.selector || "a[href$='-deck.html']";
  const insert = Decker.meta.index?.progress?.insert || "after";
  const links = document.querySelectorAll(selector);
  for (const link of links) {
    let url = null;
    try {
      url = new URL(link.href);
    } catch {
      continue;
    }
    const container = document.createElement("div");
    container.classList.add("icons");
    let title =
      navigator.language === "de" ? "Unbekannter Titel" : "Unknown Title";
    const decks = Decker.meta.decks["by-title"];
    for (const deck of decks) {
      if (link.href.endsWith(deck.url)) {
        title = deck.title;
      }
    }
    container.title =
      navigator.language === "de"
        ? `Foliensatz ${title} betrachten: Drücke Eingabe, um Betrachtungsmodus auszuwählen.`
        : `View slide deck ${title}: Press Enter to choose view mode.`;
    setupModeLinks(container, url);
    setupProgressIndicator(container, url);
    if (insert === "replace") {
      link.replaceWith(container);
    } else if (insert == "before") {
      link.before(container);
    } else {
      link.after(container);
    }
    container.prepend(link);
    for (const child of container.childNodes) {
      child.setAttribute("tabindex", -1);
    }
    container.setAttribute("tabindex", 0);
    container.addEventListener("keyup", (event) => {
      if (event.target !== container) return;
      if (event.code === "Enter") {
        event.preventDefault();
        event.stopPropagation();
        for (const child of container.childNodes) {
          child.setAttribute("tabindex", 0);
        }
        container.removeAttribute("tabindex");
        container.childNodes[0].focus();
      }
    });
    container.addEventListener("focusout", (event) => {
      if (container.contains(event.relatedTarget)) {
        return;
      } else {
        container.setAttribute("tabindex", 0);
        for (const child of container.childNodes) {
          child.setAttribute("tabindex", -1);
        }
      }
    });
  }
}

function setupProgressIndicator(container, url) {
  if (!localStorage) return;
  if (!Decker.meta.index?.progress) return;

  const progress = document.createElement("span");
  progress.classList.add("progress");
  progress.key = url.pathname + "-percentage";

  progress.setValue = function (percent) {
    this.dataset.value = percent;
    this.style = `--progress: ${percent}%`;
    this.title =
      navigator.language === "de"
        ? `${percent}% betrachtet.\nKlicken zum Wechseln\nzwischen 100% und 0%.`
        : `${percent}% watched.\nClick to toggle\nbetween 100% and 0%.`;
  };

  progress.update = function () {
    let percent = localStorage.getItem(this.key) || 0;
    percent = Number(percent);
    if (isNaN(percent) || percent === Infinity || percent > 100) percent = 0;
    this.setValue(percent);
  };

  progress.toggle = function () {
    const percent = this.dataset.value == 100 ? 0 : 100;
    this.setValue(percent);
    localStorage.setItem(this.key, percent);
  };

  progress.onclick = function () {
    this.toggle();
  };
  progress.onkeyup = function (event) {
    if (event.code === "Enter") {
      this.toggle();
      event.preventDefault();
      event.stopPropagation();
    }
  };

  progress.update();
  container.appendChild(progress);
}

let modalDialog = undefined;
let modalLinkGroup = undefined;
let modalMessage = undefined;
let modalCloseButton = undefined;

function modalLinks() {
  const selector = Decker.meta.index?.selector || "a[href$='-deck.html']";
  const links = document.querySelectorAll(selector);

  modalDialog = document.createElement("dialog");
  document.body.appendChild(modalDialog);

  modalMessage = document.createElement("h2");
  modalDialog.appendChild(modalMessage);

  modalLinkGroup = document.createElement("div");
  modalLinkGroup.className = "group";

  modalDialog.appendChild(modalLinkGroup);

  modalCloseButton = document.createElement("button");
  modalCloseButton.addEventListener("click", (event) => {
    modalDialog.close();
  });
  modalCloseButton.className = "fas fa-times";
  modalCloseButton.ariaLabel =
    navigator.language === "de" ? "Dialog Schließen" : "Close Dialog";
  modalCloseButton.title =
    navigator.language === "de" ? "Dialog Schließen" : "Close Dialog";
  modalDialog.appendChild(modalCloseButton);

  for (const link of links) {
    addModalToLink(link);
  }
}

function addModalToLink(link) {
  const modes = Decker.meta.index?.links || [];
  link.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    modalMessage.innerText =
      navigator.language === "de"
        ? `Präsentation ${link.textContent} aufrufen?`
        : `Visit Slide Deck ${link.textContent}?`;
    // remove previous elements in the link group
    while (modalLinkGroup.firstElementChild) {
      modalLinkGroup.firstElementChild.remove();
    }
    const normalLink = document.createElement("a");
    normalLink.href = link.href;
    modalLinkGroup.appendChild(normalLink);
    const normalIcon = document.createElement("i");
    normalIcon.className = "fas fa-chalkboard";
    normalLink.appendChild(normalIcon);
    const normalLabel = document.createElement("span");
    normalLink.appendChild(normalLabel);
    normalLabel.innerText = "Präsentation";
    normalLink.setAttribute(
      "title",
      navigator.language === "de"
        ? "Präsentation aufrufen"
        : "Access presentation"
    );
    normalLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? "Präsentation aufrufen"
        : "Access presentation"
    );
    if (modes.includes("a11y")) {
      const a11yLink = document.createElement("a");
      const a11yIcon = document.createElement("i");
      const a11yLabel = document.createElement("span");
      a11yLink.appendChild(a11yIcon);
      a11yLink.appendChild(a11yLabel);
      a11yLink.href = link.href + "?a11y";
      a11yIcon.className = "fas fa-universal-access";
      a11yLink.setAttribute(
        "title",
        navigator.language === "de"
          ? "In barrierearmer Darstellung öffnen"
          : "Access in accessibility mode"
      );
      a11yLink.setAttribute(
        "aria-label",
        navigator.language === "de"
          ? "In barrierearmer Darstellung öffnen"
          : "Access in accessibility mode"
      );
      a11yLabel.innerText =
        navigator.language === "de" ? "Barrierearm" : "Accessible";
      modalLinkGroup.appendChild(a11yLink);
    }

    if (modes.includes("handout")) {
      const handoutLink = document.createElement("a");
      const handoutIcon = document.createElement("i");
      const handoutLabel = document.createElement("span");
      handoutLink.appendChild(handoutIcon);
      handoutLink.appendChild(handoutLabel);
      handoutIcon.className = "handout-link";
      handoutLink.href = link.href + "?handout";
      handoutLink.setAttribute(
        "title",
        navigator.language === "de"
          ? "In Handout-Darstellung öffnen"
          : "Access in handout mode"
      );
      handoutLink.setAttribute(
        "aria-label",
        navigator.language === "de"
          ? "In Handout-Darstellung öffnen"
          : "Access in handout mode"
      );
      handoutLabel.innerText =
        navigator.language === "de" ? "Handout" : "Handout";
      modalLinkGroup.appendChild(handoutLink);
    }

    if (modes.includes("presenter")) {
      const presenterLink = document.createElement("a");
      const presenterIcon = document.createElement("i");
      const presenterLabel = document.createElement("span");
      presenterLink.appendChild(presenterIcon);
      presenterLink.appendChild(presenterLabel);
      presenterLink.href = link.href + "?a11y";
      presenterIcon.className = "fas fa-chalkboard-teacher";
      presenterLink.setAttribute(
        "title",
        navigator.language === "de"
          ? "Im Präsentationsmodus öffnen"
          : "Access in presenter mode"
      );
      presenterLink.setAttribute(
        "aria-label",
        navigator.language === "de"
          ? "Im Präsentationsmodus öffnen"
          : "Access in presenter mode"
      );
      presenterLabel.innerText =
        navigator.language === "de"
          ? "Präsentationsdarstellung"
          : "Presenter Display";
      modalLinkGroup.appendChild(presenterLink);
    }
    modalDialog.showModal();
  });
}

function updateProgressIndicators() {
  document.querySelectorAll(".progress").forEach((progress) => {
    progress.update();
  });
}

document.addEventListener("visibilitychange", () => {
  if (!document.hidden) updateProgressIndicators();
});

/* Index Pages should be small enough that loading all sources at once
 * instead of loading with an intersection observer should be feasable.
 */
function loadSources() {
  const sources = document.querySelectorAll("[data-src]");
  for (const source of sources) {
    source.setAttribute("src", source.getAttribute("data-src"));
  }
}
