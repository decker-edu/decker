function initIndexPage() {
  insertAdditionalLinks();
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
  let subtitle = undefined;
  const decks = Decker.meta.decks["by-title"];
  for (const deck of decks) {
    if (url.href.endsWith(deck.url)) {
      title = deck.title;
      subtitle = deck.subtitle;
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
        ? `${title}${
            subtitle ? " - " + subtitle : ""
          } in Handout-Darstellung öffnen`
        : `Access ${title}${subtitle ? " - " + subtitle : ""} in handout mode`
    );
    handoutLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? `${title}${
            subtitle ? " - " + subtitle : ""
          } in Handout-Darstellung öffnen`
        : `Access ${title}${subtitle ? " - " + subtitle : ""} in handout mode`
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
    pdfLink.setAttribute("aria-disabled", "true");
    pdfLink.addEventListener("click", (event) => {
      if (pdfLink.ariaDisabled === "true") {
        event.preventDefault();
        event.stopPropagation();
      }
    });
    container.appendChild(pdfLink);
    resourceExists(url.pathname.replace(".html", ".pdf")).then((exists) => {
      if (exists) {
        pdfLink.removeAttribute("aria-disabled");
      }
    });
  }
}

async function insertAdditionalLinks() {
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
    let container = link.closest(".icons");
    if (!container) {
      const container = document.createElement("div");
      container.classList.add("icons");
      if (insert === "replace") {
        link.replaceWith(container);
      } else if (insert == "before") {
        link.before(container);
      } else {
        link.after(container);
      }
    }
    let title =
      navigator.language === "de" ? "Unbekannter Titel" : "Unknown Title";
    let subtitle = undefined;
    const decks = Decker.meta.decks["by-title"];
    for (const deck of decks) {
      if (link.href.endsWith(deck.url)) {
        title = deck.title;
        subtitle = deck.subtitle;
      }
    }
    container.title =
      navigator.language === "de"
        ? `Foliensatz ${title}${
            subtitle ? " - " + subtitle : ""
          } betrachten: Drücke Eingabe, um Betrachtungsmodus auszuwählen.`
        : `View slide deck ${title}${
            subtitle ? " - " + subtitle : ""
          }: Press Enter to choose view mode.`;
    container.ariaLabel =
      navigator.language === "de"
        ? `Foliensatz ${title}${
            subtitle ? " - " + subtitle : ""
          } betrachten: Drücke Eingabe, um Betrachtungsmodus auszuwählen.`
        : `View slide deck ${title}${
            subtitle ? " - " + subtitle : ""
          }: Press Enter to choose view mode.`;
    await setupModeLinks(container, url);
    setupProgressIndicator(container, url);
    container.prepend(link);
    for (const child of container.children) {
      child.setAttribute("tabindex", -1);
    }
    container.setAttribute("tabindex", 0);
    /* Internal Navigation */
    container.addEventListener("keydown", (event) => {
      if (container.contains(document.activeElement)) {
        if (event.code === "ArrowRight") {
          const next = document.activeElement.nextElementSibling;
          if (next) {
            event.preventDefault();
            event.stopPropagation();
            next.focus();
          }
        }
        if (event.code === "ArrowLeft") {
          const prev = document.activeElement.previousElementSibling;
          if (prev) {
            event.preventDefault();
            event.stopPropagation();
            prev.focus();
          }
        }
      }
      if (event.code === "ArrowDown") {
        const containingRow = container.closest("tr");
        const nextRow = containingRow.nextElementSibling;
        if (nextRow) {
          const nextIcons = nextRow.querySelector(".icons");
          if (nextIcons) {
            event.preventDefault();
            event.stopPropagation();
            nextIcons.focus();
          }
        }
      }
      if (event.code === "ArrowUp") {
        const containingRow = container.closest("tr");
        const prevRow = containingRow.previousElementSibling;
        if (prevRow) {
          const prevIcons = prevRow.querySelector(".icons");
          if (prevIcons) {
            event.preventDefault();
            event.stopPropagation();
            prevIcons.focus();
          }
        }
      }
    });
    container.addEventListener("keyup", (event) => {
      if (event.target !== container) return;
      if (event.code === "Enter") {
        event.preventDefault();
        event.stopPropagation();
        for (const child of container.children) {
          child.setAttribute("tabindex", 0);
        }
        container.removeAttribute("tabindex");
        container.children[0].focus();
      }
    });
    container.addEventListener("focusout", (event) => {
      if (container.contains(event.relatedTarget)) {
        return;
      } else {
        container.setAttribute("tabindex", 0);
        for (const child of container.children) {
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
