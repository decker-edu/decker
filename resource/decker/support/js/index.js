const lang = navigator.language;

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
  // find title and subtitle
  const links = Decker.meta.index?.links || [];
  let title = lang === "de" ? "Unbekannter Title" : "unknown title";
  let subtitle = undefined;
  const decks = Decker.meta.decks["by-title"];
  for (const deck of decks) {
    if (url.href.endsWith(deck.url)) {
      title = deck.title;
      subtitle = deck.subtitle;
    }
  }

  // presenter mode
  if (links.includes("presenter")) {
    const presenterLink = document.createElement("a");
    presenterLink.href = url.pathname + "?presenter";
    presenterLink.classList.add("fas", "fa-chalkboard-teacher");
    presenterLink.title = presenterLink.ariaLabel =
      lang === "de" ? "Im Präsentationsmodus öffnen" : "open in presenter mode";
    container.appendChild(presenterLink);
  }

  // handout mode
  if (links.includes("handout")) {
    const handoutLink = document.createElement("a");
    handoutLink.href = url.pathname + "?handout";
    handoutLink.classList.add("handout-link");
    handoutLink.title = handoutLink.ariaLabel =
      lang === "de" ? "Im Handout-Modus öffnen" : "open in handout mode";
    container.appendChild(handoutLink);
  }

  // accessibility mode
  if (links.includes("a11y")) {
    const a11yLink = document.createElement("a");
    a11yLink.href = url.pathname + "?a11y";
    a11yLink.classList.add("fas", "fa-universal-access");
    a11yLink.title = a11yLink.ariaLabel =
      lang === "de"
        ? "In barrierearmer Darstellung öffnen"
        : "open in accessibility mode";
    container.appendChild(a11yLink);
  }

  // pdf export
  if (links.includes("pdf")) {
    const pdf = url.pathname.replace(".html", ".pdf");
    const pdfLink = document.createElement("a");
    pdfLink.href = pdf;
    pdfLink.classList.add("fas", "fa-file-pdf");
    pdfLink.title = pdfLink.ariaLabel =
      lang === "de" ? "Als PDF exportieren" : "export as PDF";
    container.appendChild(pdfLink);

    pdfLink.setAttribute("aria-disabled", true);
    resourceExists(pdf).then((pdfExists) => {
      pdfLink.setAttribute("aria-disabled", !pdfExists);
    });
    // const pdfExists = await resourceExists(
    //   url.pathname.replace(".html", ".pdf")
    // );
    // pdfLink.setAttribute("aria-disabled", !pdfExists);
  }
}

async function insertAdditionalLinks() {
  const selector = Decker.meta.index?.selector || "a[href$='-deck.html']";
  const insert = Decker.meta.index?.progress?.insert || "after";
  const links = document.querySelectorAll(selector);

  for (const link of links) {
    // setup URL
    let url = null;
    try {
      url = new URL(link.href);
    } catch {
      continue;
    }

    // tooltip for deck link
    link.title = link.ariaLabel =
      lang === "de" ? "In Folien-Modus öffnen" : "open in slide mode";

    // get title and subtitle
    let title = lang === "de" ? "Unbekannter Titel" : "Unknown Title";
    let subtitle = undefined;
    const decks = Decker.meta.decks["by-title"];
    for (const deck of decks) {
      if (link.href.endsWith(deck.url)) {
        title = deck.title;
        subtitle = deck.subtitle;
      }
    }

    // setup icon container
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
    container.prepend(link);

    // tooltip for icon container
    container.title = container.ariaLabel =
      lang === "de"
        ? `Foliensatz ${title}${
            subtitle ? " - " + subtitle : ""
          } öffnen: Drücke Eingabe, um Betrachtungsmodus auszuwählen.`
        : `Open slide deck ${title}${
            subtitle ? " - " + subtitle : ""
          }: Press Enter to choose view mode.`;

    // generate mode links (presenter mode, handout mode, a11y mode, pdf mode)
    await setupModeLinks(container, url);

    // generate progress indicators
    setupProgressIndicator(container, url);

    // fix tabindex
    for (const child of container.children) child.tabIndex = -1;
    container.tabIndex = 0;

    // Internal Navigation
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
    if (percent < 0)
      this.title =
        lang === "de"
          ? `Neuer Foliensatz.\n0% betrachtet.\nKlicken zum Wechseln\nzwischen 100% und 0%.`
          : `New slide deck.\n0% watched.\nClick to toggle\nbetween 100% and 0%.`;
    else
      this.title =
        lang === "de"
          ? `${percent}% betrachtet.\nKlicken zum Wechseln\nzwischen 100% und 0%.`
          : `${percent}% watched.\nClick to toggle\nbetween 100% and 0%.`;
  };

  progress.update = function () {
    let percent = localStorage.getItem(this.key) || -1;
    percent = Number(percent);
    if (isNaN(percent) || percent === Infinity || percent > 100) percent = 0;
    this.setValue(percent);
  };

  progress.toggle = function () {
    const percent =
      this.dataset.value < 0 ? 0 : this.dataset.value == 100 ? 0 : 100;
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
 * instead of loading with an intersection observer should be feasible.
 */
function loadSources() {
  const sources = document.querySelectorAll("[data-src]");
  for (const source of sources) {
    source.setAttribute("src", source.getAttribute("data-src"));
  }
}
