function initIndexPage() {
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
    container.classList.add("link-additions");
    setupModeLinks(container, url);
    setupProgressIndicator(container, url);
    if (insert === "replace") {
      link.replaceWith(container);
    } else if (insert == "before") {
      link.before(container);
    } else {
      link.after(container);
    }
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
        ? "In Handout-Darstellung öffnen"
        : "Access in handout mode"
    );
    handoutLink.setAttribute(
      "aria-label",
      navigator.language === "de"
        ? "In Handout-Darstellung öffnen"
        : "Access in handout mode"
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

  progress.onclick = function () {
    const percent = this.dataset.value == 100 ? 0 : 100;
    this.setValue(percent);
    localStorage.setItem(this.key, percent);
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
