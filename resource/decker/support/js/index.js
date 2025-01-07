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

function setupModeLinks(container, url) {
  const links = Decker.meta.index?.links || [];

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
}

function setupProgressIndicator(container, url) {
  if (!localStorage) return;

  // Scrape settings from Decker meta
  if (!Decker.meta.index?.progress) return;

  const key = url.pathname + "-percentage";
  let percent = localStorage.getItem(key) || 0;
  // Reset if percent is faulty
  percent = Number(percent);
  if (isNaN(percent) || percent === Infinity || percent > 100) {
    console.log(
      "[index.js] reset percent progress for " +
        key +
        " because value is " +
        percent
    );
    percent = 0;
    localStorage.setItem(key, 0);
  }

  const progress = document.createElement("progress");
  progress.max = 100;
  progress.value = percent;
  progress.key = key;
  if (navigator.language === "de") {
    progress.title = `${percent}% betrachtet.\nDoppelklick zum Wechseln\nzwischen 100% und 0%.`;
  } else {
    progress.title = `${percent}% watched.\nDouble-click to toggle\nbetween 100% and 0%.`;
  }

  container.appendChild(progress);

  progress.ondblclick = function () {
    this.value = this.value == 100 ? 0 : 100;
    if (navigator.language === "de") {
      progress.title = `${this.value}% betrachtet.\nDoppelklick zum Wechseln\nzwischen 100% und 0%.`;
    } else {
      progress.title = `${this.value}% watched.\nDouble-click to toggle\nbetween 100% and 0%.`;
    }
    localStorage.setItem(this.key, this.value);
  };
}

function modalLinks() {
  const selector = Decker.meta.index?.selector || "a[href$='-deck.html']";
  const links = document.querySelectorAll(selector);

  const modal = document.createElement("dialog");
  document.body.appendChild(modal);

  const visitMessage = document.createElement("h2");
  modal.appendChild(visitMessage);

  const modes = Decker.meta.index?.links || [];

  const linkGroup = document.createElement("div");
  linkGroup.className = "group";

  modal.appendChild(linkGroup);

  const closeButton = document.createElement("button");
  closeButton.addEventListener("click", (event) => {
    modal.close();
  });
  closeButton.className = "fas fa-times";
  closeButton.ariaLabel =
    navigator.language === "de" ? "Dialog Schließen" : "Close Dialog";
  closeButton.title =
    navigator.language === "de" ? "Dialog Schließen" : "Close Dialog";
  modal.appendChild(closeButton);

  for (const link of links) {
    link.addEventListener("click", (event) => {
      event.preventDefault();
      event.stopPropagation();
      visitMessage.innerText =
        navigator.language === "de"
          ? `Präsentation ${link.textContent} aufrufen?`
          : `Visit Slide Deck ${link.textContent}?`;
      while (linkGroup.firstElementChild) {
        linkGroup.firstElementChild.remove();
      }
      const normalLink = document.createElement("a");
      normalLink.href = link.href;
      linkGroup.appendChild(normalLink);
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
        linkGroup.appendChild(a11yLink);
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
        linkGroup.appendChild(handoutLink);
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
        linkGroup.appendChild(presenterLink);
      }
      modal.showModal();
    });
  }
}

/* Index Pages should be small enough that loading all sources at once
 * instead of loading with an intersection observer should be feasable.
 */
function loadSources() {
  const sources = document.querySelectorAll("[data-src]");
  for (const source of sources) {
    source.setAttribute("src", source.getAttribute("data-src"));
  }
}
