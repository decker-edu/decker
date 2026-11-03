let observer = undefined;

function initPage() {
  setupLazyLoading();
}

function setupLazyLoading() {
  // Use the viewport as root, because the scrolling element differs between
  // resource packs. Trigger on the first visible pixel, because elements can be
  // taller than the viewport and may never be fully visible. Extend the root by
  // one half of viewport height, to start loading before an element scrolls into view.
  let options = {
    rootMargin: "50% 0px",
    threshold: 0
  };
  observer = new IntersectionObserver(intersectionCallback, options);

  let targets = document.querySelectorAll("[data-src]");
  for (const target of targets) {
    observer.observe(target);
  }
}

function intersectionCallback(entries, observer) {
  entries.forEach((entry) => {
    const el = entry.target;

    // load iframes and videos when they come into view
    if (entry.isIntersecting) {
      if (el.getAttribute("src") !== el.getAttribute("data-src"))
        el.setAttribute("src", el.getAttribute("data-src"));
      if (el.tagName !== "IFRAME") observer.unobserve(el);
    }

    // unload iframes when they get out of view
    else if (el.tagName === "IFRAME" && el.hasAttribute("src")) {
      el.setAttribute("src", "about:blank");
    }
  });
}
