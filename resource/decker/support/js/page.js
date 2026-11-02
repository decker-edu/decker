let observer = undefined;

function initPage() {
  // Use the viewport as root, because the scrolling element differs between
  // resource packs. Trigger on the first visible pixel, because elements can be
  // taller than the viewport and may never be fully visible. Extend the root by
  // one viewport height, to start loading before an element scrolls into view.
  observer = new IntersectionObserver(cameIntoView, {
    rootMargin: "100% 0px",
    threshold: 0,
  });
  let targets = document.querySelectorAll("[data-src]");
  for (const target of targets) {
    observer.observe(target);
  }
}

function cameIntoView(entries, observer) {
  entries.forEach((entry) => {
    if (entry.isIntersecting) {
      entry.target.setAttribute("src", entry.target.getAttribute("data-src"));
      observer.unobserve(entry.target);
    }
  });
}
