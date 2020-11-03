Reveal.addEventListener("ready", event => {
  console.log(event.currentSlide.id);

  let body = document.querySelector("body");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let user = document.createElement("input");
  let close = document.createElement("div");
  let list = document.createElement("div");
  let input = document.createElement("div");
  let text = document.createElement("textarea");
  let footer = document.createElement("div");
  let deckid = document.createElement("input");
  let slideid = document.createElement("input");

  panel.classList.add("q-panel");
  header.classList.add("q-header");
  user.setAttribute("placeholder", "Enter user token (try person1 or person2)");
  header.appendChild(user);
  header.appendChild(close);
  close.classList.add("q-close");
  close.textContent = "✖";

  list.classList.add("q-list");

  input.classList.add("q-input");
  input.appendChild(text);
  text.setAttribute("rows", 4);

  footer.classList.add("q-footer");
  footer.appendChild(deckid);
  deckid.setAttribute("placeholder", "Deck ID");
  deckid.setAttribute("disabled", true);
  footer.appendChild(slideid);
  slideid.setAttribute("placeholder", "Slide ID");
  slideid.setAttribute("disabled", true);

  panel.appendChild(header);
  panel.appendChild(list);
  panel.appendChild(input);
  panel.appendChild(footer);

  document.body.appendChild(panel);

  let getContext = () => {
    return {
      deck: body.getAttribute("data-deckid"),
      slide: Reveal.getCurrentSlide().id,
      token: user.value
    };
  };

  let updateIds = () => {
    let context = getContext();
    deckid.value = context.deck;
    slideid.value = context.slide;
  };

  user.addEventListener("keydown", e => {
    if (e.key === "Enter") {
      updateCommentList(getContext, list);
      updateIds();
      e.stopPropagation();
      document.activeElement.blur();
    }
  });

  text.addEventListener("keydown", e => {
    if (e.key === "Enter" && e.shiftKey) {
      submitComment(getContext, list, e.target);
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  updateCommentList(getContext, list);
  updateIds();

  Reveal.addEventListener("slidechanged", event => {
    console.log(getContext());
    console.log(event.currentSlide.id);
    updateCommentList(getContext, list);
    updateIds();
  });
});
