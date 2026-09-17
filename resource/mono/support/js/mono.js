import { preparePolls } from "../plugins/examiner/examiner-poll.js";
import initializeBlockManipulation from "./block-manip.js";

// Styles for block manipulation are only needed with this plugin.
const style = document.createElement("link");
style.rel = "stylesheet";
style.href = import.meta.url.replace("js/mono.js", "css/block-manip.css");
document.head.appendChild(style);

Reveal.on("ready", () => {
  Decker.flashMessage("Mono plugin initialing ...");
  let pollSession = null;
  Decker.addPresenterModeListener(async function (inPresenterMode) {
    if (inPresenterMode && !pollSession) {
      pollSession = await preparePolls(Reveal);
    } else {
      pollSession.close();
      pollSession = null;
    }
  });

  initializeBlockManipulation();
});
