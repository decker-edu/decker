import { preparePolls } from "../plugins/examiner/examiner-poll.js";
import initializeBlockManipulation from "./block-manip.js";

Reveal.on("ready", () => {
  // Decker.flash.message("Mono plugin initialing ...");
  let pollSession = null;
  Decker.addPresenterModeListener(async function(inPresenterMode) {
    if (inPresenterMode && !pollSession) {
      pollSession = await preparePolls(Reveal);
    } else {
      pollSession.close();
      pollSession = null;
    }
  });

  initializeBlockManipulation();
});
