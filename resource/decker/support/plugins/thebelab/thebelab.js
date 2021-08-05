/**
 *
 * @author Henrik Tramberend
 */
var RevealThebelab = window.RevealThebelab || (function () {
    if (!Reveal.getConfig().thebelab) return;

    return {
        init: function () {
            thebelab.bootstrap();
        }
    }
})();

Reveal.registerPlugin('thebelab', RevealThebelab);