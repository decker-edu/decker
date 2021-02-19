import * as g from "/test/decks/geometry/static/geometry.js";

let center = g.point(300, 300, "drag");
let intersection = g.intersect(
  g.line(g.point(60, 60, "drag"), g.point(540, 60, "drag")),
  g.circle(center, 200)
);

let root = g.group(
  g.nthP(intersection, 1),
  g.nthN(intersection, 1),
  g.line(center, g.nthP(intersection, 0)),
  g.swtch(intersection, 0, g.text(250, 400, "Bang!"))
);

g.renderSvg(anchor, 1200, 600, root);
