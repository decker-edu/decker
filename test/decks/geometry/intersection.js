import * as g from "/test/decks/geometry/static/geometry.js";

let center = g.point(300, 300, "drag");
let intersection = new g.IsectLineCircle(
  g.line(g.point(60, 60, "drag"), g.point(540, 60, "drag")),
  g.circle(center, 200)
);

let root = g.group(
  intersection.p2,
  intersection.n2,
  intersection.p1,
  intersection,
  g.line(center, intersection.p1),
  g.swtch(intersection.p1, g.text(250, 400, "Bang!"))
);

g.renderSvg(anchor, 1200, 600, root);
