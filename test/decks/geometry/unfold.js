import * as g from "./static/geometry.js";

let surfaceAnchor = g.point(300, 230, "drag");
let surfaceNormal = g.vector(surfaceAnchor, 0, -180);
let wi = g.point(80, 100, "drag");
let mwi = g.mlabel(g.mirror(surfaceAnchor, wi), "\\mathbf{-d}_i");
let projection = g.line(
  surfaceAnchor,
  g.mlabel(g.project(surfaceNormal.p1, surfaceNormal.p2, wi), "\\mathbf{b}"),
  "arrow"
);
let proj1 = g.sum(mwi, projection, "arrow");
let proj2 = g.sum(proj1.p2, projection, "arrow");
let unfold = g.unfold(
  60,
  360,
  g.group(projection, g.line(wi, projection.p2, "infinite")),
  g.line(surfaceAnchor, mwi, "arrow"),
  g.group(proj1, g.mlabel(proj1.p2, "\\mathbf{b}", "ne")),
  g.group(proj2, g.mlabel(proj2.p2, "\\mathbf{b}", "se")),
  g.line(
    surfaceAnchor,
    g.mlabel(proj2.p2, "\\mathbf{d}_r", "ne", true),
    "arrow"
  )
);
let root = g.group(
  g.surface(surfaceAnchor, 500),
  surfaceNormal,
  g.line(surfaceAnchor, wi, "arrow"),
  g.label(unfold, "Click (and drag) me plenty!", "ne", true),
  g.mlabel(unfold, "\\sqrt{x^2+y^2}", "se", true),
  g.mlabel(wi, "\\mathbf{d}_i", "ne", true),
  g.mlabel(surfaceNormal.p2, "\\mathbf{n}", "ne", true)
);

g.renderSvg(anchor, 600, 400, root);
