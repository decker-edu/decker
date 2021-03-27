import * as g from "/support/geometry/geometry.js";

let surfaceAnchor = g.point(300, 230);
let surfaceNormal = g.vector(surfaceAnchor, 0, -200);
let wi = g.point(80, 100, "drag");
let mwi = g.mirror(surfaceAnchor, wi);
let projection = g.line(
  surfaceAnchor,
  g.project(surfaceNormal.p1, surfaceNormal.p2, wi, "line"),
  "arrow"
);
let proj1 = g.sum(mwi, projection, "arrow");
let proj2 = g.sum(proj1.p2, projection, "arrow");

let root = g.group(
  g.surface(surfaceAnchor, 500),
  surfaceNormal,
  g.line(surfaceAnchor, wi, "arrow"),
  projection,
  g.line(surfaceAnchor, mwi, "arrow"),
  proj1,
  proj2,
  g.line(wi, projection.p2, "infinite"),
  g.line(surfaceAnchor, proj2.p2, "arrow")
);

g.renderSvg(anchor, 600, 400, root);
