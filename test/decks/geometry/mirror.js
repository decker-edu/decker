import * as g from "./static/geometry.js";

let surfaceAnchor = g.point(300, 230);
let surfaceNormal = g.vector(surfaceAnchor, 0, -200, {
  end: "vec-arrow",
});
let wi = g.point(80, 100, "drag");
let mwi = g.mirror(surfaceAnchor, wi);
let projection = g.line(
  surfaceAnchor,
  g.project(surfaceNormal.p1, surfaceNormal.p2, wi),
  { end: "arrow" }
);
let proj1 = g.sum(mwi, projection, { end: "arrow" });
let proj2 = g.sum(proj1.p2, projection, { end: "arrow" });

let root = g.group(
  g.surface(surfaceAnchor, 500),
  surfaceNormal,
  g.line(surfaceAnchor, wi, { end: "arrow" }),
  projection,
  g.line(surfaceAnchor, mwi, { end: "arrow" }),
  proj1,
  proj2,
  g.line(surfaceAnchor, proj2.p2, {
    end: "arrow",
  })
);

g.renderSvg(anchor, 600, 400, root);
