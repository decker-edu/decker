import * as g from "./static/geometry.js";

let surfaceAnchor = g.point(300, 230, "drag");
let surfaceNormal = g.vector(surfaceAnchor, 0, -180, {
  end: "vec-arrow",
});
let wi = g.point(80, 100, "drag");
let mwi = g.mlabel(g.mirror(surfaceAnchor, wi), "\\mathbf{\\overrightarrow{d}}_i");
let projection = g.line(
  surfaceAnchor,
  g.mlabel(g.project(surfaceNormal.p1, surfaceNormal.p2, wi), "\\mathbf{\\overrightarrow{b}}"),
  { end: "arrow" }
);
let proj1 = g.sum(mwi, projection, { end: "arrow" });
let proj2 = g.sum(proj1.p2, projection, { end: "arrow" });

let root = g.group(
  g.surface(surfaceAnchor, 500),
  surfaceNormal,
  g.line(surfaceAnchor, wi, { end: "arrow" }),
  g.mlabel(
    g.unfold(
      60,
      360,
      projection,
      g.line(surfaceAnchor, mwi, { end: "arrow" }),
      proj1,
      proj2,
      g.line(surfaceAnchor, g.mlabel(proj2.p2, "\\mathbf{\\overrightarrow{d}}_r", "ne", true), {
        end: "arrow",
      })
    ),
    "\\sqrt{x^2+y^2}",
    "e",
    true
  ),
  g.mlabel(wi, "\\mathbf{\\overrightarrow{d}}_i", "ne", true),
  g.mlabel(surfaceNormal.p2, "\\mathbf{\\overrightarrow{n}}", "ne", true)
);

g.renderSvg(anchor, 600, 400, root);
