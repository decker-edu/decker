import * as g from "/test/decks/geometry/static/geometry.js";

let surfaceAnchor = g.point(300, 230, "drag");
let surfaceNormal = g.vector(surfaceAnchor, 0, -200, {
  end: "vec-arrow",
});
let wi = g.point(80, 100, "drag");
let mwi = g.label(g.mirror(surfaceAnchor, wi), "-dᵢ");
let projection = g.line(
  surfaceAnchor,
  g.label(g.project(surfaceNormal.p1, surfaceNormal.p2, wi), "b"),
  { end: "arrow" }
);
let proj1 = g.sum(mwi, projection, { end: "arrow" });
let proj2 = g.sum(proj1.p2, projection, { end: "arrow" });

let root = g.group(
  g.surface(surfaceAnchor, 500),
  surfaceNormal,
  g.line(surfaceAnchor, wi, { end: "arrow" }),
  g.label(
    g.unfold(
      60,
      360,
      projection,
      g.line(surfaceAnchor, mwi, { end: "arrow" }),
      proj1,
      proj2,
      g.line(surfaceAnchor, g.label(proj2.p2, "dᵣ"), {
        end: "arrow",
      })
    ),
    "Click me!"
  ),
  g.label(wi, "dᵢ"),
  g.label(surfaceNormal.p2, "n")
);

g.renderSvg(anchor, 600, 400, root);
