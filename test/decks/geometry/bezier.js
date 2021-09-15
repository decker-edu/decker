import * as g from "../../../support/geometry/geometry.js";

let p1 = g.point(60, 60, "drag");
let p2 = g.point(540, 60);
let p3 = g.point(540, 340);
let p4 = g.point(60, 340, "drag");
let l1 = g.line(p1, p2);
let l2 = g.line(p2, p3);
let l3 = g.line(p3, p4);
let segment = g.bezier(p1, p2, p3, p4);

g.renderSvg(anchor, 600, 400, g.group(segment, l1, l2, l3));
