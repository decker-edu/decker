export {
  point,
  label,
  line,
  vector,
  circle,
  bezier,
  group,
  text,
  swtch,
  unfold,
  mirror,
  project,
  sum,
  surface,
  intersect,
  nthP,
  nthN,
  renderSvg,
};

import "/test/decks/geometry/static/d3.v6.min.js";

const defaults = {
  point: { r: 9, opts: ["static"] }, // one of ["static", "drag", "computed"]
  line: {
    kind: "segment",
    start: null,
    end: null,
  },
  vector: {
    end: "arrow",
  },
  circle: {},
  unit: 60,
  arrow: { w: 5, h: 5 },
};

let nextId = 0;

class Shape {
  constructor(z = 1) {
    this.id = nextId++;
    this.zIndex = z;
  }

  evaluate() {
    return { complete: true, flat: [this] };
  }

  allComplete(dependencies) {
    let complete = true;
    let shapes = [];
    for (let shape of dependencies) {
      let e = shape.evaluate();
      complete &&= e.complete;
      shapes = [...shapes, ...e.flat];
    }
    return {
      complete: complete,
      flat: complete ? [this, ...shapes] : shapes,
    };
  }
}

class Point extends Shape {
  constructor(x, y, ...opts) {
    super();
    this.x = x;
    this.y = y;
    this.r = defaults.point.r;
    this.opts = opts.length == 0 ? defaults.point.opts : opts;
    if (this.opts.includes("drag")) this.zIndex = 1000;
    else this.zIndex = 100;
  }

  move(x, y) {
    this.x = x;
    this.y = y;
  }

  update(element) {
    d3.select(element).attr("transform", `translate(${this.x},${this.y})`);
  }

  svg() {
    let g = d3
      .create("svg:g")
      .attr("id", this.id)
      .attr("transform", `translate(${this.x},${this.y})`);
    let v = d3.create("svg:circle").attr("r", this.r);

    if (this.opts.includes("drag")) {
      g.attr("class", "handle");
      g.append(() => v.node()).attr("class", "point drag");
      g.append("svg:circle")
        .attr("class", "handle")
        .attr("r", this.r * 2);
    } else if (this.opts.includes("computed")) {
      g.append(() => v.node()).attr("class", "point computed");
    } else if (this.opts.includes("hidden")) {
      g.attr("class", "handle");
      g.append("svg:circle")
        .attr("class", "handle")
        .attr("r", this.r * 2);
    } else if (this.opts.includes("static")) {
      g.append(() => v.node()).attr("class", "point view");
    }
    return g.node();
  }
}

function point(...args) {
  return new Point(...args);
}

class Text extends Shape {
  constructor(x, y, text, opts = {}) {
    super();
    this.x = x;
    this.y = y;
    this.text = text;
    this.opts = { ...defaults.point, ...opts };
  }

  update(element) {
    d3.select(element).attr("x", this.x).attr("y", this.y);
  }

  svg() {
    return d3
      .create("svg:text")
      .attr("class", "text")
      .attr("id", this.id)
      .attr("x", this.x)
      .attr("y", this.y)
      .text(this.text)
      .node();
  }
}

function text(...args) {
  return new Text(...args);
}

class Line extends Shape {
  constructor(p1, p2, opts = {}) {
    super();
    this.p1 = p1;
    this.p2 = p2;
    this.opts = { ...defaults.line, ...opts };
    this.zIndex = 10;
  }

  get dx() {
    return this.p2.x - this.p1.x;
  }

  get dy() {
    return this.p2.y - this.p1.y;
  }

  get length() {
    return Math.sqrt(this.dx * this.dx + this.dy * this.dy);
  }

  evaluate() {
    return this.allComplete([this.p1, this.p2]);
  }

  update(element) {
    d3.select(element)
      .attr("x1", this.p1.x)
      .attr("y1", this.p1.y)
      .attr("x2", this.p2.x)
      .attr("y2", this.p2.y);
  }

  svg(w, h) {
    let line = d3
      .create("svg:line")
      .attr("id", this.id)
      .attr("class", "line")
      .attr("x1", this.p1.x)
      .attr("y1", this.p1.y)
      .attr("x2", this.p2.x)
      .attr("y2", this.p2.y);

    if (this.opts.kind === "infinite") {
    }
    if (this.opts.end) {
      line.attr("marker-end", `url(#${this.opts.end})`);
    }
    return line.node();
  }
}

function line(...args) {
  return new Line(...args);
}

class Surface extends Shape {
  constructor(p, w, opts = {}) {
    super();
    this.p = p;
    this.w = w;
    this.opts = { ...defaults.line, ...opts };
    this.zIndex = 1;
  }

  update(element) {
    d3.select(element).attr("transform", `translate(${this.p.x},${this.p.y})`);
  }

  svg(w, h) {
    let surface = d3
      .create("svg:g")
      .attr("class", "surface")
      .attr("id", this.id)
      .attr("transform", `translate(${this.p.x},${this.p.y})`);
    surface
      .append("svg:rect")
      .attr("x", -this.w / 2)
      .attr("y", 0)
      .attr("width", this.w)
      .attr("height", this.w / 7);
    surface
      .append("svg:line")
      .attr("x1", -this.w / 2)
      .attr("y1", 0)
      .attr("x2", this.w / 2)
      .attr("y2", 0);

    return surface.node();
  }
}

function surface(...args) {
  return new Surface(...args);
}

class Vector extends Shape {
  constructor(p, nx, ny, opts = {}) {
    super();
    this.p = p;
    this._p = null;
    this.nx = nx;
    this.ny = ny;
    this.opts = { ...defaults.vector, ...opts };
    this.zIndex = 10;
  }

  get p1() {
    return this.p;
  }

  get p2() {
    if (this._p) {
      this._p.x = this.p.x + this.nx;
      this._p.y = this.p.y + this.ny;
    } else {
      this._p = point(this.p.x + this.nx, this.p.y + this.ny, "invisible");
    }
    return this._p;
  }

  evaluate() {
    let result = this.allComplete([this.p]);
    if (this._p) {
      this._p.x = this.p.x + this.nx;
      this._p.y = this.p.y + this.ny;
    }
    return result;
  }

  addP2(line) {
    if (this.opts.end) {
      let l = Math.sqrt(this.nx * this.nx + this.ny * this.ny);
      let nx = (this.nx / l) * (l - defaults.arrow.w);
      let ny = (this.ny / l) * (l - defaults.arrow.w);
      line.attr("marker-end", "url(#vec-arrow)");
      line.attr("x2", this.p.x + nx).attr("y2", this.p.y + ny);
    } else {
      line.attr("x2", this.p.x + this.nx).attr("y2", this.p.y + this.ny);
    }
  }

  update(element) {
    let line = d3.select(element).attr("x1", this.p.x).attr("y1", this.p.y);
    this.addP2(line);
  }

  svg(w, h) {
    let line = d3
      .create("svg:line")
      .attr("id", this.id)
      .attr("class", "vector")
      .attr("x1", this.p.x)
      .attr("y1", this.p.y);
    this.addP2(line);

    return line.node();
  }
}

function vector(...args) {
  return new Vector(...args);
}

class Circle extends Shape {
  constructor(c, r, opts = {}) {
    super();
    this.c = c;
    this.r = r;
    this.opts = { ...defaults.point, ...opts };
  }

  evaluate() {
    let ec = this.c.evaluate();
    return {
      complete: ec.complete,
      flat: ec.complete ? [this, ...ec.flat] : ec.flat,
    };
  }

  update(element) {
    d3.select(element)
      .attr("cx", this.c.x)
      .attr("cy", this.c.y)
      .attr("r", this.r);
  }

  svg(w, h) {
    return d3
      .create("svg:circle")
      .attr("id", this.id)
      .attr("class", "circle")
      .attr("cx", this.c.x)
      .attr("cy", this.c.y)
      .attr("r", this.r)
      .node();
  }
}

function circle(...args) {
  return new Circle(...args);
}

class Bezier extends Shape {
  constructor(p1, p2, p3, p4, opts = {}) {
    super();
    this.p1 = p1;
    this.p2 = p2;
    this.p3 = p3;
    this.p4 = p4;
    this.opts = { ...opts };
    this.zIndex = 10;
  }

  evaluate() {
    return this.allComplete([this.p1, this.p2, this.p3, this.p4]);
  }

  update(element) {
    d3.select(element).attr(
      "d",
      `M${this.p1.x} ${this.p1.y} C ${this.p2.x} ${this.p2.y}, ${this.p3.x} ${this.p3.y}, ${this.p4.x} ${this.p4.y}`
    );
  }

  svg(w, h) {
    let line = d3
      .create("svg:path")
      .attr("id", this.id)
      .attr("class", "bezier")
      .attr(
        "d",
        `M${this.p1.x} ${this.p1.y} C ${this.p2.x} ${this.p2.y}, ${this.p3.x} ${this.p3.y}, ${this.p4.x} ${this.p4.y}`
      );
    return line.node();
  }
}

function bezier(...args) {
  return new Bezier(...args);
}

function flip(f) {
  return (a, b) => f(b, a);
}

class Switch extends Shape {
  constructor(operation, i, ...shapes) {
    super();
    this.operation = operation;
    this.i = i;
    this.shapes = shapes;
  }

  evaluate() {
    let e = this.operation.evaluate();
    if (e.complete) {
      let results = this.operation.calculate();
      if (results[this.i]) {
        let complete = true;
        let shapes = [];
        for (let shape of this.shapes) {
          let depend = shape.evaluate();
          complete &&= depend.complete;
          shapes = [...shapes, ...depend.flat];
        }
        return {
          complete: complete,
          flat: [...shapes, ...e.flat],
        };
      }
    }
    return {
      complete: false,
      flat: e.flat,
    };
  }
}

function swtch(...args) {
  return new Switch(...args);
}

class Unfold extends Point {
  constructor(x, y, ...shapes) {
    super();
    this.id = nextId++;
    this.x = x;
    this.y = y;
    this.shapes = shapes;
    this.upto = 0;
    this.size = 20;
    this.zIndex = 2000;
  }

  click() {
    this.upto = (this.upto + 1) % (this.shapes.length + 1);
  }

  evaluate() {
    // return {complete: true, flat: [this]};
    return this.allComplete([...this.shapes.slice(0, this.upto)]);
  }

  update(element) {
    d3.select(element).attr(
      "transform",
      `translate(${this.x},${this.y})rotate(90)`
    );
  }

  svg(w, h) {
    let symbol = d3
      .create("svg:path")
      .attr("id", this.id)
      .attr("class", "play handle")
      .attr(
        "d",
        d3
          .symbol()
          .type(d3.symbolTriangle)
          .size(this.size * this.size)
      )
      .attr("transform", `translate(${this.x},${this.y})rotate(90)`);
    return symbol.node();
  }
}

function unfold(...args) {
  return new Unfold(...args);
}

const offsets = {
  n: { x: 0, y: -1 },
  e: { x: 1, y: 0 },
  s: { x: 0, y: 1 },
  w: { x: -1, y: 0 },
  ne: { x: 1, y: -1 },
  se: { x: 1, y: 1 },
  sw: { x: 1, y: 1 },
  nw: { x: -1, y: -1 },
};

class Label extends Point {
  constructor(p, text, offs = "ne") {
    super();
    this.id = nextId++;
    this.p = p;
    this.text = text;
    this.offs = offs;
    this.zIndex = 1000;
    console.log(this);
  }

  evaluate() {
    let result = this.allComplete([this.p]);
    this.x = this.p.x;
    this.y = this.p.y;
    return result;
  }

  update(element) {
    d3.select(element)
      .attr("x", this.p.x + offsets[this.offs].x * 12)
      .attr("y", this.p.y + offsets[this.offs].y * 12);
  }

  svg(w, h) {
    let symbol = d3
      .create("svg:text")
      .attr("id", this.id)
      .attr("class", "label")
      .attr("x", this.p.x + offsets[this.offs].x * 12)
      .attr("y", this.p.y + offsets[this.offs].y * 12)
      .text(this.text);
    return symbol.node();
  }
}

function label(...args) {
  return new Label(...args);
}

class Group extends Shape {
  constructor(...shapes) {
    super();
    this.shapes = shapes;
  }

  evaluate() {
    let complete = true;
    let shapes = [];
    for (let shape of this.shapes) {
      let depend = shape.evaluate();
      complete &&= depend.complete;
      shapes = [...shapes, ...depend.flat];
    }
    return {
      complete: complete,
      flat: shapes,
    };
  }
}

function group(...args) {
  return new Group(...args);
}

const intersectors = [
  { a: Line, b: Circle, f: isectLineCircle },
  { a: Circle, b: Line, f: flip(isectLineCircle) },
];

function intersector(oa, ob) {
  let is = intersectors.find(
    ({ a, b, _ }) => oa instanceof a && ob instanceof b
  );
  if (is) {
    return is.f;
  } else {
    throw (
      "no intersector for " + a.constructor.name + " and " + b.constructor.name
    );
  }
}

function isectLineCircle(line, circ) {
  let x = line.p1.x - circ.c.x;
  let y = line.p1.y - circ.c.y;
  let dx = line.p1.x - line.p2.x;
  let dy = line.p1.y - line.p2.y;
  let a = dx * dx + dy * dy;
  let b = 2 * (x * dx + y * dy);
  let c = x * x + y * y - circ.r * circ.r;
  let d = b * b - 4 * a * c;
  if (d < 0) {
    return [];
  } else if (d == 0) {
    let t = -b / (2 * a);
    let px = line.p1.x + t * dx;
    let py = line.p1.y + t * dy;
    let nx = ((px - circ.c.x) / circ.r) * defaults.unit;
    let ny = ((py - circ.c.y) / circ.r) * defaults.unit;
    return [{ point: { x: px, y: py }, normal: { x: nx, y: ny } }];
  } else {
    let sqrt = Math.sqrt(d);
    let t1 = (-b - sqrt) / (2 * a);
    let t2 = (-b + sqrt) / (2 * a);
    let p1x = line.p1.x + t1 * dx;
    let p1y = line.p1.y + t1 * dy;
    let p2x = line.p1.x + t2 * dx;
    let p2y = line.p1.y + t2 * dy;
    let n1x = ((p1x - circ.c.x) / circ.r) * defaults.unit;
    let n1y = ((p1y - circ.c.y) / circ.r) * defaults.unit;
    let n2x = ((p2x - circ.c.x) / circ.r) * defaults.unit;
    let n2y = ((p2y - circ.c.y) / circ.r) * defaults.unit;
    return [
      { point: { x: p1x, y: p1y }, normal: { x: n1x, y: n1y } },
      { point: { x: p2x, y: p2y }, normal: { x: n2x, y: n2y } },
    ];
  }
}

class Intersection {
  constructor(a, b) {
    this.id = nextId++;
    this.a = a;
    this.b = b;
    this.isect = intersector(a, b);
  }

  calculate() {
    return this.isect(this.a, this.b);
  }

  evaluate() {
    let ea = this.a.evaluate();
    let eb = this.b.evaluate();
    return {
      complete: ea.complete && eb.complete,
      flat: [...ea.flat, ...eb.flat],
    };
  }
}

function intersect(...args) {
  return new Intersection(...args);
}

class NthP extends Point {
  constructor(operator, i) {
    super(null, null, "computed");
    this.operator = operator;
    this.i = i;
  }

  evaluate() {
    let e = this.operator.evaluate();
    if (e.complete) {
      let results = this.operator.calculate();

      if (results[this.i]) {
        let point = results[this.i].point;
        this.x = point.x;
        this.y = point.y;
        return {
          complete: true,
          flat: [...e.flat, this],
        };
      }
    }
    return {
      complete: false,
      flat: [...e.flat],
    };
  }
}

function nthP(...args) {
  return new NthP(...args);
}

class NthN extends Vector {
  constructor(operator, i) {
    super(point(0, 0, "invisible"), 0, 0);
    this.operator = operator;
    this.i = i;
  }

  evaluate() {
    let e = this.operator.evaluate();
    if (e.complete) {
      let results = this.operator.calculate();

      if (results[this.i]) {
        let p = results[this.i].point;
        let n = results[this.i].normal;
        this.p.x = p.x;
        this.p.y = p.y;
        this.nx = n.x;
        this.ny = n.y;
        return {
          complete: true,
          flat: [...e.flat, this],
        };
      }
    }
    return {
      complete: false,
      flat: [...e.flat],
    };
  }

  evaluate_old() {
    let result = this.operator.calculate();

    let e = this.operator.evaluate();
    if (result[this.i] && e.complete) {
      let p = result[this.i].point;
      let n = result[this.i].normal;
      this.px = p.x;
      this.py = p.y;
      this.nx = n.x;
      this.ny = n.y;
      return {
        complete: e.complete,
        flat: [...e.flat, this],
      };
    } else {
      return {
        complete: e.complete,
        flat: [...e.flat],
      };
    }
  }
}

function nthN(...args) {
  return new NthN(...args);
}

class Mirror extends Point {
  constructor(center, point) {
    super(0, 0, "computed");
    this.center = center;
    this.point = point;
  }

  evaluate() {
    let result = this.allComplete([this.center, this.point]);
    this.x = 2 * this.center.x - this.point.x;
    this.y = 2 * this.center.y - this.point.y;
    return result;
  }
}

function mirror(...args) {
  return new Mirror(...args);
}

class Sum extends Line {
  constructor(base, line, opts = {}) {
    super(base, point(0, 0, "computed"), opts);
    this.line = line;
  }

  evaluate() {
    let result = super.evaluate();
    this.p2.x = this.p1.x + this.line.dx;
    this.p2.y = this.p1.y + this.line.dy;
    return result;
  }
}

function sum(...args) {
  return new Sum(...args);
}

class Project extends Point {
  constructor(base, tip, point) {
    super(0, 0, "computed");
    this.base = base;
    this.tip = tip;
    this.point = point;
  }

  evaluate() {
    let result = this.allComplete([this.base, this.tip, this.point]);
    let tdx = this.tip.x - this.base.x;
    let tdy = this.tip.y - this.base.y;
    let tdl = Math.sqrt(tdx * tdx + tdy * tdy);
    let pdx = this.point.x - this.base.x;
    let pdy = this.point.y - this.base.y;
    let tdxn = tdx / tdl;
    let tdyn = tdy / tdl;
    let pd = pdx * tdxn + pdy * tdyn;
    this.x = this.base.x + pd * tdxn;
    this.y = this.base.y + pd * tdyn;
    return result;
  }
}

function project(...args) {
  return new Project(...args);
}
// Makes array elements unique by id as a key by way of built-in object
// attribute hashing.
function unique(array) {
  let u = Object.values(
    array.reduce((a, e) => {
      a[e.id] = e;
      return a;
    }, {})
  ).sort((a, b) => a - b);
  return u;
}

function flatten(root) {
  let shapes = unique(root.evaluate().flat).sort((a, b) => a.zIndex - b.zIndex);
  return shapes;
}

function clip(max, v) {
  return Math.min(Math.max(0, v), max);
}

// Only add the SVG definitions block to our first SVG element. Otherwise chaos
// ensues.
var defsAdded = false;

function addDefs(svg) {
  if (defsAdded) return;
  defsAdded = true;

  let defs = svg.append("defs");

  const mw = defaults.arrow.w;
  const mh = defaults.arrow.h;
  const arrowPoints = [
    [0, 0],
    [0, mh],
    [mw, mh / 2],
  ];

  defs
    .append("marker")
    .attr("id", "arrow")
    .attr("refX", mw)
    .attr("refY", mh / 2)
    .attr("markerWidth", mw + 1)
    .attr("markerHeight", mh + 1)
    .attr("orient", "auto")
    .append("path")
    .attr("d", d3.line()(arrowPoints));
  defs
    .append("marker")
    .attr("id", "vec-arrow")
    .attr("refX", mw / 2)
    .attr("refY", mh / 2)
    .attr("markerWidth", mw + 1)
    .attr("markerHeight", mh + 1)
    .attr("orient", "auto")
    .append("path")
    .attr("d", d3.line()(arrowPoints));
}

function update(svg, width, height, root) {
  // Updates all elements that are direct children and have an id
  // attribute.
  svg
    .selectChildren("*[id]")
    .data(flatten(root), (d) => d.id)
    .join(
      (enter) =>
        enter.append((d) => {
          return d.svg(width, height);
        }),
      // Funny thing is, this needs to use the anonymous function
      // syntax to work. Lambdas do not seem to bind 'this'.
      (update) =>
        update.each(function (d) {
          d.update(this);
        }),
      (exit) => exit.remove()
    );
}

function renderSvg(element, width, height, root) {
  let svg = d3
    .select(element)
    .append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("class", "geometry")
    .on("mousemove", function (e) {
      // console.log("event", e.offsetX, e.offsetY); // log the mouse x,y position
      // console.log("event", e.clientX, e.clientY); // log the mouse x,y position
      // console.log("client", this.clientWidth, this.clientHeight);
      // console.log("client", this.clientLeft, this.clientTop);
      // console.log("bounding", this.getBoundingClientRect());
      // console.log("viewbox", this.viewBox.baseVal.width, this.viewBox.baseVal.height);
    });

  addDefs(svg);

  let clientToBoxX, clientToBoxY;
  let svgElement = svg.node();

  let drag = d3
    .drag()
    .on("start", function (event) {
      clientToBoxX = svgElement.viewBox.baseVal.width / svgElement.clientWidth;
      clientToBoxY =
        svgElement.viewBox.baseVal.height / svgElement.clientHeight;
    })
    .on("drag", function (event) {
      let clientX = event.sourceEvent.offsetX;
      let clientY = event.sourceEvent.offsetY;
      let x = clip(svgElement.clientWidth, clientX) * clientToBoxX;
      let y = clip(svgElement.clientHeight, clientY) * clientToBoxY;
      event.subject.move(x, y);

      update(svg, width, height, root);
    });

  svg
    .selectChildren("*[id]")
    .data(flatten(root), (d) => d.id)
    .enter()
    .append((d) => {
      return d.svg(width, height);
    });

  svg.selectChildren("*[id].handle").call(drag);
  svg.selectChildren("*[id].play").on("click", clicked);

  function clicked(event, d) {
    if (event.defaultPrevented) return; // dragged
    d.click();
    update(svg, width, height, root);
  }
}
