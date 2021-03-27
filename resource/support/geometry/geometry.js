export {
  point,
  label,
  mlabel,
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
  renderSvg,
  IsectLineCircle,
};

import "/support/vendor/d3.v6.min.js";
import { Clipper } from "/support/js/clip.js";

const defaults = {
  point: { r: 8, opts: [] }, // one of [, "drag", "computed"]
  line: {
    opts: [],
  },
  vector: {
    opts: ["arrow"],
  },
  circle: {
    opts: [],
  },
  surface: {
    opts: [],
  },
  project: {
    opts: ["line"],
  },
  unit: 60,
  arrow: { w: 5, h: 4 },
};

let nextId = 0;

let slides = document.querySelector(".reveal .slides");

class Shape {
  constructor(z = 1) {
    this.complete = true;
    this.id = nextId++;
    this.zIndex = z;
    this.parent = null;
  }

  evaluate() {
    // Make sure to evaluate a possible parent.
    if (this.parent) this.parent.evaluate();
    return this.complete;
  }

  static all(...dependencies) {
    return dependencies.reduce((a, s) => a && s.evaluate(), true);
  }

  // Include the parents flat rep here, because it might not be in the tree
  // already, for example a computing node where only the results are part of
  // the tree.
  flat() {
    let pflat = this.parent ? this.parent.flat() : [];
    return this.complete ? [this, ...pflat] : pflat;
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
    let classes = this.opts.concat("point").join(" ");
    let g = d3
      .create("svg:g")
      .attr("id", this.id)
      .attr("transform", `translate(${this.x},${this.y})`);
    let v = d3.create("svg:circle").attr("r", this.r);

    if (this.opts.includes("drag")) {
      g.attr("class", "handle");
      g.append(() => v.node()).attr("class", classes);
      g.append("svg:circle")
        .attr("class", "handle")
        .attr("r", this.r * 2);
    } else if (this.opts.includes("computed")) {
      g.append(() => v.node()).attr("class", classes);
    } else if (this.opts.includes("hidden")) {
      g.attr("class", "handle");
      g.append("svg:circle")
        .attr("class", "handle")
        .attr("r", this.r * 2);
    } else if (this.opts.includes("invisible")) {
    } else {
      g.append(() => v.node()).attr("class", classes);
    }
    return g.node();
  }
}

function point(...args) {
  return new Point(...args);
}

class Text extends Shape {
  constructor(x, y, text, ...opts) {
    super();
    this.x = x;
    this.y = y;
    this.text = text;
    this.opts = opts.length == 0 ? defaults.text.opts : opts;
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
  constructor(p1, p2, ...opts) {
    super();
    this.p1 = p1;
    this.p2 = p2;
    this.opts = opts.length == 0 ? defaults.line.opts : opts;
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
    this.complete = Shape.all(this.p1, this.p2);
    return this.complete;
  }

  flat() {
    return [
      ...this.p1.flat(),
      ...this.p2.flat(),
      ...(this.complete ? [this] : []),
    ];
  }

  update(element) {
    if (this.c) {
      let [n, xp1, xp2] = this.c.clipLine(this.p1, this.p2);
      d3.select(element)
        .attr("x1", xp1.x)
        .attr("y1", xp1.y)
        .attr("x2", xp2.x)
        .attr("y2", xp2.y);
    } else {
      d3.select(element)
        .attr("x1", this.p1.x)
        .attr("y1", this.p1.y)
        .attr("x2", this.p2.x)
        .attr("y2", this.p2.y);
    }
  }

  svg(w, h) {
    let classes = this.opts.concat("line").join(" ");
    let line = d3.create("svg:line").attr("id", this.id).attr("class", classes);
    if (this.opts.includes("infinite")) {
      this.c = new Clipper({ x: 0, y: 0 }, { x: w, y: h });
      let [n, xp1, xp2] = this.c.clipLine(this.p1, this.p2);
      line
        .attr("x1", xp1.x)
        .attr("y1", xp1.y)
        .attr("x2", xp2.x)
        .attr("y2", xp2.y);
    } else {
      line
        .attr("x1", this.p1.x)
        .attr("y1", this.p1.y)
        .attr("x2", this.p2.x)
        .attr("y2", this.p2.y);
      if (this.opts.includes("arrow")) {
        line.attr("marker-end", `url(#arrow)`);
      }
    }
    return line.node();
  }
}

function line(...args) {
  return new Line(...args);
}

class Surface extends Shape {
  constructor(p, w, ...opts) {
    super();
    this.p = p;
    this.w = w;
    this.opts = opts.length == 0 ? defaults.surface.opts : opts;
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

  evaluate() {
    return (this.complete = this.p.evaluate());
  }

  flat() {
    return [...this.p.flat(), ...(this.complete ? [this] : [])];
  }
}

function surface(...args) {
  return new Surface(...args);
}

class Vector extends Shape {
  constructor(p, nx, ny, ...opts) {
    super();
    this.p = p;
    this._p = null;
    this.nx = nx;
    this.ny = ny;
    this.opts = opts.length == 0 ? defaults.vector.opts : opts;
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
    this.complete = this.p.evaluate();
    if (this._p) {
      this._p.x = this.p.x + this.nx;
      this._p.y = this.p.y + this.ny;
    }
    return this.complete;
  }

  flat() {
    return [...this.p.flat(), ...(this.complete ? [this] : [])];
  }

  addP2(line) {
    if (this.opts.includes("arrow")) {
      let dscr = this.nx * this.nx + this.ny * this.ny;
      if (dscr > 0) {
        let l = Math.sqrt(dscr);
        let nx = (this.nx / l) * (l - defaults.arrow.w);
        let ny = (this.ny / l) * (l - defaults.arrow.w);
        line.attr("marker-end", "url(#vec-arrow)");
        line.attr("x2", this.p.x + nx).attr("y2", this.p.y + ny);
      } else {
        line.attr("x2", this.p.x).attr("y2", this.p.y);
      }
    } else {
      line.attr("x2", this.p.x + this.nx).attr("y2", this.p.y + this.ny);
    }
  }

  update(element) {
    let line = d3.select(element).attr("x1", this.p.x).attr("y1", this.p.y);
    this.addP2(line);
  }

  svg(w, h) {
    let classes = this.opts.concat("vector").join(" ");
    let line = d3
      .create("svg:line")
      .attr("id", this.id)
      .attr("class", classes)
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
  constructor(c, r, ...opts) {
    super();
    this.c = c;
    this.r = r;
    this.opts = opts.length == 0 ? defaults.circle.opts : opts;
  }

  evaluate() {
    return (this.complete = this.c.evaluate());
  }

  flat() {
    return [...this.c.flat(), ...(this.complete ? [this] : [])];
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
  constructor(p1, p2, p3, p4, ...opts) {
    super();
    this.p1 = p1;
    this.p2 = p2;
    this.p3 = p3;
    this.p4 = p4;
    this.opts = opts.length == 0 ? defaults.point.opts : opts;
    this.zIndex = 10;
  }

  evaluate() {
    return (this.complete = Shape.all(this.p1, this.p2, this.p3, this.p4));
  }

  flat() {
    return [
      ...this.p1.flat(),
      ...this.p2.flat(),
      ...this.p3.flat(),
      ...this.p4.flat(),
      ...(this.complete ? [this] : []),
    ];
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
  constructor(cond, ...shapes) {
    super();
    this.cond = cond;
    this.shapes = shapes;
  }

  evaluate() {
    this.shapes.map((s) => s.evaluate());
    let c = this.cond.evaluate();
    return (this.complete = c);
  }

  flat() {
    let parts = this.shapes.reduce((a, s) => [...a, ...s.flat()], []);
    return this.complete ? parts : [];
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
    Shape.all(...this.shapes.slice(0, this.upto));
    return true;
  }

  flat() {
    let show = this.shapes
      .slice(0, this.upto)
      .reduce((a, s) => [...a, ...s.flat()], []);
    return [this, ...show];
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
  constructor(p, text, dir = "ne") {
    super();
    this.id = nextId++;
    this.p = p;
    this.text = text;
    this.dir = dir;
    this.zIndex = 1000;
    this.svgW = 1.5 * this.text.length;
    this.svgH = 1.5;
    this.f = 15;
    this.o = 20;
  }

  evaluate() {
    this.complete = this.p.evaluate();
    this.x = this.p.x;
    this.y = this.p.y;
    return this.complete;
  }

  flat() {
    return [...this.p.flat(), ...(this.complete ? [this] : [])];
  }

  offset() {
    switch (this.dir) {
      case "n":
        return {
          x: -(this.svgW / 2) * this.f,
          y: -this.o,
        };
      case "ne":
        return { x: this.o * 0.81, y: -this.o * 0.81 };
      case "e":
        return { x: this.o, y: +(this.svgH / 2) * this.f };
      case "se":
        return { x: this.o * 0.81, y: (this.o + this.svgH * this.f) * 0.81 };
      case "s":
        return { x: -(this.svgW / 2) * this.f, y: this.o + this.svgH * this.f };
      case "sw":
        return {
          x: (-this.o - this.svgW * this.f) * 0.81,
          y: (this.o + this.svgH * this.f) * 0.81,
        };
      case "w":
        return {
          x: -this.o - this.svgW * this.f,
          y: +(this.svgH / 2) * this.f,
        };
      case "nw":
        return {
          x: (-this.o - this.svgW * this.f) * 0.81,
          y: -this.o * 0.81,
        };
      default:
        return { x: this.o, y: -this.o - this.svgH * this.f };
    }
  }

  update(element) {
    let o = this.offset();
    d3.select(element).attr(
      "transform",
      `translate(${this.p.x + o.x},${this.p.y + o.y})`
    );
  }

  svg(w, h) {
    let o = this.offset();
    let g = d3
      .create("svg:g")
      .attr("class", "label")
      .attr("id", this.id)
      .attr("transform", `translate(${this.p.x + o.x},${this.p.y + o.y})`);
    g.append("svg:text")
      .attr("id", this.id)
      .attr("class", "label")
      .attr("x", 0)
      .attr("y", 0)
      .text(this.text);
    return g.node();
  }
}

function label(...args) {
  return new Label(...args);
}

class MathLabel extends Point {
  constructor(p, text, dir = "ne") {
    super();
    this.id = nextId++;
    this.p = p;
    this.text = text;
    this.dir = dir;
    this.zIndex = 1000;
    try {
      this.label = MathJax.tex2svg(text).querySelector("svg");
      this.svgW = this.label.width.baseVal.valueInSpecifiedUnits;
      this.svgH = this.label.height.baseVal.valueInSpecifiedUnits;
    } catch (e) {
      this.label = d3.create("text").text(this.text).node();
      console.log("MathLabel: " + this.text + "" + e);
    }
    this.f = 15;
    this.o = 20;
  }

  evaluate() {
    this.complete = this.p.evaluate();
    this.x = this.p.x;
    this.y = this.p.y;
    return this.complete;
  }

  flat() {
    return [...this.p.flat(), ...(this.complete ? [this] : [])];
  }

  offset() {
    switch (this.dir) {
      case "n":
        return {
          x: -(this.svgW / 2) * this.f,
          y: -this.o - this.svgH * this.f,
        };
      case "ne":
        return { x: this.o * 0.81, y: (-this.o - this.svgH * this.f) * 0.81 };
      case "e":
        return { x: this.o, y: -(this.svgH / 2) * this.f };
      case "se":
        return { x: this.o * 0.81, y: this.o * 0.81 };
      case "s":
        return { x: -(this.svgW / 2) * this.f, y: this.o };
      case "sw":
        return { x: (-this.o - this.svgW * this.f) * 0.81, y: this.o * 0.81 };
      case "w":
        return {
          x: -this.o - this.svgW * this.f,
          y: -(this.svgH / 2) * this.f,
        };
      case "nw":
        return {
          x: (-this.o - this.svgW * this.f) * 0.81,
          y: -this.o - this.svgH * this.f * 0.81,
        };
      default:
        return { x: this.o, y: -this.o - this.svgH * this.f };
    }
  }

  update(element) {
    let o = this.offset();
    d3.select(element).attr(
      "transform",
      `translate(${this.p.x + o.x},${this.p.y + o.y})`
    );
  }

  svg(w, h) {
    let o = this.offset();
    let g = d3
      .create("svg:g")
      .attr("class", "label")
      .attr("id", this.id)
      .attr("transform", `translate(${this.p.x + o.x},${this.p.y + o.y})`);
    if (this.label) g.append(() => this.label);
    return g.node();
  }
}

function mlabel(...args) {
  return new MathLabel(...args);
}

class Group extends Shape {
  constructor(...shapes) {
    super();
    this.shapes = shapes;
  }

  evaluate() {
    this.shapes.map((s) => s.evaluate());
    return true;
  }

  flat() {
    return this.shapes.reduce((a, s) => [...a, ...s.flat()], []);
  }
}

function group(...args) {
  return new Group(...args);
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
    return (this.complete = Shape.all(this.a, this.b));
  }

  flat() {
    return [...this.a.flat(), ...this.b.flat()];
  }
}

function intersect(...args) {
  return new Intersection(...args);
}

class Calculated {
  constructor(...dependencies) {
    this.dependencies = dependencies;
    for (let shape of this.dependencies) {
      shape.complete = false;
    }
  }

  calculate(complete) {
    if (complete) {
      // Set all possible results to incomplete
    }
    // Set all actual results to complete
  }

  evaluate() {
    this.complete = Shape.all(...this.dependencies);
    this.calculate(this.complete);
    return this.complete;
  }

  flat() {
    // Really only return the dependencies. The result are flattened from the
    // outside.
    return this.dependencies.reduce((a, s) => [...a, ...s.flat()], []);
  }
}

class IsectLineCircle extends Calculated {
  constructor(line, circ) {
    super(line, circ);
    this.line = line;
    this.circ = circ;
    this.p1 = point(0, 0, "computed");
    this.p1.parent = this;
    this.n1 = vector(point(0, 0, "invisible"), 0, 0, "computed", "arrow");
    this.n1.parent = this;
    this.p2 = point(0, 0, "computed");
    this.p2.parent = this;
    this.n2 = vector(point(0, 0, "invisible"), 0, 0, "computed", "arrow");
    this.n2.parent = this;
  }

  calculate(complete) {
    let x = this.line.p1.x - this.circ.c.x;
    let y = this.line.p1.y - this.circ.c.y;
    let dx = this.line.p1.x - this.line.p2.x;
    let dy = this.line.p1.y - this.line.p2.y;
    let a = dx * dx + dy * dy;
    let b = 2 * (x * dx + y * dy);
    let c = x * x + y * y - this.circ.r * this.circ.r;
    let d = b * b - 4 * a * c;

    this.p1.complete = false;
    this.n1.complete = false;
    this.p2.complete = false;
    this.n2.complete = false;

    if (complete && d >= 0) {
      let sqrt = Math.sqrt(d);
      let t1 = (-b - sqrt) / (2 * a);
      let t2 = (-b + sqrt) / (2 * a);

      // if (0 <= Math.abs(t1) && Math.abs(t1) <= 1) {
      let p1x = this.line.p1.x + t1 * dx;
      let p1y = this.line.p1.y + t1 * dy;
      let n1x = ((p1x - this.circ.c.x) / this.circ.r) * defaults.unit;
      let n1y = ((p1y - this.circ.c.y) / this.circ.r) * defaults.unit;
      this.p1.complete = true;
      this.p1.move(p1x, p1y);
      this.n1.complete = true;
      this.n1.p.move(p1x, p1y);
      this.n1.nx = n1x;
      this.n1.ny = n1y;
      // }

      // if (0 <= Math.abs(t2) && Math.abs(t2) <= 1) {
      let p2x = this.line.p1.x + t2 * dx;
      let p2y = this.line.p1.y + t2 * dy;
      let n2x = ((p2x - this.circ.c.x) / this.circ.r) * defaults.unit;
      let n2y = ((p2y - this.circ.c.y) / this.circ.r) * defaults.unit;
      this.p2.complete = true;
      this.p2.move(p2x, p2y);
      this.n2.complete = true;
      this.n2.p.move(p2x, p2y);
      this.n2.nx = n2x;
      this.n2.ny = n2y;
      // }
    }
  }
}

class Mirror extends Point {
  constructor(center, point) {
    super(0, 0, "computed");
    this.center = center;
    this.point = point;
  }

  evaluate() {
    this.complete = Shape.all(this.center, this.point);
    this.x = 2 * this.center.x - this.point.x;
    this.y = 2 * this.center.y - this.point.y;
    return this.complete;
  }

  flat() {
    return [
      ...this.point.flat(),
      ...this.center.flat(),
      ...(this.complete ? [this] : []),
    ];
  }
}

function mirror(...args) {
  return new Mirror(...args);
}

class Sum extends Line {
  constructor(base, line, ...opts) {
    super(base, point(0, 0, "computed"), ...opts);
    this.line = line;
  }

  evaluate() {
    this.complete = super.evaluate();
    this.p2.x = this.p1.x + this.line.dx;
    this.p2.y = this.p1.y + this.line.dy;
    return this.complete;
  }

  flat() {
    return super.flat();
  }
}

function sum(...args) {
  return new Sum(...args);
}

class Project extends Point {
  constructor(base, tip, point, ...opts) {
    super(0, 0, "computed");
    this.base = base;
    this.tip = tip;
    this.point = point;
    this.opts = opts.length == 0 ? defaults.point.opts : opts;
  }

  evaluate() {
    this.complete = Shape.all(this.base, this.tip, this.point);
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
    return this.complete;
  }

  flat() {
    return [
      ...this.base.flat(),
      ...this.tip.flat(),
      ...this.point.flat(),
      ...(this.complete ? [this] : []),
    ];
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
  root.evaluate();
  let shapes = unique(root.flat()).sort((a, b) => a.zIndex - b.zIndex);
  return shapes;
}

function clip(max, v) {
  return Math.min(Math.max(0, v), max);
}

// Only add the SVG definitions block to our first SVG element. Otherwise chaos
// ensues.
var svgDefsElement = false;

function addDefs(svg) {
  if (svgDefsElement) return;

  let defs = svg.append("defs");

  svgDefsElement = defs.node();

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
    .attr("markerWidth", mw)
    .attr("markerHeight", mh)
    .attr("markerUnits", "strokeWidth")
    .attr("orient", "auto")
    .append("path")
    .attr("d", d3.line()(arrowPoints));
  defs
    .append("marker")
    .attr("id", "vec-arrow")
    .attr("refX", mw / 2)
    .attr("refY", mh / 2)
    .attr("markerWidth", mw)
    .attr("markerHeight", mh)
    .attr("markerUnits", "strokeWidth")
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
  let slideZoom = slides.style.zoom || 1;

  let drag = d3
    .drag()
    .on("start", function (event) {
      slideZoom = slides.style.zoom || 1;
      clientToBoxX = svgElement.viewBox.baseVal.width / svgElement.clientWidth;
      clientToBoxY =
        svgElement.viewBox.baseVal.height / svgElement.clientHeight;
    })
    .on("drag", function (event) {
      let clientX = event.sourceEvent.offsetX / slideZoom;
      let clientY = event.sourceEvent.offsetY / slideZoom;
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
