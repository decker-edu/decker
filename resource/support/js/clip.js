// A fast Skala line clipper https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.7789&rep=rep1&type=pdf
// Adapted from https://github.com/oberbichler/skalaclip-js

export { Clipper };

function Point(x, y) {
  return { x, y };
}

function Vector(x, y, z) {
  return { x, y, z };
}

function pointCross(a, b) {
  return Vector(a.y - b.y, b.x - a.x, a.x * b.y - a.y * b.x);
}

function cross(u, v) {
  const z = u.x * v.y - u.y * v.x;
  return Point((u.y * v.z - u.z * v.y) / z, (u.z * v.x - u.x * v.z) / z);
}

function dot(u, v) {
  return u.x * v.x + u.y * v.y + u.z * v.z;
}

const mask = [0, 1, 2, 2, 4, 0, 4, 4, 8, 1, 0, 2, 8, 1, 8, 0];
const tab1 = [4, 3, 0, 3, 1, 4, 0, 3, 2, 2, 4, 2, 1, 1, 0, 4];
const tab2 = [4, 0, 1, 1, 2, 4, 2, 2, 3, 0, 4, 1, 3, 0, 3, 4];

class Clipper {
  constructor(a, b) {
    if (a.x < b.x) {
      this.x_min = a.x;
      this.x_max = b.x;
    } else {
      this.x_min = b.x;
      this.x_max = a.x;
    }

    if (a.y < b.y) {
      this.y_min = a.y;
      this.y_max = b.y;
    } else {
      this.y_min = b.y;
      this.y_max = a.y;
    }

    this.x = [
      Vector(this.x_min, this.y_min, 1),
      Vector(this.x_max, this.y_min, 1),
      Vector(this.x_max, this.y_max, 1),
      Vector(this.x_min, this.y_max, 1),
    ];

    this.e = [
      Vector(0, 1, -this.y_min),
      Vector(1, 0, -this.x_max),
      Vector(0, 1, -this.y_max),
      Vector(1, 0, -this.x_min),
    ];
  }

  code(pt) {
    let c = 0;

    if (pt.x < this.x_min) {
      c = 8;
    } else if (pt.x > this.x_max) {
      c = 2;
    }

    if (pt.y < this.y_min) {
      c |= 1;
    } else if (pt.y > this.y_max) {
      c |= 4;
    }

    return c;
  }

  clipLine(xA, xB) {
    const cA = this.code(xA);
    const cB = this.code(xB);

    // if ((cA | cB) === 0) {
    //   return [0, xA, xB];
    // }

    if ((cA & cB) !== 0) {
      return [-1, xA, xB];
    }

    const p = pointCross(xA, xB);

    let c = 0;

    for (let k = 0; k < 4; k += 1) {
      if (dot(p, this.x[k]) <= 0) {
        c |= 1 << k;
      }
    }

    if (c === 0 || c === 15) {
      return [-1, xA, xB];
    }

    const i = tab1[c];
    const j = tab2[c];

    if (true || cA !== 0 && cB !== 0) {
      const newXA = cross(p, this.e[i]);
      const newXB = cross(p, this.e[j]);
      return [3, newXA, newXB];
    }

    if (cA === 0) {
      let newXB = null;

      if ((cB & mask[c]) === 0) {
        newXB = cross(p, this.e[i]);
      } else {
        newXB = cross(p, this.e[j]);
      }
      return [2, xA, newXB];
    }

    if (cB === 0) {
      let newXA = null;

      if ((cA & mask[c]) === 0) {
        newXA = cross(p, this.e[i]);
      } else {
        newXA = cross(p, this.e[j]);
      }
      return [1, newXA, xB];
    }
  }
}
