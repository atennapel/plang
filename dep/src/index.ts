// External
abstract class Term {
  abstract toString(): string;
  abstract toInternal(map?: { [key: string]: {index: number, name: string} }): ITerm;
}

class Var extends Term {
  readonly name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return map[this.name]? ibound(map[this.name].index): ifree(this.name);
  }
}
function vr(name: string) {
  return new Var(name);
}

class Abs extends Term {
  readonly arg: string;
  readonly term: Term;

  constructor(arg: string, term: Term) {
    super();
    this.arg = arg;
    this.term = term;
  }

  toString() {
    return `(\\${this.arg}.${this.term})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    const n: { [key: string]: {index: number, name: string} } = {};
    for(let k in map) n[k] = { index: map[k].index + 1, name: map[k].name };
    n[this.arg] = { index: 0, name: this.arg };
    return iabs(this.term.toInternal(n));
  }
}
function abs(args: string[], term: Term) {
  return args.reduceRight((x, y) => new Abs(y, x), term);
}

class App extends Term {
  readonly left: Term;
  readonly right: Term;

  constructor(left: Term, right: Term) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return iapp(this.left.toInternal(map), this.right.toInternal(map));
  }
}
function app(...ts: Term[]) {
  return ts.reduce((x, y) => new App(x, y));
}

// Internal
let freshI = 0;
function fresh(): string { return `\$${freshI++}` }

abstract class ITerm {
  abstract toString(): string;

  abstract open(e: ITerm, k?: number): ITerm;
  abstract close(x: string, k?: number): ITerm;

  abstract normalize(): ITerm;

  openVar(name: string, k: number = 0): ITerm {
    return this.open(ifree(name), k);
  }
  subst(x: string, e: ITerm): ITerm {
    return this.close(x).open(e);
  }
}

class IFree extends ITerm {
  readonly name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  toString(): string {
    return this.name;
  }

  open(e: ITerm, k: number = 0): ITerm {
    return this;
  }
  close(x: string, k: number = 0): ITerm {
    return this.name === x? ibound(k): this;
  }

  normalize(): ITerm {
    return this;
  }
}
function ifree(name: string) {
  return new IFree(name);
}

class IBound extends ITerm {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString(): string {
    return `'${this.index}`;
  }

  open(e: ITerm, k: number = 0): ITerm {
    return this.index === k? e: this;
  }
  close(x: string, k: number = 0): ITerm {
    return this;
  }

  normalize(): ITerm {
    return this;
  }
}
function ibound(index: number) {
  return new IBound(index);
}

class IAbs extends ITerm {
  readonly term: ITerm;

  constructor(term: ITerm) {
    super();
    this.term = term;
  }

  toString() {
    return `(\\${this.term})`;
  }

  open(e: ITerm, k: number = 0): ITerm {
    return iabs(this.term.open(e, k + 1));
  }
  close(x: string, k: number = 0): ITerm {
    return iabs(this.term.close(x, k + 1));
  }

  normalize(): ITerm {
    const x = fresh();
    return iabs(this.term.openVar(x).normalize().close(x));
  }
}
function iabs(term: ITerm) {
  return new IAbs(term);
}

class IApp extends ITerm {
  readonly left: ITerm;
  readonly right: ITerm;

  constructor(left: ITerm, right: ITerm) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  open(e: ITerm, k: number = 0): ITerm {
    return iapp(
      this.left.open(e, k),
      this.right.open(e, k)
    );
  }
  close(x: string, k: number = 0): ITerm {
    return iapp(
      this.left.close(x, k),
      this.right.close(x, k)
    );
  }

  normalize(): ITerm {
    const l = this.left.normalize();
    const r = this.right.normalize();
    if(l instanceof IAbs)
      return l.term.open(r).normalize();
    return iapp(l, r);
  }
}
function iapp(...ts: ITerm[]) {
  return ts.reduce((x, y) => new IApp(x, y));
}

// testing
const V = vr;
const L = abs;
const A = app;

const e = A(L(['x', 'f'], A(V('f'), V('x'))), L(['x'], V('x')));
console.log('' + e);
console.log('' + e.toInternal().normalize());

