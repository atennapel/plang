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

class Uni extends Term {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return `U${this.index}`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return iuni(this.index);
  }
}
function uni(index: number) {
  return new Uni(index);
}

class Abs extends Term {
  readonly arg: string;
  readonly type: Term;
  readonly term: Term;

  constructor(arg: string, type: Term, term: Term) {
    super();
    this.arg = arg;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(\\${this.arg}:${this.type}.${this.term})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    const n: { [key: string]: {index: number, name: string} } = {};
    for(let k in map) n[k] = { index: map[k].index + 1, name: map[k].name };
    n[this.arg] = { index: 0, name: this.arg };
    return iabs([[this.arg, this.type.toInternal(n)]], this.term.toInternal(n));
  }
}
function abs(args: [string, Term][], term: Term) {
  return args.reduceRight((x, [n, t]) => new Abs(n, t, x), term);
}

class Pi extends Term {
  readonly arg: string;
  readonly type: Term;
  readonly term: Term;

  constructor(arg: string, type: Term, term: Term) {
    super();
    this.arg = arg;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(${this.arg}:${this.type} -> ${this.term})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    const n: { [key: string]: {index: number, name: string} } = {};
    for(let k in map) n[k] = { index: map[k].index + 1, name: map[k].name };
    n[this.arg] = { index: 0, name: this.arg };
    return ipi([[this.arg, this.type.toInternal(n)]], this.term.toInternal(n));
  }
}
function pi(args: [string, Term][], term: Term) {
  return args.reduceRight((x, [n, t]) => new Pi(n, t, x), term);
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
  abstract toNamed(): Term;

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
  toNamed(): Term {
    return vr(this.name);
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
  toNamed(): Term {
    throw new Error(`Bound variable ${this.index} encountered in toNamed`);
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

class IUni extends ITerm {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString(): string {
    return `U${this.index}`;
  }
  toNamed(): Term {
    return uni(this.index);
  }

  open(e: ITerm, k: number = 0): ITerm {
    return this;
  }
  close(x: string, k: number = 0): ITerm {
    return this;
  }

  normalize(): ITerm {
    return this;
  }
}
function iuni(index: number) {
  return new IUni(index);
}

class IAbs extends ITerm {
  readonly argname: string;
  readonly type: ITerm;
  readonly term: ITerm;

  constructor(argname: string, type: ITerm, term: ITerm) {
    super();
    this.argname = argname;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(\\:${this.type}.${this.term})`;
  }
  toNamed(): Term {
    return abs(
      [[this.argname, this.type.open(ifree(this.argname)).toNamed()]],
      this.term.open(ifree(this.argname)).toNamed()
    );
  }

  open(e: ITerm, k: number = 0): ITerm {
    return iabs([[this.argname, this.type.open(e, k + 1)]], this.term.open(e, k + 1));
  }
  close(x: string, k: number = 0): ITerm {
    return iabs([[this.argname, this.type.close(x, k + 1)]], this.term.close(x, k + 1));
  }

  normalize(): ITerm {
    const x = fresh();
    return iabs(
      [[this.argname, this.type.openVar(x).normalize().close(x)]],
      this.term.openVar(x).normalize().close(x)
    );
  }
}
function iabs(args: [string, ITerm][], term: ITerm) {
  return args.reduceRight((x, [n, t]) => new IAbs(n, t, x), term);
}

class IPi extends ITerm {
  readonly argname: string;
  readonly type: ITerm;
  readonly term: ITerm;

  constructor(argname: string, type: ITerm, term: ITerm) {
    super();
    this.argname = argname;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(:${this.type} -> ${this.term})`;
  }
  toNamed(): Term {
    return pi(
      [[this.argname, this.type.open(ifree(this.argname)).toNamed()]],
      this.term.open(ifree(this.argname)).toNamed()
    );
  }

  open(e: ITerm, k: number = 0): ITerm {
    return ipi([[this.argname, this.type.open(e, k + 1)]], this.term.open(e, k + 1));
  }
  close(x: string, k: number = 0): ITerm {
    return ipi([[this.argname, this.type.close(x, k + 1)]], this.term.close(x, k + 1));
  }

  normalize(): ITerm {
    const x = fresh();
    return ipi(
      [[this.argname, this.type.openVar(x).normalize().close(x)]],
      this.term.openVar(x).normalize().close(x)
    );
  }
}
function ipi(args: [string, ITerm][], term: ITerm) {
  return args.reduceRight((x, [n, t]) => new IPi(n, t, x), term);
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
  toNamed(): Term {
    return new App(this.left.toNamed(), this.right.toNamed());
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
const U = uni;
const L = abs;
const P = pi;
const A = app;

const e = A(L([['t', U(0)]], L([['x', V('t')]], V('x'))), V('Int'));
console.log('' + e);
const i = e.toInternal();
console.log('' + i)
const n = i.normalize();
console.log('' + n);
console.log('' + n.toNamed());
