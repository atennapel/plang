export abstract class Type {
  abstract toString(): string;
  abstract equals(other: Type): boolean;
}

export class TCon extends Type {
  readonly name: string;
  
  constructor(name: string) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }

  equals(other: Type): boolean {
    return other instanceof TCon && this.name === other.name;
  }
}
export function tcon(name: string) {
  return new TCon(name);
}

export class TFun extends Type {
  readonly left: Type;
  readonly right: Type;
  
  constructor(left: Type, right: Type) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} -> ${this.right})`;
  }

  equals(other: Type): boolean {
    return other instanceof TFun && this.left.equals(other.left) && this.right.equals(other.right);
  }
}
export function tfun(left: Type, right: Type) {
  return new TFun(left, right);
}

type Env = { [key: number]: Type };
function extend(env: Env, id: number, type: Type) {
  return { ...env, [id]: type };
}

export abstract class Expr {
  abstract toString(): string;
  abstract equals(other: Expr): boolean;
  abstract subst(index: number, expr: Expr): Expr;
  abstract adjust(by: number): Expr;
  abstract betareduce(): Expr | null;
  abstract infer(id: number, env: Env): Type | null;

  betaequals(other: Expr): boolean {
    return (this.betareduce() || this).equals(other.betareduce() || other);
  }
}

export class EVar extends Expr {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return this.index.toString();
  }

  equals(other: Expr): boolean {
    return other instanceof EVar && this.index === other.index;
  }

  subst(index: number, expr: Expr): Expr {
    return this.index === index? expr: this;
  }

  adjust(by: number): Expr {
    return new EVar(this.index + by);
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(id: number, env: Env): Type | null {
    return env[this.index] || null;
  }
}
export function evar(index: number) {
  return new EVar(index);
}

export class EAbs extends Expr {
  readonly type: Type;
  readonly expr: Expr;

  constructor(type: Type, expr: Expr) {
    super();
    this.type = type;
    this.expr = expr;
  }

  toString() {
    return `\\${this.type}.${this.expr}`;
  }

  equals(other: Expr): boolean {
    return other instanceof EAbs && this.type.equals(other.type) && this.expr.equals(other.expr);
  }

  subst(index: number, expr: Expr): Expr {
    return new EAbs(this.type, this.expr.subst(index + 1, expr));
  }

  adjust(by: number): Expr {
    return new EAbs(this.type, this.expr.adjust(by));
  }

  betareduce(): Expr | null {
    const e = this.expr.betareduce();
    if(e) return new EAbs(this.type, e);
    return null;
  }

  infer(id: number, env: Env): Type | null {
    const body = this.expr.subst(id, evar(id)).infer(id + 1, extend(env, id, this.type));
    if(!body) return null;
    return tfun(this.type, body);
  }
}
export function eabs(type: Type, expr: Expr) {
  return new EAbs(type, expr);
}

export class EApp extends Expr {
  readonly left: Expr;
  readonly right: Expr;

  constructor(left: Expr, right: Expr) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  equals(other: Expr): boolean {
    return other instanceof EApp && this.left.equals(other.left) && this.right.equals(other.right);
  }

  subst(index: number, expr: Expr): Expr {
    return new EApp(this.left.subst(index, expr), this.right.subst(index, expr));
  }

  adjust(by: number): Expr {
    return new EApp(this.left.adjust(by), this.right.adjust(by));
  }

  betareduce(): Expr | null {
    const l = this.left.betareduce();
    const r = this.right.betareduce();
    const left = l || this.left;
    if(left instanceof EAbs) {
      return left.expr.subst(0, r || this.right);
    } else {
      if(!l && !r) return null;
      return new EApp(left, r || this.right);
    }
  }

  infer(id: number, env: Env): Type | null {
    const left = this.left.infer(id, env);
    if(!left) return null;
    if(left instanceof TFun) {
      const right = this.right.infer(id, env);
      if(!right) return null;
      if(!left.left.equals(right)) return null;
      return left.right;
    } else return null;
  }
}
export function eapp(...es: Expr[]) {
  if(es.length === 0) throw new Error('invalid eapp');
  if(es.length === 1) return es[0];
  return es.reduce((x, y) => new EApp(x, y));
}

const T = tcon('T');

const V = evar;
const L = eabs;
const A = eapp;

const I = L(T, V(0));
const K = L(T, L(T, V(1)));
const II = L(tfun(T, T), V(0));

const e = A(II, I);
console.log('' + e);
console.log('' + e.infer(0, {}));
console.log('' + e.betareduce());
