type Env = { [key: number]: Expr };
function extend(env: Env, id: number, type: Expr) {
  return { ...env, [id]: type };
}
function showEnv(env: Env) {
  return '{' + Object.keys(env).map((k: string) => `${k}: ${env[+k]}`).join(', ') + '}';
}
function adjustEnv(env: Env, by: number) {
  const o: Env = {};
  for(let k in env) {
    o[+k + by] = env[k];
  }
  return o;
}

export abstract class Expr {
  abstract toString(): string;
  abstract equals(other: Expr): boolean;
  abstract subst(index: number, expr: Expr): Expr;
  abstract adjust(by: number): Expr;
  abstract betareduce(): Expr | null;
  abstract infer(env: Env): Expr | null;

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

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return env[this.index] || null;
  }
}
export function evar(index: number) {
  return new EVar(index);
}

export class EUni extends Expr {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return this.index === 0? '*': `*${this.index.toString()}`;
  }

  equals(other: Expr): boolean {
    return other instanceof EUni && this.index === other.index;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return new EUni(this.index + 1);
  }
}
export function euni(index: number) {
  return new EUni(index);
}

export class EPi extends Expr {
  readonly type: Expr;
  readonly expr: Expr;

  constructor(type: Expr, expr: Expr) {
    super();
    this.type = type;
    this.expr = expr;
  }

  toString() {
    return `(/${this.type}.${this.expr})`;
  }

  equals(other: Expr): boolean {
    return other instanceof EPi && this.type.equals(other.type) && this.expr.equals(other.expr);
  }

  subst(index: number, expr: Expr): Expr {
    return new EPi(this.type.subst(index + 1, expr), this.expr.subst(index + 1, expr));
  }

  adjust(by: number): Expr {
    return new EPi(this.type, this.expr.adjust(by));
  }

  betareduce(): Expr | null {
    const t = this.type.betareduce();
    const e = this.expr.betareduce();
    if(t || e) return new EPi(t || this.type, e || this.expr);
    return null;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return euni(0);
  }
}
export function epi(type: Expr, expr: Expr) {
  return new EPi(type, expr);
}

export class EAbs extends Expr {
  readonly type: Expr;
  readonly expr: Expr;

  constructor(type: Expr, expr: Expr) {
    super();
    this.type = type;
    this.expr = expr;
  }

  toString() {
    return `(\\${this.type}.${this.expr})`;
  }

  equals(other: Expr): boolean {
    return other instanceof EAbs && this.type.equals(other.type) && this.expr.equals(other.expr);
  }

  subst(index: number, expr: Expr): Expr {
    return new EAbs(this.type.subst(index + 1, expr), this.expr.subst(index + 1, expr));
  }

  adjust(by: number): Expr {
    return new EAbs(this.type, this.expr.adjust(by));
  }

  betareduce(): Expr | null {
    const t = this.type.betareduce();
    const e = this.expr.betareduce();
    if(t || e) return new EAbs(t || this.type, e || this.expr);
    return null;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    const body = this.expr.infer(extend(adjustEnv(env, 1), 0, this.type));
    if(!body) return null;
    return epi(this.type, body);
  }
}
export function eabs(type: Expr, expr: Expr) {
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

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    const left = this.left.infer(env);
    if(!left) return null;
    if(left instanceof EPi) {
      const right = this.right.infer(env);
      if(!right) return null;
      if(!left.type.equals(right)) return null;
      return left.expr.subst(0, this.right);
    } else return null;
  }
}
export function eapp(...es: Expr[]) {
  if(es.length === 0) throw new Error('invalid eapp');
  if(es.length === 1) return es[0];
  return es.reduce((x, y) => new EApp(x, y));
}

export class ENat extends Expr {
  toString() {
    return 'Nat';
  }

  equals(other: Expr): boolean {
    return other instanceof ENat;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return euni(0);
  }
}
export const enat = new ENat();

export class EZ extends Expr {
  toString() {
    return 'z';
  }

  equals(other: Expr): boolean {
    return other instanceof EZ;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return enat;
  }
}
export const ez = new EZ();

export class ES extends Expr {
  toString() {
    return 's';
  }

  equals(other: Expr): boolean {
    return other instanceof ES;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return epi(enat, enat);
  }
}
export const es = new ES();

export class ENatElim extends Expr {
  toString() {
    return 'natElim';
  }

  equals(other: Expr): boolean {
    return other instanceof ENatElim;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return epi(epi(enat, euni(0)),
            epi(eapp(V(1), ez),
            epi(epi(enat, epi(eapp(V(4), V(1)), eapp(V(4), eapp(es, V(1))))),
            epi(enat, eapp(V(3), V(0))))));
  }
}
export const enatelim = new ENatElim;

export class EVec extends Expr {
  toString() {
    return 'Vec';
  }

  equals(other: Expr): boolean {
    return other instanceof EVec;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return epi(enat, epi(euni(0), euni(0)));
  }
}
export const evec = new EVec();

export class ENil extends Expr {
  toString() {
    return 'Nil';
  }

  equals(other: Expr): boolean {
    return other instanceof ENil;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return epi(euni(0), eapp(evec, ez, V(0)));
  }
}
export const enil = new ENil();

export class ECons extends Expr {
  toString() {
    return 'Cons';
  }

  equals(other: Expr): boolean {
    return other instanceof ECons;
  }

  subst(index: number, expr: Expr): Expr {
    return this;
  }

  adjust(by: number): Expr {
    return this;
  }

  betareduce(): Expr | null {
    return this;
  }

  infer(env: Env): Expr | null {
    console.log('infer:' + this + ' ; ' + showEnv(env));
    return epi(euni(0), epi(enat, epi(V(2), epi(eapp(evec, V(2), V(3)), eapp(evec, eapp(es, V(2)), V(3))))));
  }
}
export const econs = new ECons();

const V = evar;
const U = euni;
const P = epi;
const L = eabs;
const A = eapp;

const z = ez;
const s = (n: Expr) => A(es, n);

const U0 = U(0);

const I = L(U0, L(V(1), V(0)));

const e = A(enatelim, L(enat, P(enat, enat)), L(enat, V(0)));
console.log('' + e);
console.log('' + e.infer({}));
console.log('' + e.betareduce());

/*

\*.\1.0
/* /1 1

*/
