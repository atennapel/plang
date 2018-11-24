import Type from './types';
import Kind from './kinds';

export default abstract class Expr {

  abstract toString(): string;

  abstract isValue(): boolean;

}

export abstract class Val extends Expr {
  private readonly _type = 'Val';

  isValue() {
    return true;
  }
}

export class Var extends Val {

  constructor(
    public readonly name: string,
  ) { super() }

  toString() {
    return this.name.toString();
  }

}
export const vr = (name: string) => new Var(name);
export const isVar = (expr: Expr): expr is Var => expr instanceof Var;

export class Abs extends Val {

  constructor(
    public readonly name: string,
    public readonly type: Type | null,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
  }

}
export const abs = (name: string, body: Expr) => new Abs(name, null, body);
export const absty = (name: string, type: Type, body: Expr) => new Abs(name, type, body);
export const abss = (ns: string[], body: Expr) => ns.reduceRight((b, n) => abs(n, b), body);
export const abstys = (ns: [string, Type][], body: Expr) => ns.reduceRight((b, [n, t]) => absty(n, t, b), body);
export const isAbs = (expr: Expr): expr is Abs => expr instanceof Abs;

export class AbsT extends Val {

  constructor(
    public readonly name: string,
    public readonly kind: Kind,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
  }

}
export const absT = (name: string, kind: Kind, body: Expr) => new AbsT(name, kind, body);
export const absTs = (ns: [string, Kind][], body: Expr) => ns.reduceRight((b, [n, k]) => absT(n, k, b), body);
export const isAbsT = (expr: Expr): expr is AbsT => expr instanceof AbsT;

export class Anno extends Val {

  constructor(
    public readonly expr: Expr,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }

}
export const anno = (expr: Expr, type: Type) => new Anno(expr, type);
export const isAnno = (expr: Expr): expr is Anno => expr instanceof Anno;

export abstract class Comp extends Expr {
  private readonly _type = 'Comp';

  isValue() {
    return false;
  }
}

export class Return extends Comp {

  constructor(
    public readonly val: Expr,
  ) { super() }

  toString() {
    return `(return ${this.val})`;
  }

}
export const ret = (val: Expr) => new Return(val);
export const isReturn = (expr: Expr): expr is Return => expr instanceof Return;

export class App extends Comp {

  constructor(
    public readonly left: Expr,
    public readonly right: Expr,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }

}
export const app = (left: Expr, right: Expr) => new App(left, right);
export const appFrom = (es: Expr[]) => es.reduce(app);
export function apps(...es: Expr[]) { return appFrom(es) }
export const isApp = (expr: Expr): expr is App => expr instanceof App;

export class AppT extends Comp {

  constructor(
    public readonly left: Expr,
    public readonly right: Type,
  ) { super() }

  toString() {
    return `(${this.left} @${this.right})`;
  }

}
export const appT = (left: Expr, right: Type) => new AppT(left, right);
export const appTs = (left: Expr, ts: Type[]) => ts.reduce(appT, left);
export const isAppT = (expr: Expr): expr is AppT => expr instanceof AppT;

export class Let extends Comp {

  constructor(
    public readonly name: string,
    public readonly expr: Expr,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return `(let ${this.name} = ${this.expr} in ${this.body})`;
  }

}
export const lt = (name: string, expr: Expr, body: Expr) => new Let(name, expr, body);
export const lts = (ns: [string, Expr][], body: Expr) => ns.reduceRight((b, [n, e]) => lt(n, e, b), body);
export const isLet = (expr: Expr): expr is Let => expr instanceof Let;
