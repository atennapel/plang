import Type from './types';
import { INameRep } from './generic/NameRep';

export default abstract class Expr<N extends INameRep<N>> {

  abstract toString(): string;

  abstract subst(name: N, val: Expr<N>): Expr<N>;

}

export class Var<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly name: N,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return this.name.equals(name) ? val : this;
  }

}
export const vr = <N extends INameRep<N>>(name: N) => new Var(name);
export const isVar = <N extends INameRep<N>>(expr: Expr<N>): expr is Var<N> => expr instanceof Var;

export class Abs<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly name: N,
    public readonly type: Type<N> | null,
    public readonly body: Expr<N>,
  ) { super() }

  toString() {
    return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return this.name.equals(name) ? this : new Abs(this.name, this.type, this.body.subst(name, val));
  }
  open(val: Expr<N>): Expr<N> {
    return this.body.subst(this.name, val);
  }

}
export const abs = <N extends INameRep<N>>(name: N, body: Expr<N>) => new Abs(name, null, body);
export const absty = <N extends INameRep<N>>(name: N, type: Type<N>, body: Expr<N>) => new Abs(name, type, body);
export const abss = <N extends INameRep<N>>(ns: N[], body: Expr<N>) => ns.reduceRight((b, n) => abs(n, b), body);
export const abstys = <N extends INameRep<N>>(ns: [N, Type<N>][], body: Expr<N>) => ns.reduceRight((b, [n, t]) => absty(n, t, b), body);
export const isAbs = <N extends INameRep<N>>(expr: Expr<N>): expr is Abs<N> => expr instanceof Abs;

export class App<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly left: Expr<N>,
    public readonly right: Expr<N>,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return new App(this.left.subst(name, val), this.right.subst(name, val));
  }

}
export const app = <N extends INameRep<N>>(left: Expr<N>, right: Expr<N>) => new App(left, right);
export const appFrom = <N extends INameRep<N>>(es: Expr<N>[]) => es.reduce(app);
export function apps<N extends INameRep<N>>(...es: Expr<N>[]) { return appFrom(es) }
export const isApp = <N extends INameRep<N>>(expr: Expr<N>): expr is App<N> => expr instanceof App;

export class Anno<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly expr: Expr<N>,
    public readonly type: Type<N>,
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return new Anno(this.expr.subst(name, val), this.type);
  }

}
export const anno = <N extends INameRep<N>>(expr: Expr<N>, type: Type<N>) => new Anno(expr, type);
export const isAnno = <N extends INameRep<N>>(expr: Expr<N>): expr is Anno<N> => expr instanceof Anno;
