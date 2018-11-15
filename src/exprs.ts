import Type from "./types";

export default abstract class Expr {

  abstract toString(): string;

}

export class Var extends Expr {

  constructor(
    public readonly name: string,
  ) { super() }

  toString() {
    return this.name;
  }

}
export const vr = (name: string) => new Var(name);

export class Abs extends Expr {

  constructor(
    public readonly name: string,
    public readonly type: Type | null,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return this.type ? `(\\(${this.name} : ${this.type}). ${this.body})` : `(\\${this.name}. ${this.body})`;
  }

}

export class App extends Expr {

  constructor(
    public readonly left: Expr,
    public readonly right: Expr,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }

}
export const app = (left: Expr, right: Expr) => new TFun(left, right);
export const appFrom = (es: Expr[]) => ts.reduceRight((x, y) => tfun(y, x));
export function apps(...es: Expr[]) { return tfunFrom(ts) }

export class Anno extends Expr {

  constructor(
    public readonly expr: Expr,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }

}
export const anno = (expr: Expr, type: Type) => new Anno(expr, type);
