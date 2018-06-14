import { Type } from './types'; 

export abstract class Expr {
  abstract toString(): string;
  abstract subst(name: string, expr: Expr): Expr;
}

export class EUnit extends Expr {
  toString() {
    return '()';
  }
  subst(name: string, expr: Expr): Expr {
    return this;
  }
}
export const eunit = new EUnit();

export class EVar extends Expr {
  constructor(public readonly name: string) { super() }

  toString() {
    return `${this.name}`;
  }
  subst(name: string, expr: Expr): Expr {
    return this.name === name? expr: this;
  }
}
export const evar = (name: string) => new EVar(name);

export class EApp extends Expr {
  constructor(
    public readonly left: Expr,
    public readonly right: Expr
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new EApp(this.left.subst(name, expr), this.right.subst(name, expr));
  }
}
export const eapp = (left: Expr, right: Expr) => new EApp(left, right);
export const eapps = (...es: Expr[]) => es.reduce(eapp);

export class EAbs extends Expr {
  constructor(
    public readonly name: string,
    public readonly expr: Expr
  ) { super() }

  toString() {
    return `(\\${this.name} -> ${this.expr})`;
  }
  subst(name: string, expr: Expr): Expr {
    return this.name === name? this: new EAbs(this.name, this.expr.subst(name, expr));
  }
}
export const eabs = (name: string, expr: Expr) => new EAbs(name, expr);
export const eabss = (ns: string[], expr: Expr) => ns.reduceRight((a, b) => eabs(b, a), expr);

export class EAnno extends Expr {
  constructor(
    public readonly expr: Expr,
    public readonly type: Type
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }
  subst(name: string, expr: Expr): Expr {
    return new EAnno(this.expr.subst(name, expr), this.type);
  }
}
export const eanno = (expr: Expr, type: Type) => new EAnno(expr, type);
