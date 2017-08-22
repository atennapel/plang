import Id from './Id';
import { Type } from './types';

export abstract class Expr {
  abstract toString(): string;
}

export class EUnit extends Expr {
  toString() {
    return '()';
  }
}
export const eunit = new EUnit();

export class EVar extends Expr {
  readonly id: string;

  constructor(id: string) {
    super();
    this.id = id;
  }

  toString() {
    return this.id;
  }
}
export function evar(id: string) {
  return new EVar(id);
} 

export class ELam extends Expr {
  readonly arg: string;
  readonly body: Expr;

  constructor(arg: string, body: Expr) {
    super();
    this.arg = arg;
    this.body = body;
  }

  toString() {
    return `(\\${this.arg} -> ${this.body})`;
  }
}
export function elam(tvs: string[], t: Expr) {
  return tvs.length === 0? t: tvs.reduceRight((t, tv) => new ELam(tv, t), t);
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
}
export function eapp(...es: Expr[]) {
  return es.reduce((x, y) => new EApp(x, y));
}

export class EAnno extends Expr {
  readonly expr: Expr;
  readonly type: Type;

  constructor(expr: Expr, type: Type) {
    super();
    this.expr = expr;
    this.type = type;
  }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }
}
export function eanno(expr: Expr, type: Type) {
  return new EAnno(expr, type);
}
