import Type from './types';
import NameRep from './NameRep';
import { objMapToArr, objMap } from './utils';

export default abstract class Expr {

  abstract toString(): string;

  abstract subst(name: NameRep, val: Expr): Expr;
  
  abstract substTVar(name: NameRep, type: Type): Expr;

}

export class Var extends Expr {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  subst(name: NameRep, val: Expr): Expr {
    return this.name.equals(name) ? val : this;
  }

  substTVar(name: NameRep, type: Type): Expr {
    return this;
  }

}
export const vr = (name: NameRep) => new Var(name);
export const isVar = (expr: Expr): expr is Var => expr instanceof Var;

export class Abs extends Expr {

  constructor(
    public readonly name: NameRep,
    public readonly type: Type | null,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return this.name.equals(name) ? this : new Abs(this.name, this.type, this.body.subst(name, val));
  }
  open(val: Expr): Expr {
    return this.body.subst(this.name, val);
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new Abs(this.name, this.type && this.type.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const abs = (name: NameRep, body: Expr) => new Abs(name, null, body);
export const absty = (name: NameRep, type: Type, body: Expr) => new Abs(name, type, body);
export const abss = (ns: NameRep[], body: Expr) => ns.reduceRight((b, n) => abs(n, b), body);
export const abstys = (ns: [NameRep, Type][], body: Expr) => ns.reduceRight((b, [n, t]) => absty(n, t, b), body);
export const isAbs = (expr: Expr): expr is Abs => expr instanceof Abs;

export class Anno extends Expr {

  constructor(
    public readonly expr: Expr,
    public readonly type: Type,
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return new Anno(this.expr.subst(name, val), this.type);
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new Anno(this.expr.substTVar(name, type), this.type.substTVar(name, type));
  }

}
export const anno = (expr: Expr, type: Type) => new Anno(expr, type);
export const isAnno = (expr: Expr): expr is Anno => expr instanceof Anno;

export class App extends Expr {

  constructor(
    public readonly left: Expr,
    public readonly right: Expr,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return new App(this.left.subst(name, val), this.right.subst(name, val));
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new App(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const app = (left: Expr, right: Expr) => new App(left, right);
export const appFrom = (es: Expr[]) => es.reduce(app);
export function apps(...es: Expr[]) { return appFrom(es) }
export const isApp = (expr: Expr): expr is App => expr instanceof App;

export class Let extends Expr {

  constructor(
    public readonly name: NameRep,
    public readonly expr: Expr,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return `(let ${this.name} = ${this.expr} in ${this.body})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return this.name.equals(name) ?
      new Let(this.name, this.expr.subst(name, val), this.body) :
      new Let(this.name, this.expr.subst(name, val), this.body.subst(name, val)) ;
  }
  open(val: Expr): Expr {
    return this.body.subst(this.name, val);
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new Let(this.name, this.expr.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const lt = (name: NameRep, expr: Expr, body: Expr) => new Let(name, expr, body);
export const lts = (ns: [NameRep, Expr][], body: Expr) => ns.reduceRight((b, [n, e]) => lt(n, e, b), body);
export const isLet = (expr: Expr): expr is Let => expr instanceof Let;

export class Handler extends Expr {

  constructor(public readonly cases: HandlerCase) { super() }

  toString() {
    return `(handler {${this.cases}})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return new Handler(this.cases.subst(name, val));
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new Handler(this.cases.substTVar(name, type));
  }

}
export const handler = (cases: HandlerCase) => new Handler(cases);
export const isHandler = (expr: Expr): expr is Handler => expr instanceof Handler;

export abstract class HandlerCase {

  abstract toString(): string;

  abstract subst(name: NameRep, val: Expr): HandlerCase;
  abstract substTVar(name: NameRep, type: Type): HandlerCase;

}

export class HOp extends HandlerCase {

  constructor(
    public readonly op: NameRep,
    public readonly x: NameRep,
    public readonly k: NameRep,
    public readonly expr: Expr,
    public readonly rest: HandlerCase,
  ) { super() }

  toString() {
    return `${this.op} ${this.x} ${this.k} -> ${this.expr}; ${this.rest}`;
  }

  subst(name: NameRep, val: Expr): HandlerCase {
    return new HOp(
      this.op,
      this.x,
      this.k,
      name.equals(this.x) || name.equals(this.k) ? this.expr : this.expr.subst(name, val),
      this.rest.subst(name, val),
    );
  }
  open(valX: Expr, valK: Expr): Expr {
    return this.expr.subst(this.x, valX).subst(this.k, valK);
  }

  substTVar(name: NameRep, type: Type): HandlerCase {
    return new HOp(
      this.op,
      this.x,
      this.k,
      this.expr.substTVar(name, type),
      this.rest.substTVar(name, type),
    );
  }

}
export const hop = (op: NameRep, x: NameRep, k: NameRep, expr: Expr, rest: HandlerCase) => new HOp(op, x, k, expr, rest);
export const isHOp = (expr: Expr): expr is HOp => expr instanceof HOp;

export class HReturn extends HandlerCase {

  constructor(
    public readonly x: NameRep,
    public readonly expr: Expr,
  ) { super() }

  toString() {
    return `return ${this.x} -> ${this.expr}`;
  }

  subst(name: NameRep, val: Expr): HandlerCase {
    return new HReturn(
      this.x,
      name.equals(this.x) ? this.expr : this.expr.subst(name, val),
    );
  }
  open(val: Expr): Expr {
    return this.expr.subst(this.x, val);
  }

  substTVar(name: NameRep, type: Type): HandlerCase {
    return new HReturn(
      this.x,
      this.expr.substTVar(name, type),
    );
  }
  
}
export const hreturn = (x: NameRep, expr: Expr) => new HReturn(x, expr);
export const isHReturn = (expr: Expr): expr is HReturn => expr instanceof HReturn;
