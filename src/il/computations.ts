import Expr from './exprs';
import { Val } from './values';
import NameRep from '../NameRep';
import { ValType } from './types';

export abstract class Comp extends Expr {
  private readonly _type = 'Comp';

  abstract subst(name: NameRep, val: Val): Comp;
  
  abstract substTVar(name: NameRep, type: ValType): Comp;
}

export class Return extends Comp {

  constructor(
    public readonly val: Val,
  ) { super() }

  toString() {
    return `(return ${this.val})`;
  }

  subst(name: NameRep, val: Val): Comp {
    return new Return(this.val.subst(name, val));
  }

  substTVar(name: NameRep, type: ValType): Comp {
    return new Return(this.val.substTVar(name, type));
  }

}
export const ret = (val: Val) => new Return(val);
export const isReturn = (expr: Expr): expr is Return => expr instanceof Return;

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

export class AppT extends Expr {

  constructor(
    public readonly left: Expr,
    public readonly right: Type,
  ) { super() }

  toString() {
    return `(${this.left} @${this.right})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return new AppT(this.left.subst(name, val), this.right);
  }

  substTVar(name: NameRep, type: Type): Expr {
    return new AppT(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const appT = (left: Expr, right: Type) => new AppT(left, right);
export const appTs = (left: Expr, ts: Type[]) => ts.reduce(appT, left);
export const isAppT = (expr: Expr): expr is AppT => expr instanceof AppT;

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
