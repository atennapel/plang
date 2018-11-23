import Expr from './exprs';
import { Val } from './values';
import NameRep from '../NameRep';
import Type from './types';

export abstract class Comp extends Expr {
  private readonly _type = 'Comp';

  abstract subst(name: NameRep, val: Val): Comp;
  
  abstract substTVar(name: NameRep, type: Type): Comp;
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

  substTVar(name: NameRep, type: Type): Comp {
    return new Return(this.val.substTVar(name, type));
  }

}
export const ret = (val: Val) => new Return(val);
export const isReturn = (expr: Expr): expr is Return => expr instanceof Return;

export class App extends Comp {

  constructor(
    public readonly left: Val,
    public readonly right: Val,
  ) { super() }

  toString() {
    return `(${this.left} ${this.right})`;
  }

  subst(name: NameRep, val: Val): Comp {
    return new App(this.left.subst(name, val), this.right.subst(name, val));
  }

  substTVar(name: NameRep, type: Type): Comp {
    return new App(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const app = (left: Val, right: Val) => new App(left, right);
// export const appFrom = (es: Val[]) => es.reduce(app);
// export function apps(...es: Val[]) { return appFrom(es) }
export const isApp = (expr: Expr): expr is App => expr instanceof App;

export class AppT extends Comp {

  constructor(
    public readonly left: Val,
    public readonly right: Type,
  ) { super() }

  toString() {
    return `(${this.left} @${this.right})`;
  }

  subst(name: NameRep, val: Val): Comp {
    return new AppT(this.left.subst(name, val), this.right);
  }

  substTVar(name: NameRep, type: Type): Comp {
    return new AppT(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const appT = (left: Val, right: Type) => new AppT(left, right);
// export const appTs = (left: Expr, ts: Type[]) => ts.reduce(appT, left);
export const isAppT = (expr: Expr): expr is AppT => expr instanceof AppT;

export class Let extends Comp {

  constructor(
    public readonly name: NameRep,
    public readonly expr: Comp,
    public readonly body: Comp,
  ) { super() }

  toString() {
    return `(let ${this.name} = ${this.expr} in ${this.body})`;
  }

  subst(name: NameRep, val: Val): Comp {
    return this.name.equals(name) ?
      new Let(this.name, this.expr.subst(name, val), this.body) :
      new Let(this.name, this.expr.subst(name, val), this.body.subst(name, val)) ;
  }
  open(val: Val): Comp {
    return this.body.subst(this.name, val);
  }

  substTVar(name: NameRep, type: Type): Comp {
    return new Let(this.name, this.expr.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const lt = (name: NameRep, expr: Comp, body: Comp) => new Let(name, expr, body);
export const lts = (ns: [NameRep, Comp][], body: Comp) => ns.reduceRight((b, [n, e]) => lt(n, e, b), body);
export const isLet = (expr: Expr): expr is Let => expr instanceof Let;
