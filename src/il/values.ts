import Expr from './exprs';
import NameRep from '../NameRep';
import { ValType } from './types';
import { Comp, ret } from './computations';
import Kind from './kinds';

export abstract class Val extends Expr {
  private readonly _type = 'Val';

  abstract subst(name: NameRep, val: Val): Val;
  
  abstract substTVar(name: NameRep, type: ValType): Val;
}

export class Var extends Val {

  constructor(
    public readonly name: NameRep,
  ) { super() }

  toString() {
    return this.name.toString();
  }

  subst(name: NameRep, val: Val): Val {
    return this.name.equals(name) ? val : this;
  }

  substTVar(name: NameRep, type: ValType): Val {
    return this;
  }

}
export const vr = (name: NameRep) => new Var(name);
export const isVar = (expr: Expr): expr is Var => expr instanceof Var;

export class Abs extends Val {

  constructor(
    public readonly name: NameRep,
    public readonly type: ValType | null,
    public readonly body: Comp,
  ) { super() }

  toString() {
    return this.type ? `(\\(${this.name} : ${this.type}) -> ${this.body})` : `(\\${this.name} -> ${this.body})`;
  }

  subst(name: NameRep, val: Val): Val {
    return this.name.equals(name) ? this : new Abs(this.name, this.type, this.body.subst(name, val));
  }
  open(val: Val): Comp {
    return this.body.subst(this.name, val);
  }

  substTVar(name: NameRep, type: ValType): Val {
    return new Abs(this.name, this.type && this.type.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const abs = (name: NameRep, body: Comp) => new Abs(name, null, body);
export const absty = (name: NameRep, type: ValType, body: Comp) => new Abs(name, type, body);
export const abss = (ns: NameRep[], body: Comp) => ns.reduceRight((b, n) => ret(abs(n, b)), body);
export const abstys = (ns: [NameRep, ValType][], body: Comp) => ns.reduceRight((b, [n, t]) => ret(absty(n, t, b)), body);
export const isAbs = (expr: Expr): expr is Abs => expr instanceof Abs;

export class AbsT extends Val {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
    public readonly body: Comp,
  ) { super() }

  toString() {
    return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
  }

  subst(name: NameRep, val: Val): Val {
    return new AbsT(this.name, this.kind, this.body.subst(name, val));
  }

  substTVar(name: NameRep, type: ValType): Val {
    return this.name.equals(name) ? this : new AbsT(this.name, this.kind, this.body.substTVar(name, type));
  }
  openTVar(val: ValType): Expr {
    return this.body.substTVar(this.name, val);
  }

}
export const absT = (name: NameRep, kind: Kind, body: Comp) => new AbsT(name, kind, body);
export const absTs = (ns: [NameRep, Kind][], body: Comp) => ns.reduceRight((b, [n, k]) => ret(absT(n, k, b)), body);
export const isAbsT = (expr: Expr): expr is AbsT => expr instanceof AbsT;

export class Anno extends Val {

  constructor(
    public readonly expr: Val,
    public readonly type: ValType,
  ) { super() }

  toString() {
    return `(${this.expr} : ${this.type})`;
  }

  subst(name: NameRep, val: Val): Val {
    return new Anno(this.expr.subst(name, val), this.type);
  }

  substTVar(name: NameRep, type: ValType): Val {
    return new Anno(this.expr.substTVar(name, type), this.type.substTVar(name, type));
  }

}
export const anno = (expr: Val, type: ValType) => new Anno(expr, type);
export const isAnno = (expr: Expr): expr is Anno => expr instanceof Anno;
