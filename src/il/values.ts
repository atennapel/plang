import Expr from './exprs';
import NameRep from '../NameRep';
import { ValType } from './types';

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

  subst(name: NameRep, val: Expr): Expr {
    return this.name.equals(name) ? val : this;
  }

  substTVar(name: NameRep, type: ValType): Expr {
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

export class AbsT extends Expr {

  constructor(
    public readonly name: NameRep,
    public readonly kind: Kind,
    public readonly body: Expr,
  ) { super() }

  toString() {
    return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
  }

  subst(name: NameRep, val: Expr): Expr {
    return new AbsT(this.name, this.kind, this.body.subst(name, val));
  }

  substTVar(name: NameRep, type: Type): Expr {
    return this.name.equals(name) ? this : new AbsT(this.name, this.kind, this.body.substTVar(name, type));
  }
  openTVar(val: Type): Expr {
    return this.body.substTVar(this.name, val);
  }

}
export const absT = (name: NameRep, kind: Kind, body: Expr) => new AbsT(name, kind, body);
export const absTs = (ns: [NameRep, Kind][], body: Expr) => ns.reduceRight((b, [n, k]) => absT(n, k, b), body);
export const isAbsT = (expr: Expr): expr is AbsT => expr instanceof AbsT;

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
