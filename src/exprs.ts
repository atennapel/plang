import Type from './types';
import { INameRep } from './generic/NameRep';
import Kind from './kinds';

export default abstract class Expr<N extends INameRep<N>> {

  abstract toString(): string;

  abstract subst(name: N, val: Expr<N>): Expr<N>;
  
  abstract substTVar(name: N, type: Type<N>): Expr<N>;

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

  substTVar(name: N, type: Type<N>): Expr<N> {
    return this;
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

  substTVar(name: N, type: Type<N>): Expr<N> {
    return new Abs(this.name, this.type && this.type.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const abs = <N extends INameRep<N>>(name: N, body: Expr<N>) => new Abs(name, null, body);
export const absty = <N extends INameRep<N>>(name: N, type: Type<N>, body: Expr<N>) => new Abs(name, type, body);
export const abss = <N extends INameRep<N>>(ns: N[], body: Expr<N>) => ns.reduceRight((b, n) => abs(n, b), body);
export const abstys = <N extends INameRep<N>>(ns: [N, Type<N>][], body: Expr<N>) => ns.reduceRight((b, [n, t]) => absty(n, t, b), body);
export const isAbs = <N extends INameRep<N>>(expr: Expr<N>): expr is Abs<N> => expr instanceof Abs;

export class AbsT<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly name: N,
    public readonly kind: Kind<N>,
    public readonly body: Expr<N>,
  ) { super() }

  toString() {
    return `(/\\(${this.name} : ${this.kind}) -> ${this.body})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return new AbsT(this.name, this.kind, this.body.subst(name, val));
  }

  substTVar(name: N, type: Type<N>): Expr<N> {
    return this.name.equals(name) ? this : new AbsT(this.name, this.kind, this.body.substTVar(name, type));
  }
  openTVar(val: Type<N>): Expr<N> {
    return this.body.substTVar(this.name, val);
  }

}
export const absT = <N extends INameRep<N>>(name: N, kind: Kind<N>, body: Expr<N>) => new AbsT(name, kind, body);
export const absTs = <N extends INameRep<N>>(ns: [N, Kind<N>][], body: Expr<N>) => ns.reduceRight((b, [n, k]) => absT(n, k, b), body);
export const isAbsT = <N extends INameRep<N>>(expr: Expr<N>): expr is AbsT<N> => expr instanceof AbsT;

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

  substTVar(name: N, type: Type<N>): Expr<N> {
    return new App(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const app = <N extends INameRep<N>>(left: Expr<N>, right: Expr<N>) => new App(left, right);
export const appFrom = <N extends INameRep<N>>(es: Expr<N>[]) => es.reduce(app);
export function apps<N extends INameRep<N>>(...es: Expr<N>[]) { return appFrom(es) }
export const isApp = <N extends INameRep<N>>(expr: Expr<N>): expr is App<N> => expr instanceof App;

export class AppT<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly left: Expr<N>,
    public readonly right: Type<N>,
  ) { super() }

  toString() {
    return `(${this.left} @${this.right})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return new AppT(this.left.subst(name, val), this.right);
  }

  substTVar(name: N, type: Type<N>): Expr<N> {
    return new AppT(this.left.substTVar(name, type), this.right.substTVar(name, type));
  }

}
export const appT = <N extends INameRep<N>>(left: Expr<N>, right: Type<N>) => new AppT(left, right);
export const appTs = <N extends INameRep<N>>(left: Expr<N>, ts: Type<N>[]) => ts.reduce(appT, left);
export const isAppT = <N extends INameRep<N>>(expr: Expr<N>): expr is AppT<N> => expr instanceof AppT;

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

  substTVar(name: N, type: Type<N>): Expr<N> {
    return new Anno(this.expr.substTVar(name, type), this.type.substTVar(name, type));
  }

}
export const anno = <N extends INameRep<N>>(expr: Expr<N>, type: Type<N>) => new Anno(expr, type);
export const isAnno = <N extends INameRep<N>>(expr: Expr<N>): expr is Anno<N> => expr instanceof Anno;

export class Let<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly name: N,
    public readonly expr: Expr<N>,
    public readonly body: Expr<N>,
  ) { super() }

  toString() {
    return `(let ${this.name} = ${this.expr} in ${this.body})`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return this.name.equals(name) ?
      new Let(this.name, this.expr.subst(name, val), this.body) :
      new Let(this.name, this.expr.subst(name, val), this.body.subst(name, val)) ;
  }
  open(val: Expr<N>): Expr<N> {
    return this.body.subst(this.name, val);
  }

  substTVar(name: N, type: Type<N>): Expr<N> {
    return new Let(this.name, this.expr.substTVar(name, type), this.body.substTVar(name, type));
  }

}
export const lt = <N extends INameRep<N>>(name: N, expr: Expr<N>, body: Expr<N>) => new Let(name, expr, body);
export const lts = <N extends INameRep<N>>(ns: [N, Expr<N>][], body: Expr<N>) => ns.reduceRight((b, [n, e]) => lt(n, e, b), body);
export const isLet = <N extends INameRep<N>>(expr: Expr<N>): expr is Let<N> => expr instanceof Let;

export class EmptyRecord<N extends INameRep<N>> extends Expr<N> {

  constructor() { super() }

  toString() {
    return `{}`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return this;
  }

  substTVar(name: N, type: Type<N>): Expr<N> {
    return this;
  }

}
export const emptyRecord = <N extends INameRep<N>>() => new EmptyRecord<N>();
export const isEmptyRecord = <N extends INameRep<N>>(expr: Expr<N>): expr is EmptyRecord<N> => expr instanceof EmptyRecord;

export abstract class WithLabel<N extends INameRep<N>> extends Expr<N> {

  constructor(
    public readonly label: N,
  ) { super() }

  toString() {
    return `withLabel@${this.label}`;
  }

  subst(name: N, val: Expr<N>): Expr<N> {
    return this;
  }

  substTVar(name: N, type: Type<N>): Expr<N> {
    return this;
  }

}
export const isWithLabel = <N extends INameRep<N>>(expr: Expr<N>): expr is WithLabel<N> => expr instanceof WithLabel;

export class Select<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `.${this.label}`;
  }
}
export const select = <N extends INameRep<N>>(label: N) => new Select(label);
export const isSelect = <N extends INameRep<N>>(expr: Expr<N>): expr is Select<N> => expr instanceof Select;

export class Inject<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `@${this.label}`;
  }
}
export const inject = <N extends INameRep<N>>(label: N) => new Inject(label);
export const isInject = <N extends INameRep<N>>(expr: Expr<N>): expr is Inject<N> => expr instanceof Inject;

export class Restrict<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `.-${this.label}`;
  }
}
export const restrict = <N extends INameRep<N>>(label: N) => new Restrict(label);
export const isRestrict = <N extends INameRep<N>>(expr: Expr<N>): expr is Restrict<N> => expr instanceof Restrict;

export class ExtendRec<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `.+${this.label}`;
  }
}
export const extendRec = <N extends INameRep<N>>(label: N) => new ExtendRec(label);
export const isExtendRec = <N extends INameRep<N>>(expr: Expr<N>): expr is ExtendRec<N> => expr instanceof ExtendRec;

export class ExtendVar<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `@+${this.label}`;
  }
}
export const extendVar = <N extends INameRep<N>>(label: N) => new ExtendVar(label);
export const isExtendVar = <N extends INameRep<N>>(expr: Expr<N>): expr is ExtendVar<N> => expr instanceof ExtendVar;

export class CaseVar<N extends INameRep<N>> extends WithLabel<N> {
  toString() {
    return `?${this.label}`;
  }
}
export const caseVar = <N extends INameRep<N>>(label: N) => new CaseVar(label);
export const isCaseVar = <N extends INameRep<N>>(expr: Expr<N>): expr is CaseVar<N> => expr instanceof CaseVar;
