import {
  Type,
  TVar,
  TCon,
  TApp,
  TRowEmpty,
  TRowExtend,
  hasLabel,
} from './types';
import { Apply, Subst, emptySubst, compose } from './typechecker';
import Set from './Set';
import { Result } from './Result';
import Map from './Map';
import { KRow } from './kinds';
import { terr } from './utils';

export abstract class Constraint implements Apply<Constraint> {
  constructor() {}
  abstract order(): number;
  abstract free(): Set<TVar>;
  abstract subst(sub: Subst): Constraint;
  abstract check(): Result<TypeError, Subst>;
}

function occurs(v: TVar, t: Type) {
  return t.free().contains(v);
}

function bind(v: TVar, t: Type): Result<TypeError, Subst> {
  if(t instanceof TVar && v.eq(t)) return Result.ok(emptySubst);
  if(occurs(v, t))
    return Result.err(new TypeError('Occurs check failed: ' + v + ' and ' + t));
  return Result.ok(Map.of([v, t]));
}

export class Unify extends Constraint implements Apply<Constraint> {
  left: Type;
  right: Type;
  constructor(left: Type, right: Type) {
    super();
    this.left = left;
    this.right = right;
  }
  order() { return 0 }
  free() { return this.left.free().union(this.right.free()) }
  subst(sub: Subst) {
    return new Unify(this.left.subst(sub), this.right.subst(sub));
  }
  toString(): string { return this.left + ' ~ ' + this.right }
  static unify(a: Type, b: Type): Result<TypeError, Subst> {
    if(!a.kind.eq(b.kind))
      return Result.err(new TypeError(
        'Cannot unify kinds ' + a + ' and ' + b +
        ' (' + a.kind + ' and ' + b.kind + ')'));
    if(a instanceof TVar) return bind(a, b);
    if(b instanceof TVar) return bind(b, a);
    if(a instanceof TCon && b instanceof TCon && a.name === b.name)
      return Result.ok(emptySubst);
    if(a instanceof TRowEmpty && b instanceof TRowEmpty)
      return Result.ok(emptySubst);
    if(a instanceof TApp && b instanceof TApp)
      return Unify.unify(a.left, b.left)
        .then(s1 => Unify.unify(a.right.subst(s1), b.right.subst(s1))
        .map(s2 => compose(s1, s2)));
    if(a instanceof TRowExtend && b instanceof TRowExtend) {
      if(a.label === b.label)
        return Unify.unify(a.type, b.type)
          .then(s1 => Unify.unify(a.rest.subst(s1), b.rest.subst(s1))
          .map(s2 => compose(s1, s2)));
    }
    return Result.err(new TypeError('Cannot unify ' + a + ' and ' + b));
  }
  check() { return Unify.unify(this.left, this.right) }
}

export class Lacks extends Constraint implements Apply<Constraint> {
  row: Type;
  label: string;
  constructor(row: Type, label: string) {
    if(!row.kind.eq(KRow)) terr('Type must be of kind row in Lacks constraint');
    super();
    this.row = row;
    this.label = label;
  }
  order() { return 1 }
  free() { return this.row.free() }
  subst(sub: Subst) {
    return new Lacks(this.row.subst(sub), this.label);
  }
  toString(): string { return this.row + '/' + this.label }
  check() {
    return hasLabel(this.row, this.label)?
      Result.err(new TypeError(this + ' does not hold')):
      Result.ok(emptySubst);
  }
}
