import { Eq, Hash } from './interfaces';
import { terr } from './utils';
import { Kind, KArr, karr, KType, KRow } from './kinds';
import { Constraint } from './constraints';
import Set from './Set';
import Env from './Env';
import { Apply, Subst, InferState } from './typechecker';
import Map, {KVPair} from './Map';
import { Result } from './Result';

export abstract class Type implements Eq, Apply<Type> {
  kind: Kind;
  constructor(kind: Kind) { this.kind = kind }
  eq(other: any): boolean { return false };
  abstract free(): Set<TVar>;
  abstract subst(sub: Subst): Type;
  generalize(env: Env, constraints: Constraint[]): TScheme {
    return new TScheme(this.free().without(env.free()), constraints, this);
  }
}

export class TVar extends Type implements Eq, Hash, Apply<Type> {
  name: string;
  id: string;
  constructor(name: string, id: string, kind?: Kind) {
    super(kind || KType);
    this.name = name;
    this.id = id;
  }
  eq(other: TVar) { return this.id === other.id }
  toString() { return this.id }
  hash() { return this.id }
  free() { return Set.of(this) }
  subst(sub: Subst): Type { return sub.get(this) || this }
}

export class TCon extends Type implements Eq, Apply<Type> {
  name: string;
  constructor(name: string, kind?: Kind) {
    super(kind || KType);
    this.name = name;
  }
  eq(other: TCon): boolean { return this.name === other.name }
  toString(): string { return this.name }
  hash(): string { return this.name }
  free() { return Set.empty<TVar>() }
  subst(sub: Subst) { return this }
}

export class TApp extends Type implements Eq, Apply<Type> {
  left: Type;
  right: Type;
  constructor(left: Type, right: Type) {
    if(!(left.kind instanceof KArr))
      terr('Invalid kind in TApp (left side): ' + left.kind);
    if(!(<KArr> left.kind).left.eq(right.kind))
      terr('Invalid kind in TApp (right side): ' + right.kind);
    super((<KArr> left.kind).right);
    this.left = left;
    this.right = right;
  }
  eq(other: TApp): boolean {
    return this.left.eq(other.left) && this.right.eq(other.right);
  }
  toString(): string {
    if(
      this.left instanceof TApp &&
      this.left.left instanceof TCon &&
      /[^a-z]/i.test(this.left.left.name[0])
    ) return '(' + this.left.right + ' ' + this.left.left +
        ' ' + this.right + ')';
    return '(' + this.left + ' ' + this.right + ')';
  }
  free() { return this.left.free().union(this.right.free()) }
  subst(sub: Subst) {
    return new TApp(this.left.subst(sub), this.right.subst(sub));
  }
}

export class TRowEmpty extends Type implements Eq, Apply<Type> {
  constructor() { super(KRow) }
  eq(other: TRowEmpty) { return true }
  toString() { return '{}' }
  free() { return Set.empty<TVar>() }
  subst(sub: Subst) { return this }
}
export class TRowExtend extends Type implements Eq, Apply<Type> {
  label: string;
  type: Type;
  rest: Type;
  constructor(label: string, type: Type, rest: Type) {
    if(!rest.kind.eq(KRow))
      terr('Rest in TRowExtend must be of kind KRow');
    super(KRow);
    this.label = label;
    this.type = type;
    this.rest = rest;
  }
  eq(other: TRowExtend) {
    return this.label === other.label &&
      this.type.eq(other.type) && this.rest.eq(other.rest);
  }
  toString() {
    return '{' + this.label + ' : ' + this.type + ' | ' + this.rest + '}';
  }
  free() { return this.type.free().union(this.rest.free()) }
  subst(sub: Subst) {
    return new TRowExtend(
      this.label,
      this.type.subst(sub),
      this.rest.subst(sub)
    );
  }
}
export function hasLabel(type: Type, label: string): boolean {
  if(type instanceof TRowExtend)
    return type.label === label || hasLabel(type.rest, label);
  return false;
}
export function rewriteRow(label: string, type: Type):
  Result<TypeError, Type> {
  if(type instanceof TRowExtend) {
    if(type.label === label) return Result.ok(type);
    else {
      rewriteRow(label, type.rest)
        .map(r => )
    }
  } else return Result.err(new TypeError(
    'Cannot rewrite row (' + label + '): ' + type));
}

export let Bool = new TCon('Bool', KType);
export let Int = new TCon('Int', KType);
export let Float = new TCon('Float', KType);
export let Str = new TCon('Str', KType);
export let Record = new TCon('Rec', karr(KRow, KType));
export let Variant = new TCon('Var', karr(KRow, KType));
export let TArr = new TCon('->', karr(KType, KType, KType));
export function tarr(...args: Type[]): Type {
  if(args.length === 0) return TArr;
  return args.reduceRight((l, r) => new TApp(new TApp(TArr, r), l));
}

export class TScheme implements Apply<TScheme> {
  vars: Set<TVar>;
  constraints: Constraint[];
  type: Type;
  constructor(vars: Set<TVar>, constraints: Constraint[], type: Type) {
    this.vars = vars;
    this.constraints = constraints;
    this.type = type;
  }
  static of(type: Type) { return new TScheme(Set.empty<TVar>(), [], type) }
  toString(): string {
    return 'forall ' + this.vars +
      ' [' + this.constraints.join(', ') + '] . ' + this.type;
  }
  free() { return this.type.free().without(this.vars) }
  subst(sub: Subst) {
    return new TScheme(
      this.vars,
      this.constraints.map(c => c.subst(sub)),
      this.type.subst(sub.removeKeySet(this.vars))
    );
  }
  instantiate(state: InferState): [InferState, Type, Constraint[]] {
    let res = this.vars.vals().reduce((st, tv) => {
      let r = st[0].fresh(tv.name, tv.kind);
      return <[InferState, KVPair<TVar, Type>[]]>
        [r[0], st[1].concat([[tv, r[1]]])];
    }, <[InferState, KVPair<TVar, Type>[]]> [state, []]);
    let sub = Map.from(res[1]);
    return [
      res[0],
      this.type.subst(sub),
      this.constraints.map(x => x.subst(sub))
    ];
  }
}
