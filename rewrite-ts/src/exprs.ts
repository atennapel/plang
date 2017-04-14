import { Hash } from './interfaces';
import { Infer, InferState, solve } from './typechecker';
import Env from './Env';
import { Result } from './Result';
import {
  Type,
  TScheme,
  tarr,
  TRowEmpty,
  TRowExtend,
  TApp,
  Record
} from './types';
import { KType, KRow } from './kinds';
import { Constraint, Unify, Lacks } from './constraints';
import { terr } from './utils';

export abstract class Expr implements Infer {
  constructor() {}
  abstract infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]>;
}

export class EVar extends Expr {
  name: string;
  constructor(name: string) {
    super();
    this.name = name;
  }
  toString(): string {
    return this.name;
  }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    if(!env.contains(this.name))
      return Result.err('Undefined variable: ' + this.name);
    return Result.ok(env.get(this.name).instantiate(state));
  }
}
export function vr(name: string) { return new EVar(name) }

export class ELam extends Expr {
  arg: string;
  body: Expr;
  constructor(arg: string, body: Expr) {
    super();
    this.arg = arg;
    this.body = body;
  }
  toString(): string {
    return '(\\' + this.arg + ' -> ' + this.body + ')';
  }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    let [s1, tv] = state.fresh(this.arg, KType);
    return this.body.infer(env.extend(this.arg, TScheme.of(tv)), s1)
      .map(([s, t, c]) => [s, tarr(tv, t), c]);
  }
}
export function lam(args: string[], body: Expr) {
  return args.reduceRight((y, x) => new ELam(x, y), body);
}

export class EApp extends Expr {
  left: Expr;
  right: Expr;
  constructor(left: Expr, right: Expr) {
    super();
    this.left = left;
    this.right = right;
  }
  toString(): string {
    return '(' + this.left + ' ' + this.right + ')';
  }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    return this.left.infer(env, state)
      .then(([s1, t1, c1]) => this.right.infer(env, s1)
      .map(([s2, t2, c2]) => {
        let [s3, tv] = s2.fresh('r', KType);
        return <[InferState, Type, Constraint[]]>
          [s3, tv, c1.concat(c2).concat([new Unify(t1, tarr(t2, tv))])];
      }));
  }
}
export function app(...args: Expr[]) {
  if(args.length < 1) terr('app needs at least 1 argument');
  return args.reduce((x, y) => new EApp(x, y));
}

export class ELet extends Expr {
  arg: string;
  val: Expr;
  body: Expr;
  constructor(arg: string, val: Expr, body: Expr) {
    super();
    this.arg = arg;
    this.val = val;
    this.body = body;
  }
  toString(): string {
    return '(let ' + this.arg + ' = ' + this.val + ' in ' + this.body + ')';
  }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    return this.val.infer(env, state)
      .then(([s1, t1, c1]) => solve(c1)
      .then(sub => {
        let sc = t1.subst(sub).generalize(env.subst(sub), []);
        return this.body.infer(env.extend(this.arg, sc), s1)
          .map(([s2, t2, c2]) => [s2, t2, c1.concat(c2)]);
      }));
  }
}
export function lt(arg: string, val: Expr, body: Expr) {
  return new ELet(arg, val, body);
}

export class ERecordEmpty extends Expr {
  constructor() { super() }
  toString(): string { return '{}' }
  infer(env: Env, state: InferState) {
    return Result.ok<TypeError, [InferState, Type, Constraint[]]>([
      state,
      new TApp(Record, new TRowEmpty()),
      []
    ]);
  }
}
export function recempty() { return new ERecordEmpty() }

export class ESelect extends Expr {
  label: string;
  constructor(label: string) {
    super();
    this.label = label;
  }
  toString(): string { return '.' + this.label }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    let [st1, vt] = state.fresh('t', KType);
    let [st2, vr] = st1.fresh('r', KRow);
    return Result.ok<TypeError, [InferState, Type, Constraint[]]>([
      st2,
      tarr(new TApp(Record, new TRowExtend(this.label, vt, vr)), vt),
      [new Lacks(vr, this.label)],
    ]);
  }
}
export function sel(label: string) { return new ESelect(label) }

export class EExtend extends Expr {
  label: string;
  constructor(label: string) {
    super();
    this.label = label;
  }
  toString(): string { return '.+' + this.label }
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]> {
    let [st1, vt] = state.fresh('t', KType);
    let [st2, vr] = st1.fresh('r', KRow);
    return Result.ok<TypeError, [InferState, Type, Constraint[]]>([
      st2,
      tarr(vt, new TApp(Record, vr),
        new TApp(Record, new TRowExtend(this.label, vt, vr))),
      [new Lacks(vr, this.label)],
    ]);
  }
}
export function extend(label: string) { return new EExtend(label) }
