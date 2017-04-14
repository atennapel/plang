import Map from './Map';
import Set from './Set';
import Env from './Env';
import { Result, Err, Ok } from './Result';
import { clone } from './utils';

import { Kind } from './kinds';
import {
  Constraint,
  Unify,
} from './constraints';
import {
  Type,
  TVar,
  TCon,
  TApp,
  TScheme,
} from './types';
import {
  Expr,
  EVar,
} from './exprs';

export type Subst = Map<TVar, Type>;

export let emptySubst = Map.empty<TVar, Type>();

export interface Apply<T> {
  free(): Set<TVar>;
  subst(sub: Subst): T;
}

export interface Infer {
  infer(env: Env, state: InferState):
    Result<TypeError, [InferState, Type, Constraint[]]>;
}

export class InferState {
  tvars: {[id: string]: number}
  constructor(tvars: {[id: string]: number}) {
    this.tvars = tvars;
  }
  static empty() {
    return new InferState({});
  }
  fresh(name: string, kind: Kind): [InferState, TVar] {
    let tvars = clone(this.tvars);
    if(!tvars[name]) tvars[name] = 0;
    let id = tvars[name]++;
    return [
      new InferState(tvars),
      new TVar(name, name + id, kind)
    ];
  }
  toString(): string {
    let tvars =
      '{' + Object.keys(this.tvars)
        .map(k => k + ': ' + this.tvars[k]).join(', ') + '}';
    return 'InferState(' + tvars + ')';
  }
}

// first s1 and then s2
export function compose(s1: Subst, s2: Subst) {
  return s1.union(s2.map(v => v.subst(s1)));
}

export function solve(cs: Constraint[])
  : Result<TypeError, Subst> {
  let sorted = cs.slice().sort((x, y) => x.order() - y.order());
  console.log(sorted.join(' ; '))
  return sorted.reduce((res, c) =>
    res.then(s1 => c.subst(s1).check().then(s2 => Result.ok(compose(s1, s2)))),
    <Result<TypeError, Subst>> Result.ok(emptySubst));
}

export function infer(env: Env, expr: Expr, state_?: InferState)
  : Result<TypeError, Type> {
  let state = state_ || InferState.empty();
  return expr.infer(env, state)
    .then(([s, t, c]) => solve(c)
    .map(sub => t.subst(sub)));
}
