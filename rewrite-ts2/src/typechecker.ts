import Map, { KVPair } from './Map';
import Set from './Set';
import { clone } from './utils';
import { Result, Ok, Err } from './Result';

import { Kind, ktype, krow } from './kinds';
import {
  Type,
  TVar,
  tvar,
  isTVar,
  isTCon,
  isTApp,
  tapp,
  TScheme,
  isTScheme,
  tscheme,
  typeStr,
  tarr2,
  isTMu,
  tmu,
} from './types';
import {
  Expr,
  isEVar,
  isEApp,
  isELam,
  isELet,
} from './exprs';
import Env from './Env';

type TVars = Set<TVar>;
type Subst = Map<TVar, Type>;

type InferResult<V> = Result<TypeError, V>

const emptySubst : Subst = Map.empty<TVar, Type>();

const compose = (s1: Subst, s2: Subst) => {
  return s1.union(s2.map(v => subst(s1, v)));
}

const free = (t: Type | TScheme | Env): TVars => {
  if(t instanceof Env) {
    let s = Set.empty<TVar>();
    for(let k in t.imap)
      s = s.union(free(t.imap[k]));
    return s;
  }
  if(isTScheme(t)) return free(t.type).without(Set.from(t.vars));
  if(isTVar(t)) return Set.of(t);
  if(isTCon(t)) return Set.empty<TVar>();
  if(isTApp(t)) return free(t.left).union(free(t.right));
  if(isTMu(t)) return free(t.type).without(Set.of(t.arg));
  throw new Error('impossible');
};

const subst = (sub: Subst, t: Type): Type => {
  if(isTVar(t)) return sub.getOr(t, t);
  if(isTCon(t)) return t;
  if(isTApp(t)) return tapp(subst(sub, t.left), subst(sub, t.right));
  if(isTMu(t)) return tmu(t.arg, subst(sub.removeKey(t.arg), t.type));
  throw new Error('impossible');
};
const substScheme = (sub: Subst, t: TScheme): TScheme =>
  tscheme(t.vars, subst(sub.removeKeysArray(t.vars), t.type));
const substEnv = (sub: Subst, e: Env): Env => e.map(v => substScheme(sub, v));

const bind = (v: TVar, t: Type): InferResult<Subst> => {
  if(isTVar(t) && v.id === t.id) return Result.ok(emptySubst);
  if(free(t).contains(v))
    return Result.ok(Map.of([v, tmu(v, t)]));
  return Result.ok(Map.of([v, t]));
};

const unify = (a: Type, b: Type): InferResult<Subst> => {
  console.log(`unify ${typeStr(a)} and ${typeStr(b)}`);
  if(isTVar(a)) return bind(a, b);
  if(isTVar(b)) return bind(b, a);
  if(isTCon(a) && isTCon(b) && a.name === b.name) return Result.ok(emptySubst);
  if(isTApp(a) && isTApp(b))
    return unify(a.left, b.left)
      .then(s1 => unify(subst(s1, a.right), subst(s1, b.right))
      .then(s2 => Result.ok(compose(s1, s2))));
  return Result.err(new TypeError(
    `Cannot unify ${typeStr(a)} and ${typeStr(b)}`));
};

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
      tvar(name, name + id, kind)
    ];
  }
  toString(): string {
    let tvars =
      '{' + Object.keys(this.tvars)
        .map(k => k + ': ' + this.tvars[k]).join(', ') + '}';
    return 'InferState(' + tvars + ')';
  }
}

const generalize = (t: Type, e: Env) =>
  tscheme(free(t).without(free(e)).vals(), t);

const instantiate = (state: InferState, t: TScheme)
  : [InferState, Type] => {
  let res = t.vars.reduce((st, tv) => {
    let r = st[0].fresh(tv.name, tv.kind);
    return <[InferState, KVPair<TVar, Type>[]]>
      [r[0], st[1].concat([[tv, r[1]]])];
  }, <[InferState, KVPair<TVar, Type>[]]> [state, []]);
  let sub = Map.from(res[1]);
  return [
    res[0],
    subst(sub, t.type)
  ];
};

export const inferR = (env: Env, st: InferState, e: Expr)
  : InferResult<[InferState, Subst, Type]> => {
  if(isEVar(e)) {
    if(!env.contains(e.name))
      return Result.err(new TypeError('Undefined variable: ' + e.name));
    const [st1, t] = instantiate(st, env.get(e.name));
    return Result.ok<TypeError, [InferState, Subst, Type]>
      ([st1, emptySubst, t]);
  }
  if(isEApp(e)) {
    const [st1, tv] = st.fresh('r', ktype);
    return inferR(env, st1, e.left)
      .then(([st2, s1, tfun]) => inferR(env, st2, e.right)
      .then(([st3, s2, targ]) => {
        const s3 = compose(s1, s2);
        return unify(subst(s3, tfun), subst(s3, tarr2(targ, tv)))
          .then(s4 => {
            const s5 = compose(s3, s4);
            return Result.ok([st3, s5, subst(s5, tv)]);
          });
      }));
  }
  if(isELam(e)) {
    const [st1, tv] = st.fresh(e.arg, ktype);
    const nenv = env.extend(e.arg, tscheme([], tv));
    return inferR(nenv, st1, e.body)
      .then(([st2, s1, tbody]) =>
        Result.ok([st2, s1, subst(s1, tarr2(tv, tbody))]));
  }
  if(isELet(e)) {
    return inferR(env, st, e.val)
      .then(([st1, s1, tval]) => {
        const genv = substEnv(s1, env);
        const nenv = env.extend(e.arg, generalize(tval, genv));
        return inferR(nenv, st1, e.body)
          .then(([st2, s2, tbody]) => {
            const s3 = compose(s2, s2);
            return Result.ok([st2, s3, subst(s3, tbody)]);
          });
      });
  }
  throw new Error('impossible');
}

export const infer = (e: Expr, env?: Env, state?: InferState)
  : InferResult<Type> =>
  inferR(env || Env.empty(), state || InferState.empty(), e)
    .map(([_, s, t]) => subst(s, t));
