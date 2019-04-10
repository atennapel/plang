import { Type, TMeta, tmetas, prune, TSkol, showTy } from './types';
import { Kind, KFun, kType, showKind } from './kinds';
import List from './List';
import { Name, skolemCheck } from './util';

export interface Env {
  global: { [key: string]: Type };
  tcons: { [key: string]: Kind };
  local: List<[string, Type]>;
}
export const Env = (
  global: { [key: string]: Type } = {},
  tcons: { [key: string]: Kind } = {},
  local: List<[string, Type]> = List.nil(),
) => ({ global, tcons, local });

const clone = <T>(o: { [key: string]: T }): { [key: string] : T } => {
  const n: { [key: string]: T} = {};
  for (let k in o) n[k] = o[k];
  return n;
};

export const cloneEnv = (e: Env) =>
  Env(clone(e.global), clone(e.tcons), e.local);

export const showEnv = (env: Env) => {
  const r: string[] = [];
  for (let k in env.tcons)
    r.push(`type ${k} : ${showKind(env.tcons[k])}`);
  for (let k in env.global)
    r.push(`${k} : ${showTy(env.global[k])}`);
  return r.join('\n');
};

export const extendVar = (env: Env, x: Name, t: Type): Env =>
  Env(env.global, env.tcons, List.cons([x, t], env.local));
export const extendVars = (env: Env, vs: [Name, Type][]): Env => {
  const local = vs.reduce((l, kv) => List.cons(kv, l), env.local);
  return Env(env.global, env.tcons, local);
};
export const lookupVar = (env: Env, x: Name): Type | null => {
  const t = env.local.first(([k, _]) => x === k);
  if (t) return t[1];
  return env.global[x] || null;
};
export const lookupTCon = (env: Env, x: Name): Kind | null =>
  env.tcons[x] || null;

export const skolemCheckEnv = (sk: TSkol[], env: Env): void => {
  env.local.each(([_, t]) => skolemCheck(sk, prune(t)));
  const vars = env.global;
  for (let k in vars) skolemCheck(sk, prune(vars[k]));
};

export const tmetasEnv = (
  env: Env,
  free: TMeta[] = [],
  tms: TMeta[] = [],
): TMeta[] => {
  env.local.each(([_, t]) => tmetas(prune(t), free, tms));
  const vars = env.global;
  for (let k in vars) tmetas(prune(vars[k]), free, tms);
  return tms;
};

const initialEnv = Env(
  {},
  {
    '->': KFun(kType, KFun(kType, kType)),
  },
);

export const getInitialEnv = () => cloneEnv(initialEnv);
