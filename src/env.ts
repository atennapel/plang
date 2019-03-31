import { Type, TMeta, tmetas, prune, TSkol } from './types';
import { Kind, KFun, kType } from './kinds';
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

export const extendVar = (env: Env, x: Name, t: Type): Env =>
  Env(env.global, env.tcons, List.cons([x, t], env.local));
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

export const initialEnv = Env(
  {},
  {
    '->': KFun(kType, KFun(kType, kType)),
  },
);
