import { Forall } from "./types";
import { Kind } from "./kinds";
import { Maybe, Just, Nothing } from "./maybe";
import { Name } from "./names";

export type Map<T> = { [key: string]: T };
export const extend = <T>(map: Map<T>, key: string, val: T): Map<T> => {
  const ret = Object.create(map);
  ret[key] = val;
  return ret;
};

export interface Context {
  readonly tag: 'Context';
  readonly kvars: Map<true>;
  readonly tvars: Map<Kind>;
  readonly metas: Map<Kind>;
  readonly vars: Map<Forall>;
}
export const Context = (
  kvars: Map<true> = {},
  tvars: Map<Kind> = {},
  metas: Map<Kind> = {},
  vars: Map<Forall> = {},
): Context => ({ tag: 'Context', kvars, tvars, metas, vars });

export const findKVar = (ctx: Context, name: Name): Maybe<true> =>
  ctx.kvars[name] ? Just(true as true) : Nothing();
export const findTVar = (ctx: Context, name: Name): Maybe<Kind> => {
  const ret = ctx.tvars[name];
  return ret ? Just(ret) : Nothing();
};
export const findMeta = (ctx: Context, name: Name): Maybe<Kind> => {
  const ret = ctx.metas[name];
  return ret ? Just(ret) : Nothing();
};
export const findVar = (ctx: Context, name: Name): Maybe<Forall> => {
  const ret = ctx.vars[name];
  return ret ? Just(ret) : Nothing();
};

export const extendMeta = (ctx: Context, name: Name, kind: Kind): Context =>
  Context(ctx.kvars, ctx.tvars, extend(ctx.metas, name, kind), ctx.vars);
export const extendVar = (ctx: Context, name: Name, type: Forall): Context =>
  Context(ctx.kvars, ctx.tvars, ctx.metas, extend(ctx.vars, name, type));
