import { Forall, tFun, nFun } from "./types";
import { Kind, KFun, kfun, kType, nType } from "./kinds";
import { Name } from "./names";
import { Right, Left } from "./either";
import { TC, ok } from "./TC";

export type Map<T> = { [key: string]: T };
export const extend = <T>(map: Map<T>, key: string, val: T): Map<T> => {
  const ret = Object.create(map);
  ret[key] = val;
  return ret;
};
export const extendMut = <T>(map: Map<T>, key: string, val: T): Map<T> => {
  map[key] = val;
  return map;
};
export const append = <T>(a: Map<T>, b: Map<T>): Map<T> => {
  const n: Map<T> = {};
  for (let k in a) n[k] = a[k];
  for (let k in b) n[k] = b[k];
  return n;
};
export const appendMut = <T>(a: Map<T>, b: Map<T>): Map<T> => {
  for(let k in b) a[k] = b[k];
  return a;
};

export interface Context {
  readonly tag: 'Context';
  readonly kvars: Map<true>;
  readonly tvars: Map<Kind>;
  readonly vars: Map<Forall>;
}
export const Context = (
  kvars: Map<true> = {},
  tvars: Map<Kind> = {},
  vars: Map<Forall> = {},
): Context => ({ tag: 'Context', kvars, tvars, vars });

export const findKVar = (ctx: Context, name: Name): TC<true> =>
  ctx.kvars[name] ? ok : Left(`undefined kvar ${name}`);
export const findTVar = (ctx: Context, name: Name): TC<Kind> => {
  const ret = ctx.tvars[name];
  return ret ? Right(ret) : Left(`undefined tvar ${name}`);
};
export const findVar = (ctx: Context, name: Name): TC<Forall> => {
  const ret = ctx.vars[name];
  return ret ? Right(ret) : Left(`undefined var ${name}`);
};

export const extendVar = (ctx: Context, name: Name, type: Forall): Context =>
  Context(ctx.kvars, ctx.tvars, extend(ctx.vars, name, type));
export const extendContextMut = (ctx: Context, kvs: Map<true>, tvs: Map<Kind>, vs: Map<Forall>): Context => {
  appendMut(ctx.kvars, kvs);
  appendMut(ctx.tvars, tvs);
  appendMut(ctx.vars, vs);
  return ctx;
}

export const initial = Context(
  {
    [nType]: true,
  },
  {
    [nFun]: kfun(kType, kType, kType),
  },
  {

  },
);
