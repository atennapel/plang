import { Forall, tFun, nFun, nEffsEmpty, nEffsExtend, Type, TVar } from "./types";
import { Kind, KFun, kfun, kType, nType, nEffs, nEff, kEffs, kEff } from "./kinds";
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
  readonly effs: Map<Map<true>>;
  readonly ops: Map<{ eff: Name, param: Type, ret: Type }>;
  readonly vars: Map<Forall>;
}
export const Context = (
  kvars: Map<true> = {},
  tvars: Map<Kind> = {},
  effs: Map<Map<true>> = {},
  ops: Map<{ eff: Name, param: Type, ret: Type }> = {},
  vars: Map<Forall> = {},
): Context => ({ tag: 'Context', kvars, tvars, effs, ops, vars });

export const findKVar = (ctx: Context, name: Name): TC<true> =>
  ctx.kvars[name] ? ok : Left(`undefined kvar ${name}`);
export const findTVar = (ctx: Context, name: Name): TC<Kind> => {
  const ret = ctx.tvars[name];
  return ret ? Right(ret) : Left(`undefined tvar ${name}`);
};
export const findEff = (ctx: Context, name: Name): TC<Map<true>> => {
  const ret = ctx.effs[name];
  return ret ? Right(ret) : Left(`undefined eff ${name}`);
};
export const findOp = (ctx: Context, name: Name): TC<{ eff: Name, param: Type, ret: Type }> => {
  const ret = ctx.ops[name];
  return ret ? Right(ret) : Left(`undefined eff ${name}`);
};
export const findVar = (ctx: Context, name: Name): TC<Forall> => {
  const ret = ctx.vars[name];
  return ret ? Right(ret) : Left(`undefined var ${name}`);
};

export const extendVar = (ctx: Context, name: Name, type: Forall): Context =>
  Context(ctx.kvars, ctx.tvars, ctx.effs, ctx.ops, extend(ctx.vars, name, type));
export const extendContextMut = (
  ctx: Context,
  kvs: Map<true>,
  tvs: Map<Kind>,
  effs: Map<Map<true>>,
  ops: Map<{ eff: Name, param: Type, ret: Type }>,
  vs: Map<Forall>
): Context => {
  appendMut(ctx.kvars, kvs);
  appendMut(ctx.tvars, tvs);
  appendMut(ctx.effs, effs);
  appendMut(ctx.ops, ops);
  appendMut(ctx.vars, vs);
  return ctx;
}

export const initial = Context(
  {
    [nType]: true,
    [nEff]: true,
    [nEffs]: true,
  },
  {
    [nFun]: kfun(kType, kEffs, kType, kType),
    [nEffsEmpty]: kEffs,
    [nEffsExtend]: kfun(kEff, kEffs, kEffs),
    Unit: kType,
  },
  {},
  {},
  {
    Unit: Forall([], TVar('Unit')),
  },
);
