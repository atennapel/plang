import { Elem, showElem, CMarker, matchCMarker, isCTMeta, matchCTMeta, isUnsolved, isTMetaUnsolved, matchCKMeta } from './elems';
import { err, impossible } from './errors';
import { freshName, Plain, Name, eqName, showName } from './names';
import { Type, isTVar, isTMeta, isTFun, isTForall, TFun, TForall, showType, freeTMeta } from './types';
import { log } from './logging';
import { isKVar, isKMeta, KFun, Kind, isKFun, showKind } from './kinds';

export type Context = Elem[];

export let context: Context = [];
export const setContext = (ctx: Context = []): Context => {
  context = ctx;
  return context;
};

export const showContext = (ctx: Context) =>
  `[${ctx.map(showElem).join(', ')}]`;

export const add = (e: Elem) => context.push(e);
export const addAll = (es: Elem[]) => {
  for (let i = 0, l = es.length; i < l; i++)
    context.push(es[i]);
};
export const removeN = (n: number): Context =>
  context.splice(-n, n);

export const findIndex = (fn: (elem: Elem) => boolean): number => {
  for (let i = context.length - 1; i >= 0; i--) {
    const elem = context[i];
    if (fn(elem)) return i;
  }
  return -1;
};
export const findElem = <T extends Elem>(fn: (elem: Elem) => elem is T, msg: string = 'elem not found in findElem'): T => {
  for (let i = context.length - 1; i >= 0; i--) {
    const elem = context[i];
    if (fn(elem)) return elem;
  }
  return err(msg);
};
export const findElems = <T>(fn: (elem: Elem) => T | null, ctx: Context = context): T[] => {
  const r: T[] = [];
  for (let i = 0, l = ctx.length; i < l; i++) {
    const elem = ctx[i];
    const res = fn(elem);
    if (res) r.push(res);
  }
  return r;
};
export const findElemNot = (fn: (elem: Elem) => boolean, msg: string = 'elem found in findElemNot'): void => {
  const i = findIndex(fn);
  if (i >= 0) return err(msg);
};
export const split = (fn: (elem: Elem) => boolean, msg: string = 'elem not found in split'): Context => {
  const i = findIndex(fn);
  if (i < 0) return err(msg);
  const ret = context.splice(i);
  ret.shift();
  return ret;
};

export const replace = (fn: (elem: Elem) => boolean, es: Elem[], msg: string = 'elem not found in replace'): void => {
  const i = findIndex(fn);
  if (i < 0) return err(msg);
  const right = split(fn);
  addAll(es);
  addAll(right);
};

export const withElems = <T>(es: Elem[], fn: () => T): T => {
  const m = freshName(Plain('m'));
  add(CMarker(m));
  addAll(es);
  const val = fn();
  split(matchCMarker(m));
  return val;
};
export const withElemsContext = <T>(es: Elem[], fn: () => T): [T, Context] => {
  const m = freshName(Plain('m'));
  add(CMarker(m));
  addAll(es);
  const val = fn();
  const right = split(matchCMarker(m));
  return [val, right];
};

export const comesBefore = (a: Name, b: Name): boolean => {
  for (let i = 0, l = context.length; i < l; i++) {
    const e = context[i];
    if (isCTMeta(e) && eqName(e.name, a)) return true;
    if (isCTMeta(e) && eqName(e.name, b)) return false;
  }
  return false;
};

export const apply = (type: Type): Type => {
  if (isTVar(type)) return type;
  if (isTMeta(type)) {
    const e = findElem(matchCTMeta(type.name), `apply: ${showType(type)} not found in context`);
    return e.type ? apply(e.type) : type;
  }
  if (isTFun(type)) return TFun(apply(type.left), apply(type.right));
  if (isTForall(type)) return TForall(type.name, applyKind(type.kind), apply(type.type));
  return impossible('apply');
};

export const applyKind = (kind: Kind): Kind => {
  if (isKVar(kind)) return kind;
  if (isKMeta(kind)) {
    const e = findElem(matchCKMeta(kind.name), `applyKind: ${showKind(kind)} not found in context`);
    return e.kind ? applyKind(e.kind) : kind;
  }
  if (isKFun(kind)) return KFun(applyKind(kind.left), applyKind(kind.right));
  return impossible('applyKind');
};

export const isComplete = () => findIndex(isUnsolved) < 0;

export const unsolvedTMetas = (ctx: Context = context): [Name, Kind][] =>
  findElems(e => isTMetaUnsolved(e) ? [e.name, e.kind] as [Name, Kind] : null, ctx);

export const unsolvedTMetasInType = (type: Type, ctx: Context = context): [Name, Kind][] => {
  const u = unsolvedTMetas(ctx);
  const f = freeTMeta(type);
  const ret: [Name, Kind][] = [];
  for (let i = 0, l = f.length; i < l; i++) {
    const x = f[i];
    let found: Kind | null = null;
    for (let j = 0, k = u.length; j < k; j++) {
      const y = u[j];
      if (eqName(x, y[0])) {
        found = y[1];
        break;
      }
    }
    if (found) ret.push([x, found]);
  }
  return ret;
};
