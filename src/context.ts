import { Elem, showElem, CMarker, matchCMarker, isCTMeta, matchCTMeta } from './elems';
import { err, impossible } from './errors';
import { freshName, Plain, Name, eqName } from './names';
import { Type, isTVar, isTMeta, isTFun, isTForall, TFun, TForall } from './types';

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
    const e = findElem(matchCTMeta(type.name));
    return e.type ? apply(e.type) : type;
  }
  if (isTFun(type)) return TFun(apply(type.left), apply(type.right));
  if (isTForall(type)) return TForall(type.name, apply(type.type));
  return impossible('apply');
};
