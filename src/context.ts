import { Elem, showElem, CMarker, matchCMarker } from './elems';
import { err } from './errors';
import { freshName, Plain } from './names';

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

export const withElems = <T>(es: Elem[], fn: () => T): T => {
  const m = freshName(Plain('m'));
  add(CMarker(m));
  addAll(es);
  const val = fn();
  split(matchCMarker(m));
  return val;
};
