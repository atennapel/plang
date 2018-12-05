import * as Elem from './elems';

type Elem = Elem.Elem;

export type IndexMap = { [key: string]: number };
export interface Context {
  tag: 'Context';
  elems: Elem[];
  tvars: IndexMap;
  metas: IndexMap;
  vars: IndexMap;
  markers: IndexMap;
}
export const context = (
  elems: Elem[] = [],
  tvars: IndexMap = {},
  metas: IndexMap = {},
  vars: IndexMap = {},
  markers: IndexMap = {},
): Context => ({ tag: 'Context', elems, tvars, metas, vars, markers });

export const show = (ctx: Context): string => `[${ctx.elems.map(Elem.show).join(', ')}]`;

// mutable methods
export const add = (ctx: Context, elem: Elem): Context => {
  
  return ctx;
};
