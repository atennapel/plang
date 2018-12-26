import { Name, TName, freshTName } from "./names";
import { err } from "./utils";
import { Kind, KType, KFun, kfun, KRow } from "./kinds";

export type Type = TConst | TMeta | TVar | TApp | TRowExtend;

export interface TConst {
  readonly tag: 'TConst';
  readonly name: Name;
  readonly kind: Kind;
}
export const TConst = (name: Name, kind: Kind = KType): TConst =>
  ({ tag: 'TConst', name, kind });
export const isTConst = (type: Type): type is TConst => type.tag === 'TConst';

export interface TMeta {
  readonly tag: 'TMeta';
  readonly name: TName;
  readonly kind: Kind;
  type: Type | null;
}
export const TMeta = (name: TName, kind: Kind, type: Type | null): TMeta =>
  ({ tag: 'TMeta', name, kind, type });
export const isTMeta = (type: Type): type is TMeta => type.tag === 'TMeta';

export interface TVar {
  readonly tag: 'TVar';
  readonly name: TName;
  readonly kind: Kind;
}
export const TVar = (name: TName, kind: Kind = KType): TVar =>
  ({ tag: 'TVar', name, kind });
export const isTVar = (type: Type): type is TVar => type.tag === 'TVar';

export interface TApp {
  readonly tag: 'TApp';
  readonly left: Type;
  readonly right: Type;
}
export const TApp = (left: Type, right: Type): TApp =>
  ({ tag: 'TApp', left, right });
export const isTApp = (type: Type): type is TApp => type.tag === 'TApp';
export const tappFrom = (es: Type[]): Type => es.reduce(TApp);
export const tapp = (...es: Type[]): Type => es.reduce(TApp);
export const flattenTApp = (type: Type): Type[] => {
  let c: Type = type;
  const r = [];
  while (isTApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};

export interface TRowExtend {
  readonly tag: 'TRowExtend';
  readonly label: Name;
  readonly type: Type;
  readonly rest: Type;
}
export const TRowExtend = (label: Name, type: Type, rest: Type): TRowExtend =>
  ({ tag: 'TRowExtend', label, type, rest });
export const isTRowExtend = (type: Type): type is TRowExtend => type.tag === 'TRowExtend';
export const flattenTRowExtend = (type: Type): { ts: [Name, Type][], rest: Type } => {
  let c = type;
  const ts: [Name, Type][] = [];
  while (isTRowExtend(c)) {
    ts.push([c.label, c.type]);
    c = c.rest;
  }
  return { ts, rest: c };
};

export const showType = (type: Type): string => {
  if (isTConst(type)) return type.name;
  if (isTMeta(type)) return `?${type.name}`;
  if (isTVar(type)) return `'${type.name}`;
  if (isTApp(type)) return `(${showType(type.left)} ${showType(type.right)})`;
  if (isTRowExtend(type)) return `(${type.label} : ${showType(type.type)} | ${showType(type.rest)})`;
  return err('unexpected type in showType');
};

export const freshTMeta = (kind: Kind = KType) => TMeta(freshTName(), kind, null);

export const TFun = TConst('->', kfun(KType, KType, KType));
export const tfun = (a: Type, b: Type): TApp => TApp(TApp(TFun, a), b);
export const tfunFrom = (ts: Type[]): Type => ts.reduceRight((a, b) => tfun(b, a));
export const tfuns = (...ts: Type[]): Type => ts.reduceRight((a, b) => tfun(b, a));
export const isTFun = (type: Type): { left: Type, right: Type } | null =>
  isTApp(type) && isTApp(type.left) && type.left.left === TFun ?
    ({ left: type.left.right, right: type.right }) : null;
export const flattenTFun = (type: Type): Type[] => {
  let c = type;
  const r: Type[] = [];
  let t = isTFun(c);
  while (t) {
    r.push(t.left);
    c = t.right;
    t = isTFun(c);
  }
  r.push(c);
  return r;
};

export const TFloat = TConst('Float');
export const TStr = TConst('Str');

export const TRowEmpty = TConst('()', KRow);

export const TRecord = TConst('Rec', kfun(KRow, KType));
export const TVariant = TConst('Var', kfun(KRow, KType));

export const TList = TConst('List', kfun(KType, KType));

export const initialTypes: { [key: string]: Type } = {
  Float: TFloat,
  Str: TStr,
  Rec: TRecord,
  Var: TVariant,
  List: TList,
};

export const trow = (ts: [Name, Type][], rest: Type = TRowEmpty): Type =>
  ts.reduceRight((r, [l, t]) => TRowExtend(l, t, r), rest);

const tvAlphabet = 'abcdefghijklmnopqrstuvwxyz';
export const prettyTypeR = (type: Type, map: { [key: number]: string }, index: { val: number }): string => {
  if (isTConst(type)) return type.name;
  if (isTMeta(type)) return `?${type.name}`;
  if (isTVar(type)) {
    if (!map[type.name]) {
      const l = tvAlphabet.length;
      const ix = index.val++;
      const rank = 0 | ix / l;
      map[type.name] = `${tvAlphabet[ix % l]}${rank > 0 ? rank : ''}`;
    }
    return map[type.name];
  }
  if (isTFun(type))
    return flattenTFun(type)
      .map(t => isTFun(t) ? `(${prettyTypeR(t, map, index)})` : prettyTypeR(t, map, index))
      .join(' -> ');
  if (isTApp(type))
    return flattenTApp(type)
      .map(t => isTApp(t) ? `(${prettyTypeR(t, map, index)})` : prettyTypeR(t, map, index))
      .join(' ');
  if (isTRowExtend(type)) {
    const fl = flattenTRowExtend(type);
    const head = fl.ts.map(([l, t]) => `${l} : ${prettyTypeR(t, map, index)}`).join(', ');
    return fl.rest === TRowEmpty ? `(${head})` : `(${head} | ${prettyTypeR(fl.rest, map, index)})`;
  }
  return err('unexpected type in prettyTypeR');
};
export const prettyType = (type: Type): string => {
  return prettyTypeR(type, {}, { val: 0 });
};
