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
export const tapp = (...es: Type[]): Type => es.reduce(TApp);

export interface TRowExtend {
  readonly tag: 'TRowExtend';
  readonly label: Name;
  readonly type: Type;
  readonly rest: Type;
}
export const TRowExtend = (label: Name, type: Type, rest: Type): TRowExtend =>
  ({ tag: 'TRowExtend', label, type, rest });
export const isTRowExtend = (type: Type): type is TRowExtend => type.tag === 'TRowExtend';

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
export const tfuns = (...ts: Type[]): Type => ts.reduceRight((a, b) => tfun(b, a));

export const TRowEmpty = TConst('RowEmpty', KRow);

export const TRecord = TConst('Rec', kfun(KRow, KType));
export const TVariant = TConst('Var', kfun(KRow, KType));

export const trow = (ts: [Name, Type][], rest: Type = TRowEmpty): Type =>
  ts.reduceRight((r, [l, t]) => TRowExtend(l, t, r), rest);
