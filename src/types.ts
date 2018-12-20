import { Name, TName, freshTName } from "./names";
import { err } from "./utils";

export type Type = TConst | TMeta | TVar | TApp;

export interface TConst {
  readonly tag: 'TConst';
  readonly name: Name;
}
export const TConst = (name: Name): TConst =>
  ({ tag: 'TConst', name });
export const isTConst = (type: Type): type is TConst => type.tag === 'TConst';

export interface TMeta {
  readonly tag: 'TMeta';
  readonly name: TName;
  type: Type | null;
}
export const TMeta = (name: TName, type: Type | null): TMeta =>
  ({ tag: 'TMeta', name, type });
export const isTMeta = (type: Type): type is TMeta => type.tag === 'TMeta';

export interface TVar {
  readonly tag: 'TVar';
  readonly name: TName;
}
export const TVar = (name: TName): TVar =>
  ({ tag: 'TVar', name });
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

export const showType = (type: Type): string => {
  if (isTConst(type)) return type.name;
  if (isTMeta(type)) return `?${type.name}`;
  if (isTVar(type)) return `'${type.name}`;
  if (isTApp(type)) return `(${showType(type.left)} ${showType(type.right)})`;
  return err('unexpected type in showType');
};

export const freshTMeta = () => TMeta(freshTName(), null);

export const TFun = TConst('->');
export const tfun = (a: Type, b: Type): TApp => TApp(TApp(TFun, a), b);
export const tfuns = (...ts: Type[]): Type => ts.reduceRight((a, b) => tfun(b, a));
