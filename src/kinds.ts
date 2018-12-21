import { Name, TName, freshTName } from "./names";
import { err } from "./utils";

export type Kind = KConst | KMeta | KFun;

export interface KConst {
  readonly tag: 'KConst';
  readonly name: Name;
}
export const KConst = (name: Name): KConst =>
  ({ tag: 'KConst', name });
export const isKConst = (kind: Kind): kind is KConst => kind.tag === 'KConst';

export interface KMeta {
  readonly tag: 'KMeta';
  readonly name: TName;
  kind: Kind | null;
}
export const KMeta = (name: TName, kind: Kind | null): KMeta =>
  ({ tag: 'KMeta', name, kind });
export const isKMeta = (kind: Kind): kind is KMeta => kind.tag === 'KMeta';

export interface KFun {
  readonly tag: 'KFun';
  readonly left: Kind;
  readonly right: Kind;
}
export const KFun = (left: Kind, right: Kind): KFun =>
  ({ tag: 'KFun', left, right });
export const isKFun = (kind: Kind): kind is KFun => kind.tag === 'KFun';
export const kfun = (...ks: Kind[]): Kind => ks.reduceRight((a, b) => KFun(b, a));

export const showKind = (type: Kind): string => {
  if (isKConst(type)) return type.name;
  if (isKMeta(type)) return `?${type.name}`;
  if (isKFun(type)) return `(${showKind(type.left)} -> ${showKind(type.right)})`;
  return err('unexpected kind in showKind');
};

export const freshKMeta = () => KMeta(freshTName(), null);

export const KType = KConst('Type');
export const KRow = KConst('Row');
