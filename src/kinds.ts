import { impossible } from './errors';
import { Name, showName, eqName } from './names';

export type Kind
  = KVar
  | KMeta
  | KFun;

export interface KVar {
  readonly tag: 'KVar';
  readonly name: Name;
}
export const KVar = (name: Name): KVar =>
  ({ tag: 'KVar', name });
export const isKVar = (kind: Kind): kind is KVar =>
  kind.tag === 'KVar';
export const matchKVar = (name: Name) => (kind: Kind): kind is KVar =>
  isKVar(kind) && eqName(kind.name, name);

export interface KMeta {
  readonly tag: 'KMeta';
  readonly name: Name;
}
export const KMeta = (name: Name): KMeta =>
  ({ tag: 'KMeta', name });
export const isKMeta = (kind: Kind): kind is KMeta =>
  kind.tag === 'KMeta';
export const matchKMeta = (name: Name) => (kind: Kind): kind is KMeta =>
  isKMeta(kind) && eqName(kind.name, name);

export interface KFun {
  readonly tag: 'KFun';
  readonly left: Kind;
  readonly right: Kind;
}
export const KFun = (left: Kind, right: Kind): KFun =>
  ({ tag: 'KFun', left, right });
export const isKFun = (kind: Kind): kind is KFun =>
  kind.tag === 'KFun';
export const kfunFrom = (ks: Kind[]): Kind =>
  ks.reduceRight((x, y) => KFun(y, x));
export const kfun = (...ks: Kind[]): Kind =>
  kfunFrom(ks);
export const flattenKFun = (kind: Kind): Kind[] => {
  const r: Kind[] = [];
  let c = kind;
  while (isKFun(c)) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
};

export const showKind = (kind: Kind): string => {
  if (isKVar(kind)) return `${showName(kind.name)}`;
  if (isKMeta(kind)) return `?${showName(kind.name)}`;
  if (isKFun(kind)) return `(${showKind(kind.left)} -> ${showKind(kind.right)})`;
  return impossible('showKind');
};

export const prettyKind = (kind: Kind): string => {
  if (isKVar(kind)) return `${showName(kind.name)}`;
  if (isKMeta(kind)) return `?${showName(kind.name)}`;
  if (isKFun(kind))
    return flattenKFun(kind)
      .map(k => isKFun(k) ? `(${prettyKind(k)})` : prettyKind(k))
      .join(' -> ');
  return impossible('prettyKind');
};
