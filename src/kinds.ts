import { NameT, showName, Name, eqName } from './names';

export type Kind
  = KVar
  | KMeta
  | KFun;

export interface KVar {
  readonly tag: 'KVar';
  readonly name: NameT;
}
export const KVar = (name: NameT): KVar => ({ tag: 'KVar', name });
export const isKVar = (kind: Kind): kind is KVar => kind.tag === 'KVar';

export interface KMeta {
  readonly tag: 'KMeta';
  readonly name: NameT;
}
export const KMeta = (name: NameT): KMeta => ({ tag: 'KMeta', name });
export const isKMeta = (kind: Kind): kind is KMeta => kind.tag === 'KMeta';

export interface KFun {
  readonly tag: 'KFun';
  readonly left: Kind;
  readonly right: Kind;
}
export const KFun = (left: Kind, right: Kind): KFun => ({ tag: 'KFun', left, right });
export const isKFun = (kind: Kind): kind is KFun => kind.tag === 'KFun';
export const kfunFrom = (ks: Kind[]): Kind => ks.reduceRight((x, y) => KFun(y, x));
export const kfun = (...ks: Kind[]): Kind => kfunFrom(ks);

export const nType = Name('Type');
export const kType = KVar(nType);

export const flattenKFun = (kind: Kind): Kind[] => {
  let c = kind;
  const r: Kind[] = [];
  while (isKFun(c)) {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
};
export const showKind = (kind: Kind): string => {
   switch (kind.tag) {
     case 'KVar': return showName(kind.name);
     case 'KMeta': return `?${showName(kind.name)}`;
     case 'KFun':
      return flattenKFun(kind)
        .map(k => {
          const s = showKind(k);
          return isKFun(k) ? `(${s})` : s;
        })
        .join(' -> ');
   }
};

export const eqKind = (a: Kind, b: Kind): boolean => {
  switch (a.tag) {
    case 'KVar': return isKVar(b) && eqName(a.name, b.name);
    case 'KMeta': return isKMeta(b) && eqName(a.name, b.name);
    case 'KFun': return isKFun(b) && eqKind(a.left, b.left) && eqKind(a.right, b.right);
  }
};

export const containsKMeta = (x: NameT, kind: Kind): boolean => {
  switch (kind.tag) {
    case 'KVar': return false;
    case 'KMeta': return eqName(x, kind.name);
    case 'KFun': return containsKMeta(x, kind.left) || containsKMeta(x, kind.right);
  }
};
