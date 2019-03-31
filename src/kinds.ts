import { Name, Id, impossible } from './util';

export type Kind
  = KCon
  | KFun
  | KMeta;

export interface KCon {
  readonly tag: 'KCon';
  readonly name: Name;
}
export const KCon = (name: Name): KCon => ({ tag: 'KCon', name });

export interface KFun {
  readonly tag: 'KFun';
  readonly left: Kind;
  readonly right: Kind;
}
export const KFun = (left: Kind, right: Kind): KFun =>
  ({ tag: 'KFun', left, right });
export const kfunFrom = (ks: Kind[]): Kind =>
  ks.reduceRight((x, y) => KFun(y, x));

export interface KMeta {
  readonly tag: 'KMeta';
  readonly id: Id;
  kind: Kind | null;
}
export const KMeta = (id: Id): KMeta =>
  ({ tag: 'KMeta', id, kind: null });

export const kType = KCon('Type');

export const flattenKFun = (ki: Kind): Kind[] => {
  const r: Kind[] = [];
  let c = ki;
  while (c.tag === 'KFun') {
    r.push(c.left);
    c = c.right;
  }
  r.push(c);
  return r;
};

export const showKind = (ki: Kind): string => {
  if (ki.tag === 'KCon') return ki.name;
  if (ki.tag === 'KFun')
    return flattenKFun(ki)
      .map(k => k.tag === 'KFun' ? `(${showKind(k)})` : showKind(k))
      .join(' -> ');
  if (ki.tag === 'KMeta') return `?${ki.id}`;
  return impossible('showKind');
};

export const pruneKind = (ki: Kind): Kind => {
  switch (ki.tag) {
    case 'KFun':
      return KFun(pruneKind(ki.left), pruneKind(ki.right));
    case 'KMeta': {
      if (!ki.kind) return ki;
      const k = pruneKind(ki.kind);
      ki.kind = k;
      return k;
    }
    default: return ki;
  }
}

export const occursKMeta = (x: KMeta, k: Kind): boolean => {
  if (x === k) return true;
  if (k.tag === 'KFun')
    return occursKMeta(x, k.left) || occursKMeta(x, k.right);
  return false;
};

export const eqKind = (a: Kind, b: Kind): boolean => {
  if (a === b) return true;
  if (a.tag === 'KCon')
    return b.tag === 'KCon' && a.name === b.name;
  if (a.tag === 'KFun')
    return b.tag === 'KFun' && eqKind(a.left, b.left)
      && eqKind(a.right, b.right);
  return false;
};
