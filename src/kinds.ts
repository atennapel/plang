import { Name } from './names';

export type Kind = KVar | KFun;

export interface KVar {
  readonly tag: 'KVar';
  readonly name: Name;
};
export const KVar = (name: Name): Kind => ({ tag: 'KVar', name });

export interface KFun {
  readonly tag: 'KFun';
  readonly left: Kind;
  readonly right: Kind;
};
export const KFun = (left: Kind, right: Kind): Kind => ({ tag: 'KFun', left, right });
export const kfun = (...ks: Kind[]): Kind => ks.reduceRight((a, b) => KFun(b, a));

export type CasesKind<R> = {
  KVar: (name: Name) => R;
  KFun: (left: Kind, right: Kind) => R;
};
export const caseKind = <R>(val: Kind, cs: CasesKind<R>): R => {
  switch (val.tag) {
    case 'KVar': return cs.KVar(val.name);
    case 'KFun': return cs.KFun(val.left, val.right);
  }
};

export const showKind = (type: Kind): string => caseKind(type, {
  KVar: name => `${name}`,
  KFun: (left, right) => `(${showKind(left)} -> ${showKind(right)})`,
});

export const eqKind = (a: Kind, b: Kind): boolean => caseKind(a, {
  KVar: name => b.tag === 'KVar' && b.name === name,
  KFun: (left, right) => b.tag === 'KFun' && eqKind(left, b.left) && eqKind(right, b.right),
});

export const nType = 'Type';
export const kType = KVar(nType);
