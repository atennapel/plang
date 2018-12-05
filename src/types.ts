import { Name } from './names';

export type Type = TVar | TMeta | TFun;

export interface TVar { tag: 'TVar', name: Name };
export const TVar = (name: Name): Type => ({ tag: 'TVar', name });

export interface TMeta { tag: 'TMeta', name: Name };
export const TMeta = (name: Name): Type => ({ tag: 'TMeta', name });

export interface TFun { tag: 'TFun', left: Type, right: Type };
export const TFun = (left: Type, right: Type): Type => ({ tag: 'TFun', left, right });

export type CasesType<R> = {
  TVar: (name: Name) => R;
  TMeta: (name: Name) => R;
  TFun: (left: Type, right: Type) => R;
};
export const caseOf = <R>(val: Type, cs: CasesType<R>): R => {
  switch (val.tag) {
    case 'TVar': return cs.TVar(val.name);
    case 'TMeta': return cs.TMeta(val.name);
    case 'TFun': return cs.TFun(val.left, val.right);
  }
};

export const showType = (type: Type): string => caseOf(type, {
  TVar: name => `${name}`,
  TMeta: name => `^${name}`,
  TFun: (left, right) => `(${showType(left)} -> ${showType(right)})`,
});

export interface Forall { tag: 'Forall', args: Name[], type: Type };
export const Forall = (args: Name[], type: Type): Forall => ({ tag: 'Forall', args, type });

export const showForall = (forall: Forall) =>
  forall.args.length === 0 ?
    showType(forall.type) :
    `forall ${forall.args.join(' ')}. ${showType(forall.type)}`;
