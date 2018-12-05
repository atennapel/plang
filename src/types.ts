import { Name } from './names';
import { ADT, cases } from './utils';

export type TypeTag = { TVar: TVar, TMeta: TMeta, TFun: TFun };
export type Type = ADT<TypeTag>;

export type TVar = { tag: 'TVar', name: Name };
export const TVar = (name: Name): Type => ({ tag: 'TVar', name });

export type TMeta = { tag: 'TMeta', name: Name };
export const TMeta = (name: Name): Type => ({ tag: 'TMeta', name });

export type TFun = { tag: 'TFun', left: Type, right: Type };
export const TFun = (left: Type, right: Type): Type => ({ tag: 'TFun', left, right });

export const caseType = cases<TypeTag>();

export const showType = (type: Type): string => caseType(type, {
  TVar: val => `${val.name}`,
  TMeta: val => `^${val.name}`,
  TFun: val => `(${showType(val.left)} -> ${showType(val.right)})`,
});

export type Forall = { tag: 'Forall', args: Name[], type: Type };
export const Forall = (args: Name[], type: Type): Forall => ({ tag: 'Forall', args, type });

export const showForall = (forall: Forall) =>
  forall.args.length === 0 ?
    showType(forall.type) :
    `forall ${forall.args.join(' ')}. ${showType(forall.type)}`;
