import { Kind, KMeta } from './kinds';
import { TMeta, TSkol, Type, showTy } from './types';

export const impossible = (msg: string) => {
  throw new Error(`impossible: ${msg}`);
};
export const terr = (msg: string) => { throw new TypeError(msg) }

export type Name = string;

export type Id = number;
let _id = 0;
export const resetId = () => { _id = 0 };
export const freshId = () => _id++;

export const freshTMeta = (kind: Kind, name: Name | null = null) =>
  TMeta(freshId(), kind, name);
export const freshTSkol = (name: Name, kind: Kind) =>
  TSkol(name, freshId(), kind);
export const freshKMeta = () => KMeta(freshId());

export const skolemCheck = (sk: TSkol[], ty: Type): void => {
  if (ty.tag === 'TSkol' && sk.indexOf(ty) >= 0)
    return terr(`skolem check failed: ${showTy(ty)}`);
  if (ty.tag === 'TApp') {
    skolemCheck(sk, ty.left);
    return skolemCheck(sk, ty.right);
  }
  if (ty.tag === 'TForall')
    return skolemCheck(sk, ty.type);
};
