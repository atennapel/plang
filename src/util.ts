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

const HEX = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
export const binToHex = (s: string): string => {
  const r: string[] = [];
  for (let i = 0, l = s.length; i < l; i += 4) {
    let t = s.slice(i, i + 4);
    if (t.length === 0) t = '0000';
    else if (t.length === 1) t = `${t}000`;
    else if (t.length === 2) t = `${t}00`;
    else if (t.length === 3) t = `${t}0`;
    const x = parseInt(t, 2);
    r.push(HEX[x]);
  }
  return r.join('');
};

export const binToASCII = (s: string): string => {
  const r: string[] = [];
  for (let i = 0, l = s.length; i < l; i += 7) {
    let t = s.slice(i, i + 7);
    if (t.length === 0) t = '0000000';
    else if (t.length === 1) t = `${t}000000`;
    else if (t.length === 2) t = `${t}00000`;
    else if (t.length === 3) t = `${t}0000`;
    else if (t.length === 4) t = `${t}000`;
    else if (t.length === 5) t = `${t}00`;
    else if (t.length === 6) t = `${t}0`;
    r.push(String.fromCharCode(parseInt(t, 2)));
  }
  return r.join('');
};
export const binToBase64 = (s: string): string => btoa(binToASCII(s));
