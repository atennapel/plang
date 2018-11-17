import Context from './generic/context';
import { kvar, kfun, kfuns } from './kinds';
import NameRep, { name } from './generic/NameRep';
import Elem, { ckvar, ctvar } from './elems';
import Type, { tvar, TApp, tapps, isTApp, isTVar, TVar } from './types';
import { TypeN } from './TC';

export const nType = name('Type');
export const kType = kvar(nType);

export const nFun = name('->');
export const tFun = tvar(nFun);

export const tfun = (left: TypeN, right: TypeN) => tapps(tFun, left, right);
export const tfunFrom = (ts: TypeN[]) => ts.reduceRight((x, y) => tfun(y, x));
export function tfuns(...ts: TypeN[]) { return tfunFrom(ts) }
export const matchTFun = (type: TypeN): { left: TypeN, right: TypeN } | null =>
  isTApp(type) && isTApp(type.left) && isTVar(type.left.left) && type.left.left.name.equals(nFun) ?
    { left: type.left.right, right: type.right } :
    null;

export const initialContext = Context.of<Elem<NameRep>>(
  ckvar(nType),

  ctvar(nFun, kfuns(kType, kType, kType)),
);
