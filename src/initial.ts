import Context from './generic/context';
import { kvar, kfun, kfuns } from './kinds';
import NameRep, { name } from './generic/NameRep';
import Elem, { ckvar, ctvar } from './elems';
import Type, { tvar, TApp, tapps, isTApp, isTVar, TVar, trowempty } from './types';
import { TypeN } from './TC';

export const nType = name('Type');
export const kType = kvar(nType);

export const nRow = name('Row');
export const kRow = kvar(nRow);

export const nFun = name('->');
export const tFun = tvar(nFun);

export const nComp = name('Comp');
export const kComp = kvar(nComp);

export const tfun = (left: TypeN, right: TypeN) => tapps(tFun, left, right);
export const tfunFrom = (ts: TypeN[]) => ts.reduceRight((x, y) => tfun(y, x));
export function tfuns(...ts: TypeN[]) { return tfunFrom(ts) }
export const matchTFun = (type: TypeN): { left: TypeN, right: TypeN } | null =>
  isTApp(type) && isTApp(type.left) && isTVar(type.left.left) && type.left.left.name.equals(nFun) ?
    { left: type.left.right, right: type.right } :
    null;

export const tComp = tvar(nComp);
export const tcomp = (type: TypeN, eff: TypeN) => tapps(tComp, type, eff);

export const nFunE = name('FnEff');
export const tFunE = tvar(nFunE);

export const tfunE = (left: TypeN, right: TypeN) => tapps(tFunE, left, right);
export const tfunEFrom = (ts: TypeN[]) => ts.reduceRight((x, y) => tfunE(y, x));
export function tfunEs(...ts: TypeN[]) { return tfunEFrom(ts) }
export const matchTFunE = (type: TypeN): { left: TypeN, right: TypeN } | null =>
  isTApp(type) && isTApp(type.left) && isTVar(type.left.left) && type.left.left.name.equals(nFunE) ?
    { left: type.left.right, right: type.right } :
    null;

export const nRec = name('Rec');
export const tRec = tvar(nRec);
export const matchTRec = (type: TypeN): TypeN | null =>
  isTApp(type) && isTVar(type.left) && type.left.name.equals(nRec) ?
    type.right :
    null;

export const nVar = name('Var');
export const tVar = tvar(nVar);
export const matchTVariant = (type: TypeN): TypeN | null =>
  isTApp(type) && isTVar(type.left) && type.left.name.equals(nVar) ?
    type.right :
    null;

export const initialContext = Context.of<Elem<NameRep>>(
  ckvar(nType),
  ckvar(nRow),
  ckvar(nComp),

  ctvar(nFun, kfuns(kType, kType, kType)),

  ctvar(nComp, kfuns(kType, kRow, kComp)),
  ctvar(nFunE, kfuns(kType, kComp, kType)),

  ctvar(nRec, kfun(kRow, kType)),
  ctvar(nVar, kfun(kRow, kType)),
);
