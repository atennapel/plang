import { impossible } from './utils';
import { ctvar } from './elems';
import { isKVar, isKFun, KFun } from './kinds';
import { isTVar, isTMeta, isTFun, isTForall, tvar, isTApp } from './types';
import { kType } from './initial';
import { TC, KindN, TypeN, error, ok, withElems, findKVar, findTVar, findTMeta, freshName, log } from './TC';

export const checkKind = (exp: KindN, actual: KindN, msg?: string): TC<void> => {
  if (exp.equals(actual)) return ok;
  return error(`kind mismatch, expected ${exp} but got ${actual}${msg ? `: ${msg}`: ''}`);
};

export const wfKind = (kind: KindN): TC<void> => {
  if (isKVar(kind)) return findKVar(kind.name).void();
  if (isKFun(kind)) return wfKind(kind.left).then(wfKind(kind.right));
  return impossible();
};

export const wfType = (type: TypeN): TC<KindN> => {
  if (isTVar(type)) return findTVar(type.name).map(e => e.kind);
  if (isTMeta(type)) return findTMeta(type.name).map(e => e.kind);
  if (isTFun(type))
    return wfType(type.left).chain(k => checkKind(kType, k, `left side of ${type}`))
      .then(wfType(type.right).chain(k => checkKind(kType, k, `right side of ${type}`)))
      .map(() => kType);
  if (isTApp(type))
    return wfType(type.left)
      .checkIs(isKFun, k => `left side of ${type} is not a higher-kinded type: ${k}`) 
      .chain(k => wfType(type.right)
      .chain(kr => checkKind(k.left, kr, `type application ${type}`))
      .map(() => k.right));
  if (isTForall(type))
    return wfKind(type.kind)
      .then(freshName(type.name)
      .chain(x => withElems([ctvar(x, type.kind)], wfType(type.open(tvar(x))))));
  return impossible();
};
