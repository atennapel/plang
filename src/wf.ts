import { impossible } from './utils';
import { ctvar } from './elems';
import { isKVar, isKFun, isKComp } from './kinds';
import { isTVar, isTMeta, isTForall, tvar, isTApp, isTRowEmpty, isTRowExtend, rowContainsDuplicate, isTEffsEmpty, isTEffsExtend } from './types';
import { TC, KindN, TypeN, error, ok, withElems, findKVar, findTVar, findTMeta, freshName, pure, check, log } from './TC';
import { kRow, kType, matchTRec, matchTVariant, kEffs, kEff } from './initial';

export const checkKind = (exp: KindN, actual: KindN, msg?: string): TC<void> => {
  if (exp.equals(actual)) return ok;
  return error(`kind mismatch, expected ${exp} but got ${actual}${msg ? `: ${msg}`: ''}`);
};

export const wfKind = (kind: KindN): TC<void> => {
  if (isKVar(kind)) return findKVar(kind.name).void();
  if (isKFun(kind)) return wfKind(kind.left).then(wfKind(kind.right));
  if (isKComp(kind)) return wfKind(kind);
  return impossible();
};

export const wfType = (type: TypeN): TC<KindN> => {
  if (isTVar(type)) return findTVar(type.name).map(e => e.kind);
  if (isTMeta(type)) return findTMeta(type.name).map(e => e.kind);
  
  const rec = matchTRec(type);
  if (rec)
    return wfType(rec)
      .chain(k => checkKind(kRow, k, `${type}`))
      .then(check(!rowContainsDuplicate(rec), `record contains duplicates ${type}`))
      .map(() => kType);
  const variant = matchTVariant(type);
  if (variant)
    return wfType(variant)
      .chain(k => checkKind(kRow, k, `${type}`))
      .then(check(!rowContainsDuplicate(variant), `variant contains duplicates ${type}`))
      .map(() => kType);
  
  if (isTApp(type))
    return wfType(type.left)
      .checkIs(isKFun, k => `left side of ${type} is not a higher-kinded type: ${k}`) 
      .chain(k => wfType(type.right)
      .chain(kr => checkKind(k.left, kr, `type application ${type}`))
      .map(() => k.right));
  if (isTRowExtend(type))
    return wfType(type.type)
      .chain(k => checkKind(kType, k, `row type ${type}`))
      .then(wfType(type.rest))
      .chain(k => checkKind(kRow, k, `row rest ${type}`))
      .map(() => kRow);
  if (isTEffsExtend(type))
    return wfType(type.type)
      .chain(k => checkKind(kEff, k, `effs type ${type}`))
      .then(wfType(type.rest))
      .chain(k => checkKind(kEffs, k, `effs rest ${type}`))
      .map(() => kEffs);
  if (isTForall(type))
    return wfKind(type.kind)
      .then(freshName(type.name)
      .chain(x => withElems([ctvar(x, type.kind)], wfType(type.open(tvar(x))))));
  if (isTRowEmpty(type)) return pure(kRow);
  if (isTEffsEmpty(type)) return pure(kEffs);
  return impossible();
};
