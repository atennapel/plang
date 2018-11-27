import TC, { error, ok, findKVar, pure, findTVar, findTMeta, freshName, withElems } from "./TC";
import Kind, { isKVar, isKFun, kEffs, kEff, kType, kComp } from "./kinds";
import { impossible } from "../utils";
import Type, { isTVar, isTMeta, isTApp, isTEffsExtend, isTForall, isTEffsEmpty, tvar, isTFun, isTComp } from "./types";
import { ctvar } from "./elems";

export const checkKind = (exp: Kind, actual: Kind, msg?: string): TC<void> => {
  if (exp.equals(actual)) return ok;
  return error(`kind mismatch, expected ${exp} but got ${actual}${msg ? `: ${msg}`: ''}`);
};

export const wfKind = (kind: Kind): TC<void> => {
  if (isKVar(kind)) return findKVar(kind.name).void();
  if (isKFun(kind)) return wfKind(kind.left).then(wfKind(kind.right));
  return impossible();
};

export const wfType = (type: Type): TC<Kind> => {
  if (isTVar(type)) return findTVar(type.name).map(e => e.kind);
  if (isTMeta(type)) return findTMeta(type.name).map(e => e.kind);
  if (isTApp(type))
    return wfType(type.left)
      .checkIs(isKFun, k => `left side of ${type} is not a higher-kinded type: ${k}`) 
      .chain(k => wfType(type.right)
      .chain(kr => checkKind(k.left, kr, `type application ${type}`))
      .map(() => k.right));
  if (isTFun(type))
    return wfType(type.left)
      .chain(kl => checkKind(kType, kl, `left side of function: ${type}`))
      .then(wfType(type.right)
      .chain(kr => checkKind(kComp, kr, `right side of function: ${type}`))
      .map(() => kType));
  if (isTComp(type))
    return wfType(type.type)
      .chain(kl => checkKind(kType, kl, `left side of comp type: ${type}`))
      .then(wfType(type.eff)
      .chain(kr => checkKind(kEffs, kr, `right side of comp type: ${type}`))
      .map(() => kComp));
  if (isTEffsExtend(type))
    return wfType(type.type)
      .chain(k => checkKind(kEff, k, `effs type ${type}`))
      .then(wfType(type.rest))
      .chain(k => checkKind(kEffs, k, `effs rest ${type}`))
      .map(() => kEffs);
  if (isTForall(type))
    return wfKind(type.kind)
      .then(freshName(type.name)
      .chain(x => withElems([ctvar(x, type.kind)],
        wfType(type.open(tvar(x)))
        .chain(k => checkKind(kComp, k, `tforall: ${type}`)))))
      .map(() => kType);
  if (isTEffsEmpty(type)) return pure(kEffs);
  return impossible();
};
