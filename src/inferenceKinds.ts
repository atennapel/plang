import { isTVar, Type, isTMeta, isTFun, isTForall, openTForall, TVar, isTApp } from './types';
import { Kind, kType, KFun, KMeta } from './kinds';
import { impossible } from './errors';
import { findElem, applyKind, withElems, add } from './context';
import { matchCTVar, matchCTMeta, CTVar, CKMeta } from './elems';
import { unifyKinds } from './unificationKinds';
import { freshName, Plain } from './names';

export const inferKind = (type: Type): Kind => {
  if (isTVar(type)) return findElem(matchCTVar(type.name)).kind;
  if (isTMeta(type)) return findElem(matchCTMeta(type.name)).kind;
  if (isTFun(type)) {
    unifyKinds(inferKind(type.left), kType);
    unifyKinds(applyKind(inferKind(type.right)), kType);
    return kType;
  }
  if (isTApp(type)) {
    const k1 = inferKind(type.left);
    const k2 = inferKind(type.right);
    const r = freshName(Plain('k'));
    add(CKMeta(r));
    unifyKinds(applyKind(k1), KFun(applyKind(k2), KMeta(r)));
    return applyKind(KMeta(r));
  }
  if (isTForall(type)) {
    const x = freshName(type.name);
    return withElems([CTVar(x, type.kind)], () => inferKind(openTForall(type, TVar(x))));
  }
  return impossible('inferKind');
};

export const unifyKindsOfTypes = (t1: Type, t2: Type): Kind => {
  const k1 = inferKind(t1);
  const k2 = inferKind(t2);
  unifyKinds(applyKind(k1), applyKind(k2));
  return applyKind(k1);
};

export const checkKindType = (type: Type) => unifyKinds(inferKind(type), kType);
