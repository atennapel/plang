import { isTVar, Type, isTMeta, isTFun, isTForall, openTForall, TVar } from './types';
import { Kind, kType } from './kinds';
import { impossible } from './errors';
import { findElem, applyKind, withElems } from './context';
import { matchCTVar, matchCTMeta, CTVar } from './elems';
import { unifyKinds } from './unificationKinds';
import { freshName } from './names';

export const inferKind = (type: Type): Kind => {
  if (isTVar(type)) return findElem(matchCTVar(type.name)).kind;
  if (isTMeta(type)) return findElem(matchCTMeta(type.name)).kind;
  if (isTFun(type)) {
    unifyKinds(inferKind(type.left), kType);
    unifyKinds(applyKind(inferKind(type.right)), kType);
    return kType;
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
