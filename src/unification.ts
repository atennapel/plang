import { Type, showType, isTVar, isTMeta, isTFun, isTForall, isTApp, openTForall, TVar, containsTMeta } from './types';
import { log } from './logging';
import { unifyKindsOfTypes } from './inferenceKinds';
import { eqName, freshName, showName, Name } from './names';
import { apply, applyKind, withElems, replace, findElem } from './context';
import { err } from './errors';
import { CTVar, matchCTMeta, CTMeta } from './elems';
import { unifyKinds } from './unificationKinds';

const bind = (tv: Name, type: Type): void => {
  log(`bind: ${showName(tv)} := ${showType(type)}`);
  if (containsTMeta(tv, type)) return err(`infinite type: ${showName(tv)} in ${showType(type)}`);
  const kind = findElem(matchCTMeta(tv)).kind;
  replace(matchCTMeta(tv), [CTMeta(tv, kind, type)]);
};

export const unify = (t1: Type, t2: Type): void => {
  log(`unify: ${showType(t1)} ~ ${showType(t2)}`);
  unifyKindsOfTypes(t1, t2);
  if (t1 === t2) return;
  if (isTVar(t1) && isTVar(t2) && eqName(t1.name, t2.name)) return;
  if (isTMeta(t1) && isTMeta(t2) && eqName(t1.name, t2.name)) return;
  if (isTMeta(t1)) return bind(t1.name, t2);
  if (isTMeta(t2)) return bind(t2.name, t1);
  if (isTFun(t1) && isTFun(t2)) {
    unify(t1.left, t2.left);
    unify(apply(t1.right), apply(t2.right));
    return;
  }
  if (isTApp(t1) && isTApp(t2)) {
    unify(t1.left, t2.left);
    unify(apply(t1.right), apply(t2.right));
    return;
  }
  if (isTForall(t1) && isTForall(t2)) {
    unifyKinds(t1.kind, t2.kind);
    const x = freshName(t1.name);
    withElems([CTVar(x, applyKind(t1.kind))], () => unify(openTForall(t1, TVar(x)), openTForall(t2, TVar(x))));
    return;
  }
  return err(`unify failed: ${showType(t1)} ~ ${showType(t2)}`);
};
