import { Type, showType, isTVar, isTMeta, isTFun, isTForall, isTApp, openTForall, TVar, containsTMeta, TMeta, TFun } from './types';
import { log } from './logging';
import { unifyKindsOfTypes } from './inferenceKinds';
import { eqName, freshName, showName, Name } from './names';
import { apply, applyKind, withElems, replace, findElem, context, split, add, addAll, setContext } from './context';
import { err, InferError } from './errors';
import { CTVar, matchCTMeta, CTMeta } from './elems';
import { unifyKinds } from './unificationKinds';
import { wfType } from './wellformedness';
import { kType } from './kinds';
import { solveConstraints } from './constraintSolver';

const bind = (tv: Name, type: Type): void => {
  log(`bind: ${showName(tv)} := ${showType(type)}`);
  if (containsTMeta(tv, type)) return err(`infinite type: ${showName(tv)} in ${showType(type)}`);
  const kind = findElem(matchCTMeta(tv)).kind;
  const cs = findElem(matchCTMeta(tv)).cs.slice(0);
  const right = split(matchCTMeta(tv));
  wfType(type);
  if (isTMeta(type)) {
    const e = findElem(matchCTMeta(type.name));
    e.cs.forEach(c => cs.push(c));
    replace(matchCTMeta(type.name), [CTMeta(type.name, e.kind, cs, e.type)]);
  }
  const newcs = solveConstraints(cs, type, true);
  if (isTMeta(type)) {
    const e = findElem(matchCTMeta(type.name));
    replace(matchCTMeta(type.name), [CTMeta(type.name, e.kind, newcs, e.type)]);
  }
  add(CTMeta(tv, kind, newcs, type));
  addAll(right);
};

const inst = (x: Name, type: Type): void => {
  log(`inst: ${showName(x)} := ${showType(type)}`);
  const old = context.slice(0);
  try {
    bind(x, type);
  } catch (error) {
    if (!(error instanceof InferError)) throw error;
    setContext(old);
    if (isTMeta(type)) return bind(type.name, TMeta(x));
    if (isTFun(type)) {
      const a = freshName(x);
      const b = freshName(x);
      const cs = findElem(matchCTMeta(x)).cs;
      replace(matchCTMeta(x), [
        CTMeta(b, kType, []),
        CTMeta(a, kType, []),
        CTMeta(x, kType, cs, TFun(TMeta(a), TMeta(b))),
      ]);
      inst(a, type.left);
      inst(b, apply(type.right));
      return;
    }
    if (isTForall(type)) {
      const a = freshName(x);
      withElems([CTVar(a, type.kind)], () => inst(x, openTForall(type, TVar(a))));
      return;
    }
    return err(`inst failed: ${showName(x)} := ${showType(type)}`);
  }
};

export const unify = (t1: Type, t2: Type): void => {
  log(`unify: ${showType(t1)} ~ ${showType(t2)}`);
  unifyKindsOfTypes(t1, t2);
  if (t1 === t2) return;
  if (isTVar(t1) && isTVar(t2) && eqName(t1.name, t2.name)) return;
  if (isTMeta(t1) && isTMeta(t2) && eqName(t1.name, t2.name)) return;
  if (isTMeta(t1)) return inst(t1.name, t2);
  if (isTMeta(t2)) return inst(t2.name, t1);
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
