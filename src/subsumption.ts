import { Type, isTVar, isTMeta, isTFun, isTForall, openTForall, TMeta, TVar, showType, containsTMeta, isMono, TFun } from './types';
import { eqName, freshName, Name, showName, Plain } from './names';
import { withElems, split, addAll, add, comesBefore, context, setContext, replace, apply } from './context';
import { CTMeta, CTVar, matchCTMeta } from './elems';
import { err, InferError } from './errors';
import { wfType } from './wellformedness';

const solve = (x: Name, type: Type): void => {
  if (!isMono(type)) return err(`cannot solve with a polytype: ${showName(x)} := ${showType(type)}`);
  const right = split(matchCTMeta(x));
  wfType(type);
  add(CTMeta(x, type));
  addAll(right);
};

const instL = (x: Name, type: Type): void => {
  const old = context.slice(0);
  try {
    solve(x, type);
  } catch (error) {
    if (!(error instanceof InferError)) throw error;
    setContext(old);
    if (isTMeta(type)) return solve(type.name, TMeta(x));
    if (isTFun(type)) {
      const a = freshName(x);
      const b = freshName(x);
      replace(matchCTMeta(x), [
        CTMeta(b),
        CTMeta(a),
        CTMeta(x, TFun(TMeta(a), TMeta(b))),
      ]);
      instR(type.left, a);
      instL(b, apply(type.right));
      return;
    }
    if (isTForall(type)) {
      const a = freshName(x);
      withElems([CTVar(a)], () => instL(x, openTForall(type, TVar(a))));
      return;
    }
    return err(`instL failed: ${showName(x)} := ${showType(type)}`);
  }
};

const instR = (type: Type, x: Name): void => {
  const old = context.slice(0);
  try {
    solve(x, type);
  } catch (error) {
    if (!(error instanceof InferError)) throw error;
    setContext(old);
    if (isTMeta(type)) return solve(type.name, TMeta(x));
    if (isTFun(type)) {
      const a = freshName(x);
      const b = freshName(x);
      replace(matchCTMeta(x), [
        CTMeta(b),
        CTMeta(a),
        CTMeta(x, TFun(TMeta(a), TMeta(b))),
      ]);
      instL(a, type.left);
      instR(apply(type.right), b);
      return;
    }
    if (isTForall(type)) {
      const a = freshName(x);
      withElems([CTMeta(x)], () => instR(openTForall(type, TMeta(a)), x));
      return;
    }
    return err(`instL failed: ${showName(x)} := ${showType(type)}`);
  }
};

const subsume = (t1: Type, t2: Type): void => {
  if (t1 === t2) return;
  if (isTVar(t1) && isTVar(t2) && eqName(t1.name, t2.name)) return;
  if (isTMeta(t1) && isTMeta(t2) && eqName(t1.name, t2.name)) return;
  if (isTFun(t1) && isTFun(t2)) {
    subsume(t2.left, t1.left);
    subsume(t1.right, t2.right);
    return;
  }
  if (isTForall(t1)) {
    const x = freshName(t1.name);
    withElems([CTMeta(x)], () => subsume(openTForall(t1, TMeta(x)), t2));
    return;
  }
  if (isTForall(t2)) {
    const x = freshName(t2.name);
    withElems([CTVar(x)], () => subsume(t1, openTForall(t2, TVar(x))));
    return;
  }
  if (isTMeta(t1)) {
    if (containsTMeta(t1.name, t2)) return err(`infinite type: ${showType(t1)} in ${showType(t2)}`);
    instL(t1.name, t2);
    return;
  }
  if (isTMeta(t2)) {
    if (containsTMeta(t2.name, t1)) return err(`infinite type: ${showType(t2)} in ${showType(t1)}`);
    instR(t1, t2.name);
    return;
  }
  return err(`subsume failed: ${showType(t1)} <: ${showType(t2)}`);
};
