import {
  isTVar,
  isTMeta,
  showType,
  Type,
  isTApp,
  isTForall,
  TMeta,
  openTForall,
  TVar,
  containsTMeta,
  TApp,
} from './types';
import { eqName } from './names';
import { apply, namestore, context, discardContext, storeContext, restoreContext } from './global';
import { CTMeta, CTVar } from './elems';
import { eqKind, KFun } from './kinds';
import { InferError, infererr } from './error';
import { solve } from './subsumption';
import { deriveKind } from './kindInference';
import { log } from './config';

export const inst = (x: TMeta, type: Type): void => {
  log(`inst ${showType(x)} := ${showType(type)}`);
  storeContext();
  try {
    solve(x, type);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isTMeta(type)) return solve(type, x);
    if (isTApp(type)) {
      const ka = deriveKind(type.left);
      const kb = deriveKind(type.right);
      const kr = (ka as KFun).right;
      const y = x.name;
      const a = namestore.fresh(y);
      const b = namestore.fresh(y);
      const ta = TMeta(a);
      const tb = TMeta(b);
      context.replace('CTMeta', y, [
        CTMeta(b, kb),
        CTMeta(a, ka),
        CTMeta(y, kr, TApp(ta, tb)),
      ]);
      inst(ta, type.left);
      inst(tb, apply(type.right));
      return;
    }
    if (isTForall(type)) {
      if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
      const y = namestore.fresh(type.name);
      context.enter(y, CTVar(y, type.kind));
      inst(x, openTForall(type, TVar(y)));
      context.leave(y);
      return;
    }
    return infererr(`inst failed: ${showType(x)} := ${showType(type)}, ${err}`);
  }
};

export const unify = (a: Type, b: Type): void => {
  log(`unify ${showType(a)} ~ ${showType(b)} in ${context}`);
  if (a === b) return;
  if (!eqKind(deriveKind(a), deriveKind(b)))
    return infererr(`kind mismatch in ${showType(a)} ~ ${showType(b)}`);
  if (a === b) return;
  if (isTVar(a) && isTVar(b) && eqName(a.name, b.name)) return;
  if (isTMeta(a) && isTMeta(b) && eqName(a.name, b.name)) return;
  if (isTApp(a) && isTApp(b)) {
    unify(a.left, b.left);
    return unify(apply(a.right), apply(b.right));
  }
  if (isTForall(a) && isTForall(b)) {
    if (!a.kind) return infererr(`forall lacks kind: ${showType(a)}`);
    if (!b.kind) return infererr(`forall lacks kind: ${showType(b)}`);
    if (!eqKind(a.kind, b.kind))
      return infererr(`kind does not match in foralls: ${showType(a)} ~ ${showType(b)}`);
    const t = namestore.fresh(a.name);
    context.enter(t, CTVar(t, a.kind));
    unify(openTForall(a, TVar(t)), openTForall(b, TVar(t)));
    context.leave(t);
    return;
  }
  if (isTMeta(a)) {
    if (containsTMeta(a.name, b))
      return infererr(`occurs check L failed: ${showType(a)} in ${showType(b)}`);
    return inst(a, b);
  }
  if (isTMeta(b)) {
    if (containsTMeta(b.name, a))
      return infererr(`occurs check R failed: ${showType(b)} in ${showType(a)}`);
    return inst(b, a);
  }
  return infererr(`unify failed: ${showType(a)} ~ ${showType(b)}`)
};
