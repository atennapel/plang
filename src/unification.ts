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
import { CTMeta, CKMeta, CTVar } from './elems';
import { KMeta, kType } from './kinds';
import { InferError, infererr } from './error';
import { solve } from './subsumption';

const inst = (x: TMeta, type: Type): void => {
  storeContext();
  try {
    solve(x, type);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isTMeta(type)) return solve(type, x);
    if (isTApp(type)) {
      const y = x.name;
      const a = namestore.fresh(y);
      const b = namestore.fresh(y);
      const ta = TMeta(a);
      const tb = TMeta(b);
      context.replace('CTMeta', y, [
        CTMeta(b, kType),
        CTMeta(a, kType),
        CTMeta(y, kType, TApp(ta, tb)),
      ]);
      inst(ta, type.left);
      inst(tb, apply(type.right));
      return;
    }
    if (isTForall(type)) {
      const y = namestore.fresh(type.name);
      if (type.kind) {
        context.enter(y, CTVar(y, type.kind));
      } else {
        const k = namestore.fresh(type.name);
        context.enter(y, CKMeta(k), CTVar(y, KMeta(k)));
      }
      inst(x, openTForall(type, TVar(y)));
      context.leave(y);
      return;
    }
    return infererr(`inst failed: ${showType(x)} := ${showType(type)}`);
  }
};

export const unify = (a: Type, b: Type): void => {
  if (a === b) return;
  if (isTVar(a) && isTVar(b) && eqName(a.name, b.name)) return;
  if (isTMeta(a) && isTMeta(b) && eqName(a.name, b.name)) return;
  if (isTApp(a) && isTApp(b)) {
    unify(a.left, b.left);
    return unify(apply(a.right), apply(b.right));
  }
  if (isTForall(a) && isTForall(b)) {
    const t = namestore.fresh(a.name);
    const k = namestore.fresh(a.name);
    context.enter(t, CKMeta(k), CTVar(t, KMeta(k)));
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
