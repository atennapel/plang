import {
  isTVar,
  isTMeta,
  showType,
  Type,
  matchTFun,
  isTApp,
  isTForall,
  TMeta,
  openTForall,
  TVar,
  containsTMeta,
  isMono,
  TFun,
} from './types';
import { eqName } from './names';
import { apply, namestore, context, discardContext, storeContext, restoreContext } from './global';
import { CTMeta, CTVar } from './elems';
import { kType, eqKind } from './kinds';
import { wfType } from './wellformedness';
import { InferError, infererr } from './error';
import { unify, inst } from './unification';
import { deriveKind } from './kindInference';

export const solve = (x: TMeta, type: Type): void => {
  if (!isMono(type)) return infererr('solve with polytype');
  const elem = context.lookup('CTMeta', x.name);
  if (!elem) return infererr('solve with undefined tmeta');
  const right = context.split('CTMeta', x.name);
  wfType(type);
  context.add(CTMeta(x.name, elem.kind, type));
  context.addAll(right);
};
// x := List y
const instL = (x: TMeta, type: Type): void => {
  storeContext();
  try {
    solve(x, type);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isTMeta(type)) return solve(type, x);
    const f = matchTFun(type);
    if (f) {
      const y = x.name;
      const a = namestore.fresh(y);
      const b = namestore.fresh(y);
      const ta = TMeta(a);
      const tb = TMeta(b);
      context.replace('CTMeta', y, [
        CTMeta(b, kType),
        CTMeta(a, kType),
        CTMeta(y, kType, TFun(ta, tb)),
      ]);
      instR(f.left, ta);
      instL(tb, apply(f.right));
      return;
    }
    if (isTApp(type)) return inst(x, type);
    if (isTForall(type)) {
      if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
      const y = namestore.fresh(type.name);
      context.enter(y, CTVar(y, type.kind));
      instL(x, openTForall(type, TVar(y)));
      context.leave(y);
      return;
    }
    return infererr(`instL failed: ${showType(x)} := ${showType(type)}, ${err}`);
  }
};

const instR = (type: Type, x: TMeta): void => {
  storeContext();
  try {
    solve(x, type);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isTMeta(type)) return solve(type, x);
    const f = matchTFun(type);
    if (f) {
      const y = x.name;
      const a = namestore.fresh(y);
      const b = namestore.fresh(y);
      const ta = TMeta(a);
      const tb = TMeta(b);
      context.replace('CTMeta', y, [
        CTMeta(b, kType),
        CTMeta(a, kType),
        CTMeta(y, kType, TFun(ta, tb)),
      ]);
      instL(ta, f.left);
      instR(apply(f.right), tb);
      return;
    }
    if (isTApp(type)) return inst(x, type);
    if (isTForall(type)) {
      if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
      const y = namestore.fresh(type.name);
      context.enter(y, CTMeta(y, type.kind));
      instR(openTForall(type, TMeta(y)), x);
      context.leave(y);
      return;
    }
    return infererr(`instR failed: ${showType(x)} := ${showType(type)}, ${err}`);
  }
};

export const subsume = (a: Type, b: Type): void => {
  // console.log(`subsume ${showType(a_)} <: ${showType(b_)} in ${context}`);
  if (a === b) return;
  if (!eqKind(deriveKind(a), deriveKind(b)))
    return infererr(`kind mismatch in ${showType(a)} <: ${showType(b)}`);
  if (isTVar(a) && isTVar(b) && eqName(a.name, b.name)) return;
  if (isTMeta(a) && isTMeta(b) && eqName(a.name, b.name)) return;
  const fa = matchTFun(a);
  const fb = matchTFun(b);
  if (fa && fb) {
    subsume(fb.left, fa.left);
    return subsume(apply(fa.right), apply(fb.right));
  }
  if (isTApp(a) && isTApp(b))
    return unify(a, b);
  if (isTForall(a)) {
    if (!a.kind) return infererr(`forall lacks kind: ${showType(a)}`);
    const t = namestore.fresh(a.name);
    context.enter(t, CTMeta(t, a.kind));
    subsume(openTForall(a, TMeta(t)), b);
    context.leave(t);
    return;
  }
  if (isTForall(b)) {
    if (!b.kind) return infererr(`forall lacks kind: ${showType(b)}`);
    const t = namestore.fresh(b.name);
    context.enter(t, CTVar(t, b.kind));
    subsume(a, openTForall(b, TVar(t)));
    context.leave(t);
    return;
  }
  if (isTMeta(a)) {
    if (containsTMeta(a.name, b))
      return infererr(`occurs check L failed: ${showType(a)} in ${showType(b)}`);
    return instL(a, b);
  }
  if (isTMeta(b)) {
    if (containsTMeta(b.name, a))
      return infererr(`occurs check R failed: ${showType(b)} in ${showType(a)}`);
    return instR(a, b);
  }
  return infererr(`subsume failed: ${showType(a)} <: ${showType(b)}`)
};
