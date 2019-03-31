import { Env } from './env';
import { TMeta, Type, TFun, isTFun, occursTMeta, showTy } from './types';
import { freshTMeta, terr } from './util';
import { kType, showKind, eqKind } from './kinds';
import { kindOf } from './kindInference';

const bindTMeta = (env: Env, x: TMeta, t: Type): void => {
  if (x.type) return unify(env, x.type, t);
  if (t.tag === 'TMeta' && t.type) {
    if (!x.name && t.name) x.name = t.name;
    return unify(env, x, t.type);
  }
  if (occursTMeta(x, t))
    return terr(`${showTy(x)} occurs in ${showTy(t)}`);
  const k1 = kindOf(env, x);
  const k2 = kindOf(env, t);
  if (!eqKind(k1, k2))
    return terr(`kind mismatch in unification of ${showTy(x)} ~ ${showTy(t)}: ${showKind(k1)} ~ ${showKind(k2)}`);
  if (!x.name && t.tag === 'TMeta' && t.name) x.name = t.name;
  x.type = t;
};
export const unify = (env: Env, a: Type, b: Type): void => {
  console.log(`unify ${showTy(a)} ~ ${showTy(b)}`);
  if (a.tag === 'TVar' || b.tag === 'TVar')
    return terr(`tvar in unify: ${showTy(a)} ~ ${showTy(b)}`);
  if (a === b) return;
  if (a.tag === 'TMeta') return bindTMeta(env, a, b);
  if (b.tag === 'TMeta') return bindTMeta(env, b, a);
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    unify(env, a.left, b.left);
    return unify(env, a.right, b.right);
  }
  if (a.tag === 'TSkol' && b.tag === 'TSkol' && a.id === b.id)
    return;
  if (a.tag === 'TCon' && b.tag === 'TCon' && a.name === b.name)
    return;
  return terr(`failed to unify: ${showTy(a)} ~ ${showTy(b)}`);
};

export const unifyTFun = (env: Env, ty: Type): TFun => {
  if (isTFun(ty)) return ty;
  const fn = TFun(freshTMeta(kType), freshTMeta(kType));
  unify(env, ty, fn);
  return fn;
};
