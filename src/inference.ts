import { Expr, isVar, isAbs, isApp, isLet, showExpr } from "./exprs";
import { Type, freshTMeta, tfun, isTVar, TApp, isTApp, isTMeta, TVar } from "./types";
import { resetTName } from "./names";
import { Env, findVar, withExtend, showEnv } from "./env";
import { err } from "./utils";
import { prune, unify } from "./unification";

type Occ = { [key: number]: boolean };
const tmetas = (type: Type, occ: Occ = {}): Occ => {
  if (isTMeta(type)) { occ[type.name] = true; return occ }
  if (isTApp(type)) return tmetas(type.right, tmetas(type.left, occ));
  return occ;
};
const tmetasEnv = (env: Env, occ: Occ = {}): Occ => {
  for (let k in env) tmetas(env[k], occ);
  return occ;
};

type Subst = { [key: number]: Type };
const inst = (type: Type, subst: Subst = {}): Type => {
  if (isTVar(type)) return subst[type.name] || (subst[type.name] = freshTMeta());
  if (isTApp(type)) return TApp(inst(type.left, subst), inst(type.right, subst));
  return type;
};

const genRec = (type: Type, occ: Occ): Type => {
  if (isTMeta(type) && !occ[type.name]) return TVar(type.name);
  if (isTApp(type)) return TApp(genRec(type.left, occ), genRec(type.right, occ));
  return type;
};
const gen = (env: Env, type: Type): Type => {
  const free = tmetasEnv(env);
  return genRec(type, free);
};

const infer = (env: Env, expr: Expr): Type => {
  // console.log('infer', showExpr(expr), showEnv(env));
  if (isVar(expr)) return inst(findVar(env, expr.name));
  if (isAbs(expr)) {
    const tv = freshTMeta();
    const tr = withExtend(env, expr.name, tv, env => infer(env, expr.body));
    return tfun(tv, tr);
  }
  if (isApp(expr)) {
    const tf = infer(env, expr.left);
    const ta = infer(env, expr.right);
    const tv = freshTMeta();
    unify(tf, tfun(ta, tv));
    return tv;
  }
  if (isLet(expr)) {
    const tv_ = prune(infer(env, expr.val));
    const tv = gen(env, tv_);
    return withExtend(env, expr.name, tv, env => infer(env, expr.body));
  }
  return err('unexpected expr in infer');
};

const genTop = (type: Type): Type => {
  if (isTMeta(type)) return TVar(type.name);
  if (isTApp(type)) return TApp(genTop(type.left), genTop(type.right));
  return type;
};

export const inferTop = (env: Env, expr: Expr): Type => {
  resetTName();
  const ty = infer(env, expr);
  return genTop(prune(ty));
};
