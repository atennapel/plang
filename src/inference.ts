import { Expr, isVar, isAbs, isApp, isLet, showExpr, isAnno, isLabeled, isWithLabel } from "./exprs";
import { Type, freshTMeta, tfun, isTVar, TApp, isTApp, isTMeta, TVar, TRecord, tapp, tfuns, TRowExtend, TVariant, isTRowExtend } from "./types";
import { resetTName } from "./names";
import { Env, findVar, withExtend, showEnv } from "./env";
import { err } from "./utils";
import { prune, unify, inferKind } from "./unification";
import { unifyKind } from "./kindUnification";
import { KType, KRow } from "./kinds";

export type Occ = { [key: number]: boolean };
const tmetas = (type: Type, occ: Occ = {}): Occ => {
  if (isTMeta(type)) { occ[type.name] = true; return occ }
  if (isTApp(type)) return tmetas(type.right, tmetas(type.left, occ));
  if (isTRowExtend(type)) return tmetas(type.rest, tmetas(type.type, occ));
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
  if (isTRowExtend(type)) return TRowExtend(type.label, inst(type.type, subst), inst(type.rest, subst));
  return type;
};

const genRec = (type: Type, occ: Occ | null): Type => {
  if (isTMeta(type) && (!occ || !occ[type.name])) return TVar(type.name);
  if (isTApp(type)) return TApp(genRec(type.left, occ), genRec(type.right, occ));
  if (isTRowExtend(type)) return TRowExtend(type.label, genRec(type.type, occ), genRec(type.rest, occ));
  return type;
};
const gen = (type: Type, env?: Env): Type =>
  genRec(type, env ? tmetasEnv(env) : null);

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
    const tv = prune(infer(env, expr.val));
    return withExtend(env, expr.name, tv, env => infer(env, expr.body));
  }
  if (isAnno(expr)) {
    const ty = infer(env, expr.expr);
    unify(ty, expr.type, tmetasEnv(env));
    return expr.type;
  }
  if (isWithLabel(expr)) {
    switch (expr.type) {
      case 'Select': {
        const tt = freshTMeta();
        const tr = freshTMeta(KRow);
        return tfuns(tapp(TRecord, TRowExtend(expr.label, tt, tr)), tt);
      }
      case 'Extend': {
        const tt = freshTMeta();
        const tr = freshTMeta(KRow);
        return tfuns(tt, tapp(TRecord, tr), tapp(TRecord, TRowExtend(expr.label, tt, tr)));
      }
      case 'Inject': {
        const tt = freshTMeta();
        const tr = freshTMeta(KRow);
        return tfuns(tt, tapp(TVariant, TRowExtend(expr.label, tt, tr)));
      }
      case 'Case': {
        const ta = freshTMeta();
        const tb = freshTMeta();
        const tr = freshTMeta(KRow);
        return tfuns(
          tfuns(ta, tb),
          tfuns(tapp(TVariant, tr), tb),
          tapp(TVariant, TRowExtend(expr.label, ta, tr)),
          tb,
        );
      }
    }
  }
  return err('unexpected expr in infer');
};

export const inferTop = (env: Env, expr: Expr): Type => {
  resetTName();
  const ty = infer(env, expr);
  unifyKind(KType, inferKind(ty));
  return gen(prune(ty));
};
