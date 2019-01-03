import { Expr, isVar, isAbs, isApp, isAnno, openAbs, Var, showExpr } from './exprs';
import { Type, TMeta, TFun, isTForall, isTFun, TVar, openTForall, showType, isTMeta, tforall, substTMeta } from './types';
import { wfContext, wfType } from './wellformedness';
import { apply, isComplete, findElem, addAll, withElems, add, replace, context, showContext, withElemsContext, unsolvedTMetasInType } from './context';
import { err } from './errors';
import { resetName, freshName, showName } from './names';
import { matchCVar, CTMeta, CVar, CTVar, matchCTMeta } from './elems';
import { subsume } from './subsumption';
import { log } from './logging';
import { kType } from './kinds';
import { checkKindType } from './inferenceKinds';

const generalize = (fn: () => Type): Type => {
  const [ty, right] = withElemsContext([], fn);
  const u = unsolvedTMetasInType(ty, right);
  return tforall(u, u.reduce((t, [n, k]) => substTMeta(n, TVar(n), t), ty));
};

const typesynth = (expr: Expr): Type => {
  log(`typesynth: ${showExpr(expr)}`);
  if (isVar(expr)) return findElem(matchCVar(expr.name), `undefined var ${expr.name}`).type;
  if (isAbs(expr)) {
    const x = freshName(expr.arg);
    const a = freshName(expr.arg);
    const b = freshName(expr.arg);
    return generalize(() => {
      addAll([CTMeta(a, kType), CTMeta(b, kType), CVar(x, TMeta(a))]);
      typecheck(openAbs(expr, Var(x)), TMeta(b));
      return apply(TFun(TMeta(a), TMeta(b)));
    });
  }
  if (isApp(expr)) {
    const left = typesynth(expr.left);
    return typeappsynth(apply(left), expr.right);
  }
  if (isAnno(expr)) {
    const ty = expr.type;
    wfType(ty);
    checkKindType(ty);
    typecheck(expr.expr, ty);
    return ty;
  }
  return err(`cannot typesynth: ${showExpr(expr)}`);
};

const typecheck = (expr: Expr, type: Type): void => {
  log(`typecheck: ${showExpr(expr)} : ${showType(type)}`);
  if (isTForall(type)) {
    const x = freshName(type.name);
    withElems([CTVar(x, type.kind)], () => typecheck(expr, openTForall(type, TVar(x))));
    return;
  }
  if (isAbs(expr) && isTFun(type)) {
    const x = freshName(expr.arg);
    withElems([CVar(x, type.left)], () => typecheck(openAbs(expr, Var(x)), type.right));
    return;
  }
  const ty = typesynth(expr);
  subsume(apply(ty), apply(type));
};

const typeappsynth = (type: Type, expr: Expr): Type => {
  log(`typeappsynth: ${showType(type)} @ ${showExpr(expr)}`);
  if (isTForall(type)) {
    const x = freshName(type.name);
    add(CTMeta(x, type.kind));
    return typeappsynth(openTForall(type, TMeta(x)), expr);
  }
  if (isTMeta(type)) {
    const a = freshName(type.name);
    const b = freshName(type.name);
    replace(matchCTMeta(type.name), [
      CTMeta(b, kType),
      CTMeta(a, kType),
      CTMeta(type.name, kType, TFun(TMeta(a), TMeta(b))),
    ]);
    typecheck(expr, TMeta(a));
    return TMeta(b);
  }
  if (isTFun(type)) {
    typecheck(expr, type.left);
    return type.right;
  }
  return err(`cannot typeappsynth: ${showType(type)} @ ${showExpr(expr)}`);
};

export const infer = (expr: Expr): Type => {
  resetName();
  wfContext();
  const ty = generalize(() => apply(typesynth(expr)));
  wfType(ty);
  checkKindType(ty);
  wfContext();
  if (!isComplete()) return err(`incomplete context after inference`);
  return ty;
};
