import { caseExpr, Expr } from "./exprs";
import { TC } from "./TC";
import { Type, Forall, TMeta, TFun, Subst, substMeta, substTVar, freeMeta, TVar, TFunP } from "./types";
import { Context, findVar, extendVar } from "./context";
import { isLeft, Right } from "./either";
import { prune, unify } from "./unification";
import { fresh, resetId, Name } from "./names";
import { kType, Kind } from "./kinds";

const instantiate = (type: Forall): Type => {
  const sub: Subst = {};
  const args = type.args;
  for (let i = args.length - 1; i >= 0; i--) {
    const c = args[i];
    const n = c[0];
    const k = c[1];
    sub[n] = TMeta(fresh(n), k);
  }
  return substTVar(sub, type.type);
};

export const infer = (ctx: Context, expr: Expr): TC<Type> => caseExpr(expr, {
  Var: name => {
    const ty = findVar(ctx, name);
    if (isLeft(ty)) return ty;
    return Right(instantiate(ty.val));
  },
  Abs: (arg, body) => {
    const m = TMeta(fresh(arg), kType);
    const res = infer(extendVar(ctx, arg, Forall([], m)), body);
    if (isLeft(res)) return res;
    return Right(TFunP(prune(m), res.val));
  },
  App: (left, right) => {
    const lr = infer(ctx, left);
    if (isLeft(lr)) return lr;
    const rr = infer(ctx, right);
    if (isLeft(rr)) return rr;
    const m = TMeta(fresh('t'), kType);
    const ur = unify(ctx, lr.val, TFunP(rr.val, m), false);
    if (isLeft(ur)) return ur;
    return Right(prune(m));
  },
});

const generalize = (ctx: Context, type: Type): Forall => {
  const fr = freeMeta(type);
  for (let name in ctx.vars) {
    for (let k in freeMeta(ctx.vars[name].type))
      (fr[k] as any) = undefined;
  }
  const args: [Name, Kind][] = [];
  const sub: Subst = {};
  for (let k in fr) {
    args.push([k, fr[k].kind]);
    sub[k] = TVar(k);
  }
  return Forall(args, substMeta(sub, type));
};

export const inferGen = (ctx: Context, expr: Expr): TC<Forall> => {
  resetId();
  const ty = infer(ctx, expr);
  if (isLeft(ty)) return ty;
  return Right(generalize(ctx, ty.val));
};
