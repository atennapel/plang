import { caseExpr, Expr, showExpr } from "./exprs";
import { TC } from "./TC";
import { Type, Forall, TMeta, TFun, Subst, substMeta, substTVar, freeMeta, TVar, TFunP, tEffsEmpty, isTEffsEmpty, showType, TypeEff, typePure, showTypeEff, typeEff } from "./types";
import { Context, findVar, extendVar } from "./context";
import { isLeft, Right, Left } from "./either";
import { prune, unify } from "./unification";
import { fresh, resetId, Name } from "./names";
import { kType, Kind, kEffs } from "./kinds";

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

export const infer = (ctx: Context, expr: Expr): TC<TypeEff> => {
  console.log(`infer ${showExpr(expr)}`);
  return caseExpr(expr, {
    Var: name => {
      const ty = findVar(ctx, name);
      if (isLeft(ty)) return ty;
      return Right(typePure(instantiate(ty.val)));
    },
    Abs: (arg, body) => {
      const m = TMeta(fresh(arg), kType);
      const res = infer(extendVar(ctx, arg, Forall([], m)), body);
      if (isLeft(res)) return res;
      return Right(typePure(TFun(prune(m), res.val.eff, res.val.type)));
    },
    App: (left, right) => {
      const lr = infer(ctx, left);
      if (isLeft(lr)) return lr;
      const rr = infer(ctx, right);
      if (isLeft(rr)) return rr;
      const m = TMeta(fresh('t'), kType);
      const e = TMeta(fresh('e'), kEffs);
      const ur = unify(ctx, lr.val.type, TFun(rr.val.type, e, m), false);
      if (isLeft(ur)) return ur;
      const ure1 = unify(ctx, e, lr.val.eff);
      if (isLeft(ure1)) return ure1;
      const ure2 = unify(ctx, e, rr.val.eff);
      if (isLeft(ure2)) return ure2;
      return Right(typeEff(prune(m), prune(e)));
    },
  });
};

const generalize = (ctx: Context, typeE: TypeEff): TC<Forall> => {
  if (!isTEffsEmpty(typeE.eff) && typeE.eff.tag !== 'TMeta')
    return Left(`cannot generalize over ${showTypeEff(typeE)}`);
  const type = typeE.type;
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
  return Right(Forall(args, substMeta(sub, type)));
};

export const inferGen = (ctx: Context, expr: Expr): TC<Forall> => {
  resetId();
  const ty = infer(ctx, expr);
  if (isLeft(ty)) return ty;
  console.log(`infer done ${showTypeEff(ty.val)}`);
  const f = generalize(ctx, ty.val);
  if (isLeft(f)) return f;
  return Right(f.val);
};
