import { caseExpr, Expr, showExpr, Abs, Var } from "./exprs";
import { TC } from "./TC";
import { Type, Forall, TMeta, TFun, Subst, substMeta, substTVar, freeMeta, TVar, TFunP, tEffsEmpty, isTEffsEmpty, showType, TypeEff, typePure, showTypeEff, typeEff, matchTEffsExtend, TEffsExtend, matchTFun, flattenTEffs, teffs, containsMeta, showForall, occTVar, OccMap, freshMeta } from "./types";
import { Context, findVar, extendVar, findOp } from "./context";
import { isLeft, Right, Left } from "./either";
import { prune, unify } from "./unification";
import { fresh, resetId, Name } from "./names";
import { kType, Kind, kEffs, eqKind } from "./kinds";

const openFun = (type: Type): Type => {
  const f = matchTFun(type);
  if (!f) return type;
  console.log(`openFun ${showType(type)}`);
  const fl = flattenTEffs(f.eff);
  if (!isTEffsEmpty(fl.rest)) return type;
  const ret = TFun(f.left, teffs(fl.ts, freshMeta('e', kEffs)), openFun(f.right));
  console.log(`opened ${showType(ret)}`);
  return ret;
};

const instantiate = (type: Forall): Type => {
  const sub: Subst = {};
  const args = type.args;
  for (let i = args.length - 1; i >= 0; i--) {
    const c = args[i];
    const n = c[0];
    const k = c[1];
    sub[n] = freshMeta(n, k);
  }
  return openFun(substTVar(sub, type.type));
};

export const infer = (ctx: Context, expr: Expr): TC<TypeEff> => {
  console.log(`infer ${showExpr(expr)}`);
  return caseExpr(expr, {
    Var: name => {
      const ty = findVar(ctx, name);
      if (isLeft(ty)) return ty;
      return Right(typeEff(instantiate(ty.val), freshMeta('e', kEffs)));
    },
    Abs: (arg, type, body) => {
      const m = type || freshMeta(arg, kType);
      const res = infer(extendVar(ctx, arg, Forall([], m)), body);
      if (isLeft(res)) return res;
      const ty = typeEff(TFun(prune(m), res.val.eff, res.val.type), freshMeta('e', kEffs));
      return Right(ty);
    },
    App: (left, right) => {
      const lr = infer(ctx, left);
      if (isLeft(lr)) return lr;
      const rr = infer(ctx, right);
      if (isLeft(rr)) return rr;
      const m = freshMeta('t', kType);
      const e = freshMeta('e', kEffs);
      const ur = unify(ctx, lr.val.type, TFun(rr.val.type, e, m), false);
      if (isLeft(ur)) return ur;
      const ure1 = unify(ctx, e, lr.val.eff);
      if (isLeft(ure1)) return ure1;
      const ure2 = unify(ctx, e, rr.val.eff);
      if (isLeft(ure2)) return ure2;
      const ty = typeEff(prune(m), prune(e));
      return Right(ty);
    },
    Handler: (map, value) => {
      const a = freshMeta('a');
      const b = freshMeta('b');
      const e = freshMeta('e', kEffs);
      const te = freshMeta('e', kEffs);
      const ret = infer(ctx, value || Abs('x', a, Var('x')));
      if (isLeft(ret)) return ret;
      const u1 = unify(ctx, TFun(a, e, b), ret.val.type);
      if (isLeft(u1)) return u1;
      const ute1 = unify(ctx, te, ret.val.eff);
      if (isLeft(ute1)) return ute1;
      const effs: Name[] = [];
      for (let op in map) {
        const opi = findOp(ctx, op);
        if (isLeft(opi)) return opi;
        effs.push(opi.val.eff);
        const retop = infer(ctx, map[op]);
        if (isLeft(retop)) return retop;
        const u2 = unify(ctx, TFunP(opi.val.param, TFun(TFun(opi.val.ret, e, b), e, b)), retop.val.type);
        if (isLeft(u2)) return u2;
        const ute2 = unify(ctx, prune(te), retop.val.eff);
        if (isLeft(ute2)) return ute2;
      }
      const pe = prune(e);
      return Right(typeEff(TFun(TFun(TVar('Unit'), teffs(effs.map(TVar), pe), prune(a)), pe, prune(b)), prune(te)));
    },
  });
};

const closeFunTy = (type: Type, occ: OccMap, closed: Name[] = []): Type => {
  const fn = matchTFun(type);
  if (fn) {
    const rec = closeFunTy(fn.right, occ, closed);
    const fl = flattenTEffs(fn.eff);
    if (fl.rest.tag === 'TVar' && occ[fl.rest.name] === 1) {
      closed.push(fl.rest.name);
      return TFun(fn.left, teffs(fl.ts, tEffsEmpty), rec);
    } else return TFun(fn.left, fn.eff, rec);
  }
  return type;
};

const closeFun = (type: Forall): Forall => {
  console.log(`closeFun ${showForall(type)}`);
  const args = type.args;
  const effs = args.filter(([n, k]) => eqKind(k, kEffs)).map(([n, _]) => n);
  const occ = occTVar(type.type, effs);
  const closed: Name[] = [];
  const ret = closeFunTy(type.type, occ, closed);
  const retty = Forall(args.filter(([n, k]) => closed.indexOf(n) === -1), ret);
  console.log(`closed ${showForall(retty)}`);
  return retty;
};

const generalize = (ctx: Context, typeE: TypeEff, topLevel = false): TC<Forall> => {
  if (!isTEffsEmpty(typeE.eff) && typeE.eff.tag !== 'TMeta')
    return Left(`cannot generalize over ${showTypeEff(typeE)}`);
  const type = typeE.type;
  const fr = freeMeta(type);
  if (!topLevel) {
    for (let name in ctx.vars) {
      for (let k in freeMeta(ctx.vars[name].type)) {
        (fr[k] as any) = undefined;
      }
    }
  }
  const args: [Name, Kind][] = [];
  const sub: Subst = {};
  for (let k in fr) {
    args.push([k, fr[k].kind]);
    sub[k] = TVar(k);
  }
  return Right(closeFun(Forall(args, substMeta(sub, type))));
};

export const inferGen = (ctx: Context, expr: Expr, topLevel = false): TC<Forall> => {
  resetId();
  const ty = infer(ctx, expr);
  if (isLeft(ty)) return ty;
  console.log(`infer done ${showTypeEff(ty.val)}`);
  const f = generalize(ctx, ty.val, topLevel);
  if (isLeft(f)) return f;
  return Right(f.val);
};
