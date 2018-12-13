import { Type, caseType, TMeta, TApp, showType, matchTEffsExtend, TEffsExtend, freshMeta } from "./types";
import { Context, findTVar } from "./context";
import { Kind, eqKind, showKind, kEffs } from "./kinds";
import { TC, ok } from "./TC";
import { isLeft, Left, Right, isRight } from "./either";
import { Name, fresh } from "./names";

export const prune = (type: Type): Type => caseType(type, {
  TVar: name => type,
  TMeta: (name, kind, ty) => {
    if (!ty) return type;
    const t = prune(ty);
    if (t !== ty) (type as TMeta).type = t;
    return t;
  },
  TApp: (left, right, kind) => {
    const l = prune(left);
    const r = prune(right);
    return l === left && r === right ? type: TApp(l, r, kind);
  },
});

export const kindOf = (ctx: Context, type: Type): TC<Kind> => caseType(type, {
  TVar: name => findTVar(ctx, name),
  TMeta: (name, kind) => Right(kind),
  TApp: (left, right, kind) => {
    if (kind) return Right(kind);
    const kle = kindOf(ctx, left); if (isLeft(kle)) return kle;
    const kl = kle.val;
    if (kl.tag !== 'KFun') return Left(`not a hkt in left side of ${showType(type)}`);
    const kre = kindOf(ctx, right); if (isLeft(kre)) return kre;
    const kr = kre.val;
    if (!eqKind(kl.left, kr)) return Left(`invalid argument kind in ${showType(type)}`);
    (type as TApp).kind = kl.right;
    return Right(kl.right);
  },
});

const occurs = (m: Name, type: Type): boolean => caseType(type, {
  TVar: name => false,
  TMeta: name => name === m,
  TApp: (left, right) => occurs(m, left) || occurs(m, right),
});

export const bind = (meta: TMeta, type: Type): TC<true> => {
  console.log(`${showType(meta)} := ${showType(type)}`);
  if (type.tag === 'TMeta' && meta.name === type.name) return ok;
  if (occurs(meta.name, type)) return Left(`${showType(meta)} occurs in ${showType(type)}`);
  meta.type = type;
  return ok;
};

const rewriteEffs = (ctx: Context, eff: Type, type: Type): TC<Type> => {
  console.log(`rewriteEffs ${showType(eff)} in ${showType(type)}`);
  const ex = matchTEffsExtend(type);
  if (ex) {
    const rest = ex.rest;
    const u = unify(ctx, eff, ex.eff);
    if (isRight(u)) return Right(prune(rest));
    if (rest.tag === 'TMeta') {
      const r = freshMeta('e', kEffs);
      const ret = bind(rest, TEffsExtend(prune(eff), r));
      if (isLeft(ret)) return ret;
      return Right(TEffsExtend(prune(ex.eff), r));
    }
    const rec = rewriteEffs(ctx, eff, ex.rest);
    if (isLeft(rec)) return rec;
    return Right(TEffsExtend(prune(ex.eff), rec.val));
  }
  return Left(`cannot rewrite effs: ${showType(eff)} in ${showType(type)}`);
};

export const unify = (ctx: Context, a_: Type, b_: Type, pr = true): TC<true> => {
  // eq check & prune
  let a = a_;
  let b = b_;
  if (a === b) return ok;
  if (pr) { a = prune(a_); if (a === b_) return ok }
  if (pr) { b = prune(b_); if (a === b) return ok }
  console.log(`${showType(a)} ~ ${showType(b)}`);
  // kind check
  const ka = kindOf(ctx, a);
  if (isLeft(ka)) return ka;
  const kb = kindOf(ctx, b);
  if (isLeft(kb)) return kb;
  if (!eqKind(ka.val, kb.val))
    return Left(`kind mismatch: (${showType(a)} :k ${showKind(ka.val)}) ~ (${showType(b)} :k ${showKind(kb.val)})`);
  // structural
  if (a.tag === 'TMeta') return bind(a, b);
  if (b.tag === 'TMeta') return bind(b, a);
  if (a.tag === 'TVar' && b.tag === 'TVar' && a.name === b.name) return ok;
  const ea = matchTEffsExtend(a);
  const eb = matchTEffsExtend(b);
  if (ea && eb) {
    const reb = rewriteEffs(ctx, ea.eff, b);
    if (isLeft(reb)) return reb;
    return unify(ctx, ea.rest, reb.val, false);
  }
  if (a.tag === 'TApp' && b.tag === 'TApp') {
    const left = unify(ctx, a.left, b.left, false);
    if (isLeft(left)) return left;
    return unify(ctx, a.right, b.right);
  }
  return Left(`cannot unify ${showType(a)} ~ ${showType(b)}`);
};
