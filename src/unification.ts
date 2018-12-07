import { Type, caseType, TMeta, TApp } from "./types";
import { Context, findTVar, findMeta } from "./context";
import { Kind, eqKind } from "./kinds";
import { TC } from "./TC";
import { fromMaybe, isLeft, Left, Right } from "./either";

export const prune = (type: Type): Type => caseType(type, {
  TVar: name => type,
  TMeta: (name, ty) => {
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
  TVar: name => fromMaybe(findTVar(ctx, name), () => `undefined tvar ${name}`),
  TMeta: (name, ty) => fromMaybe(findMeta(ctx, name), () => `undefined tmeta ${name}`),
  TApp: (left, right, kind) => {
    if (kind) return Right(kind);
    const kle = kindOf(ctx, left); if (isLeft(kle)) return kle;
    const kl = kle.val;
    if (kl.tag !== 'KFun') return Left(`not a hkt in left side of ${type}`);
    const kre = kindOf(ctx, right); if (isLeft(kre)) return kre;
    const kr = kre.val;
    if (!eqKind(kl.left, kr)) return Left(`invalid argument kind in ${type}`);
    return Right(kl.right);
  },
});

export const bind

export const unify
