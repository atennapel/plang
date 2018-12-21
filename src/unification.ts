import { Type, isTMeta, isTApp, TApp, showType, TMeta, isTConst, isTVar } from "./types";
import { err } from "./utils";
import { unifyKind } from "./kindUnification";
import { Kind, KType, freshKMeta, KFun } from "./kinds";
import { Occ } from "./inference";

export const prune = (type: Type): Type => {
  if (isTMeta(type)) {
    if (!type.type) return type;
    const ty = prune(type.type);
    type.type = ty;
    return ty;
  }
  if (isTApp(type)) return TApp(prune(type.left), prune(type.right));
  return type;
};

const occurs = (a: TMeta, b: Type): void => {
  if (a === b) return err('occurs check failed');
  if (isTApp(b)) return occurs(a, b.left), occurs(a, b.right);
}

const bind = (a: TMeta, b: Type, occ?: Occ): void => {
  if (occ && isTVar(b) && occ[a.name])
    return err(`cannot bind ${showType(a)} := ${showType(b)} in annotation`);
  occurs(a, b);
  a.type = b;
};

export const inferKind = (type: Type): Kind => {
  if (isTApp(type)) {
    const kf = inferKind(type.left);
    const ka = inferKind(type.right);
    const kr = freshKMeta();
    unifyKind(kf, KFun(ka, kr));
    return kr;
  }
  return type.kind;
};

export const unify = (a_: Type, b_: Type, occ?: Occ): void => {
  if (a_ === b_) return;
  const a = prune(a_);
  const b = prune(b_);
  if (a === b) return;

  unifyKind(inferKind(a), inferKind(b));

  if (isTMeta(a)) return bind(a, b, occ);
  if (isTMeta(b)) return bind(b, a, occ);

  if (isTApp(a) && isTApp(b)) return unify(a.left, b.left, occ), unify(a.right, b.right, occ);

  return err(`cannot unify ${showType(a)} ~ ${showType(b)}`);
};
