import { Type, isTMeta, isTApp, TApp, showType, TMeta, isTConst, isTVar, isTRowExtend, TRowExtend, trow, TRowEmpty } from "./types";
import { err } from "./utils";
import { unifyKind } from "./kindUnification";
import { Kind, KType, freshKMeta, KFun, KRow } from "./kinds";
import { Occ } from "./inference";
import { Name } from "./names";

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

const rewriteRow = (l: Name, ty: Type): TRowExtend => {
  const row: [Name, Type][] = [];
  let c = ty;
  while (isTRowExtend(c)) {
    if (c.label === l) {
      return TRowExtend(l, c.type, trow(row, c.rest));
    } else row.push([c.label, c.type]);
  }
  if (c === TRowEmpty) return err(`cannot rewriteRow ${l} in ${showType(c)}`);
  return err('unimplemented tmeta in rewriteRow');
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
  if (isTRowExtend(type)) {
    const kt = inferKind(type.type);
    unifyKind(kt, KType);
    const kr = inferKind(type.rest);
    unifyKind(kr, KRow);
    return KRow;
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

  if (isTRowExtend(a) && isTRowExtend(b)) {
    const rewritten = rewriteRow(a.label, b);
  }

  return err(`cannot unify ${showType(a)} ~ ${showType(b)}`);
};
