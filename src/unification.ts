import { Type, isTMeta, isTApp, TApp, showType, TMeta } from "./types";
import { err } from "./utils";

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

const bind = (a: TMeta, b: Type): void => {
  occurs(a, b);
  a.type = b;
};

export const unify = (a_: Type, b_: Type): void => {
  if (a_ === b_) return;
  const a = prune(a_);
  const b = prune(b_);
  if (a === b) return;

  if (isTMeta(a)) return bind(a, b);
  if (isTMeta(b)) return bind(b, a);

  if (isTApp(a) && isTApp(b)) return unify(a.left, b.left), unify(a.right, b.right);

  return err(`cannot unify ${showType(a)} ~ ${showType(b)}`);
};
