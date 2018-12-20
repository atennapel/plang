import { err } from "./utils";
import { Kind, isKMeta, isKFun, KFun, KMeta, showKind } from "./kinds";

export const pruneKind = (kind: Kind): Kind => {
  if (isKMeta(kind)) {
    if (!kind.kind) return kind;
    const ki = pruneKind(kind.kind);
    kind.kind = ki;
    return ki;
  }
  if (isKFun(kind)) return KFun(pruneKind(kind.left), pruneKind(kind.right));
  return kind;
};

const occurs = (a: KMeta, b: Kind): void => {
  if (a === b) return err('occurs check failed');
  if (isKFun(b)) return occurs(a, b.left), occurs(a, b.right);
}

const bind = (a: KMeta, b: Kind): void => {
  occurs(a, b);
  a.kind = b;
};

export const unifyKind = (a_: Kind, b_: Kind): void => {
  if (a_ === b_) return;
  const a = pruneKind(a_);
  const b = pruneKind(b_);
  if (a === b) return;

  if (isKMeta(a)) return bind(a, b);
  if (isKMeta(b)) return bind(b, a);

  if (isKFun(a) && isKFun(b)) return unifyKind(a.left, b.left), unifyKind(a.right, b.right);

  return err(`cannot unify ${showKind(a)} ~ ${showKind(b)}`);
};
