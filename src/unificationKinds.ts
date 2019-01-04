import { Kind, showKind, isKVar, isKMeta, isKFun, containsKMeta, KMeta, KFun } from './kinds';
import { log } from './logging';
import { err, InferError } from './errors';
import { eqName, Name, showName, freshName } from './names';
import { applyKind, context, setContext, split, add, addAll, replace } from './context';
import { matchCKMeta, CKMeta } from './elems';
import { wfKind } from './wellformedness';

const bindKind = (kv: Name, kind: Kind): void => {
  log(`bindKind: ${showName(kv)} := ${showKind(kind)}`);
  if (containsKMeta(kv, kind)) return err(`infinite kind: ${showName(kv)} in ${showKind(kind)}`);
  const right = split(matchCKMeta(kv));
  wfKind(kind);
  add(CKMeta(kv, kind));
  addAll(right);
};

const instKind = (x: Name, kind: Kind): void => {
  log(`instKind: ${showName(x)} := ${showKind(kind)}`);
  const old = context.slice(0);
  try {
    bindKind(x, kind);
  } catch (error) {
    if (!(error instanceof InferError)) throw error;
    setContext(old);
    if (isKMeta(kind)) return bindKind(kind.name, KMeta(x));
    if (isKFun(kind)) {
      const a = freshName(x);
      const b = freshName(x);
      replace(matchCKMeta(x), [
        CKMeta(b),
        CKMeta(a),
        CKMeta(x, KFun(KMeta(a), KMeta(b))),
      ]);
      instKind(a, kind.left);
      instKind(b, applyKind(kind.right));
      return;
    }
    return err(`instKind failed: ${showName(x)} := ${showKind(kind)}`);
  }
};

export const unifyKinds = (k1: Kind, k2: Kind): void => {
  log(`unifyKinds: ${showKind(k1)} ~ ${showKind(k2)}`);
  if (k1 === k2) return;
  if (isKVar(k1) && isKVar(k2) && eqName(k1.name, k2.name)) return;
  if (isKMeta(k1) && isKMeta(k2) && eqName(k1.name, k2.name)) return;
  if (isKMeta(k1)) return instKind(k1.name, k2);
  if (isKMeta(k2)) return instKind(k2.name, k1);
  if (isKFun(k1) && isKFun(k2)) {
    unifyKinds(k1.left, k2.left);
    unifyKinds(applyKind(k1.right), applyKind(k2.right));
    return;
  }
  return err(`unifyKinds failed: ${showKind(k1)} ~ ${showKind(k2)}`);
};
