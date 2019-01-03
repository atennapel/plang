import { Kind, showKind, isKVar, isKMeta, isKFun, containsKMeta } from './kinds';
import { log } from './logging';
import { err } from './errors';
import { eqName, Name, showName } from './names';
import { applyKind, replace } from './context';
import { matchCKMeta, CKMeta } from './elems';

const bind = (kv: Name, kind: Kind): void => {
  if (containsKMeta(kv, kind)) return err(`infinite kind: ${showName(kv)} in ${showKind(kind)}`);
  replace(matchCKMeta(kv), [CKMeta(kv, kind)]);
};

export const unifyKinds = (k1: Kind, k2: Kind): void => {
  log(`unifyKinds: ${showKind(k1)} ~ ${showKind(k2)}`);
  if (k1 === k2) return;
  if (isKVar(k1) && isKVar(k2) && eqName(k1.name, k2.name)) return;
  if (isKMeta(k1) && isKMeta(k2) && eqName(k1.name, k2.name)) return;
  if (isKMeta(k1)) return bind(k1.name, k2);
  if (isKMeta(k2)) return bind(k2.name, k1);
  if (isKFun(k1) && isKFun(k2)) {
    unifyKinds(k1.left, k2.left);
    unifyKinds(applyKind(k1.right), applyKind(k2.right));
    return;
  }
  return err(`unifyKinds failed: ${showKind(k1)} ~ ${showKind(k2)}`);
};
