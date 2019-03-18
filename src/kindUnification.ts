import { eqName } from './names';
import { context, discardContext, storeContext, restoreContext, applyKind, namestore } from './global';
import { CKMeta } from './elems';
import { KMeta, Kind, isKVar, isKMeta, isKFun, containsKMeta, showKind, KFun } from './kinds';
import { InferError, infererr } from './error';
import { wfKind } from './wellformedness';
import { log } from './config';

const solveKind = (x: KMeta, kind: Kind): void => {
  const elem = context.lookup('CKMeta', x.name);
  if (!elem) return infererr('solve with undefined kmeta');
  const right = context.split('CKMeta', x.name);
  wfKind(kind);
  context.add(CKMeta(x.name, kind));
  context.addAll(right);
};

const instKind = (x: KMeta, kind: Kind): void => {
  log(`instKind ${showKind(x)} ~ ${showKind(kind)} in ${context}`);
  storeContext();
  try {
    solveKind(x, kind);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isKMeta(kind)) return solveKind(kind, x);
    if (isKFun(kind)) {
      const y = x.name;
      const a = namestore.fresh(y);
      const b = namestore.fresh(y);
      const ta = KMeta(a);
      const tb = KMeta(b);
      context.replace('CKMeta', y, [
        CKMeta(b),
        CKMeta(a),
        CKMeta(y, KFun(ta, tb)),
      ]);
      instKind(ta, kind.left);
      instKind(tb, applyKind(kind.right));
      return;
    }
    return infererr(`inst kind failed: ${showKind(x)} := ${showKind(kind)}, ${err}`);
  }
};

export const unifyKinds = (a: Kind, b: Kind): void => {
  log(`unifyKinds ${showKind(a)} ~ ${showKind(b)} in ${context}`);
  if (a === b) return;
  if (isKVar(a) && isKVar(b) && eqName(a.name, b.name)) return;
  if (isKMeta(a) && isKMeta(b) && eqName(a.name, b.name)) return;
  if (isKFun(a) && isKFun(b)) {
    unifyKinds(a.left, b.left);
    return unifyKinds(applyKind(a.right), applyKind(b.right));
  }
  if (isKMeta(a)) {
    if (containsKMeta(a.name, b))
      return infererr(`kind occurs check L failed: ${showKind(a)} in ${showKind(b)}`);
    return instKind(a, b);
  }
  if (isKMeta(b)) {
    if (containsKMeta(b.name, a))
      return infererr(`kind occurs check R failed: ${showKind(b)} in ${showKind(a)}`);
    return instKind(b, a);
  }
  return infererr(`kind unify failed: ${showKind(a)} ~ ${showKind(b)}`)
};
