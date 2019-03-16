import { eqName } from './names';
import { context, discardContext, storeContext, restoreContext, applyKind } from './global';
import { CKMeta } from './elems';
import { KMeta, Kind, isKVar, isKMeta, isKFun, containsKMeta, showKind } from './kinds';
import { InferError, infererr } from './error';
import { wfKind } from './wellformedness';

const solveKind = (x: KMeta, kind: Kind): void => {
  const elem = context.lookup('CKMeta', x.name);
  if (!elem) return infererr('solve with undefined kmeta');
  const right = context.split('CKMeta', x.name);
  wfKind(kind);
  context.add(CKMeta(x.name, kind));
  context.addAll(right);
};

const instKind = (x: KMeta, kind: Kind): void => {
  storeContext();
  try {
    solveKind(x, kind);
    discardContext();
  } catch (err) {
    if (!(err instanceof InferError)) throw err;
    restoreContext();
    if (isKMeta(kind)) return solveKind(kind, x);
    return infererr(`inst kind failed: ${showKind(x)} := ${showKind(kind)}, ${err}`);
  }
};

export const unifyKinds = (a: Kind, b: Kind): void => {
  console.log(`unifyKinds ${showKind(a)} ~ ${showKind(b)} in ${context}`);
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
