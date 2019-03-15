import {
  KVar,
  KMeta,
  kfun,
  showKind,
} from './kinds';

const kind = kfun(KVar('a'), KMeta('b'), kfun(KVar('x'), KVar('y'), KMeta('z')), KVar('c'));
console.log(showKind(kind));
