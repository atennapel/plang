import {
  KVar,
  KMeta,
  kfun,
  showKind,
} from './kinds';
import {
  TVar,
  TMeta,
  tapp,
  tforallK,
  showType,
} from './types';

const kind = kfun(KVar('a'), KMeta('b'), kfun(KVar('x'), KVar('y'), KMeta('z')), KVar('c'));
const type = tforallK([['a', kind], ['b', null]], tapp(TVar('a'), TMeta('b'), TVar('c'), tapp(TVar('x'), TVar('y'))));
console.log(showType(type));
