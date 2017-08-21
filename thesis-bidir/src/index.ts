import {
  tunit,
  tvar,
  texists,
  tarr,
  tforall,
} from './types';
import { id } from './Id';
import Context from './Context';
import { InferState, subtype } from './typechecker';

const x = id('x', 0);
const y = id('y', 0);
const tx = tvar(x);
const ty = tvar(y);

const a = tarr(tunit, tunit);
const b = tarr(tunit, tunit);

console.log(''+subtype(InferState.empty(), new Context([]), a, b).map(({c}) => c));
