import { infer } from './typechecker';
import {
  eanno,
  eapp,
  evar,
  elam,
  eunit,
} from './exprs';

const e = eapp(elam(['x'], evar('x')), eunit);
console.log(''+infer(e).map(({c, t}) => t));
