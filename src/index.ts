import {
  tunit,
  tvar,
  tex,
  tfuns,
  tforalls,
} from './types';
import {
  eunit,
  evar,
  eapps,
  eabss,
  eanno,
} from './exprs';
import compile from './compilerJS';

console.log('' + compile(eapps(eabss(['x'], evar('x')), eunit)));
