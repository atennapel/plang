import Context from './context';
import { nType, nEffs, nEff, kType } from './kinds';
import { ckvar, ctvar, cvar } from './elems';
import { name } from './NameRep';
import { tvar } from './types';

const initialContext = Context.of(
  ckvar(nType),
  ckvar(nEffs),
  ckvar(nEff),

  ctvar(name('Unit'), kType),
  cvar(name('Unit'), tvar(name('Unit'))),
);

export default initialContext;
