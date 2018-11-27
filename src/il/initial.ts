import Context from './context';
import { nType, nEffs, nEff, nComp } from './kinds';
import { ckvar } from './elems';

const initialContext = Context.of(
  ckvar(nType),
  ckvar(nComp),
  ckvar(nEffs),
  ckvar(nEff),
);

export default initialContext;
