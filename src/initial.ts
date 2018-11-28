import Context from './context';
import { nType, nEffs, nEff } from './kinds';
import { ckvar } from './elems';

const initialContext = Context.of(
  ckvar(nType),
  ckvar(nEffs),
  ckvar(nEff),
);

export default initialContext;
