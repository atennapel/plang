import Context from './generic/context';
import { kvar } from './kinds';
import NameRep, { name } from './generic/NameRep';
import Elem, { ckvar } from './elems';

export const nType = name('Type');
export const kType = kvar(nType);

export const initialContext = Context.of<Elem<NameRep>>(
  ckvar(nType),
);
