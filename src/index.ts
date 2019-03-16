import { Name } from './names';
import {
  KVar,
  KMeta,
  kfun,
  kType,
  showKind,
} from './kinds';
import {
  TVar,
  TMeta,
  tapp,
  tforallK,
  tfun,
  showType,
} from './types';
import {
  Var,
  abs,
  app,
  Ann,
  showTerm,
} from './terms';
import { infer } from './inference';

const a = Name('a');
const b = Name('b');
const c = Name('c');
const x = Name('x');
const y = Name('y');
const z = Name('z');
const f = Name('f');
const g = Name('g');

const term = abs([x, y], Var(x));
console.log(showTerm(term));
const ty = infer(term);
console.log(showType(ty));
