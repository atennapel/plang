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
  tforall,
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
const t = Name('t');

const v = Var;
const tv = TVar;

const id_ =  Ann(abs([y], v(y)), tforall([t], tfun(tv(t), tv(t))));
const const_ = abs([x, y], v(x));
const term = app(const_, id_);
console.log(showTerm(term));
const ty = infer(term);
console.log(showType(ty));
