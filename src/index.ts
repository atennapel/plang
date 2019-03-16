import { Name } from './names';
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
import {
  Var,
  abs,
  app,
  Ann,
  showTerm,
} from './terms';

const a = Name('a');
const b = Name('b');
const c = Name('c');
const x = Name('x');
const y = Name('y');
const z = Name('z');
const f = Name('f');
const g = Name('g');

const kind = kfun(KVar(a), KMeta(b), kfun(KVar(x), KVar(y), KMeta(z)), KVar(c));
const type = tforallK([[a, kind], [b, null]], tapp(TVar(a), TMeta(b), TVar(c), tapp(TVar(x), TVar(y))));
const term = abs([x, y, z], app(Var(f), abs([x], Var(x)), app(Var(g), Var(z)), Ann(Var(y), type)));
console.log(showTerm(term));
