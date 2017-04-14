import {
  Type,
  TVar,
  TCon,
  TApp,
  Bool,
  Int,
  Float,
  Str,
  tarr,
  TScheme,
} from './types';
import Set from './Set';
import Env from './Env';
import { InferState, infer } from './typechecker';
import {
  vr,
  lam,
  app,
  lt,
  recempty,
  sel,
  extend,
} from './exprs';

let env = new Env({
  True: TScheme.of(Bool),
  str: TScheme.of(Str),
  one: TScheme.of(Float),
});

let True = vr('True');
let str = vr('str');
let one = vr('one');

let e = app(sel('x'), app(extend('y'), True, app(extend('x'), one, recempty())));
console.log(''+e);
let i = infer(env, e);
console.log(''+i);
