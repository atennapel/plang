import { setContext, addAll } from './context';
import { Var, showExpr, abs, app } from './exprs';
import { Plain } from './names';
import { CTVar, CVar } from './elems';
import { TVar, prettyType } from './types';
import { infer } from './inference';
import { setLogging } from './logging';
import { setupKinds, kType, kfun } from './kinds';

/**
TODO:
  - higher kinded types
  - constraints
*/

const $ = Plain;
const [x, y, z] = ['x', 'y', 'z'].map($);

const Bool = $('Bool');
const True = $('True');

setContext();
setupKinds();
addAll([
  CTVar(Bool, kType),
  CVar(True, TVar(Bool)),
]);

const expr = abs([x, y], Var(x));
console.log(showExpr(expr));
const microtime = require('microtime');
setLogging(true, true);
try {
  let time = microtime.now();
  const type = infer(expr);
  time = microtime.now() - time;
  console.log(prettyType(type));
  console.log(`${time}`);
} catch(err) {
  console.log('' + err);
}
