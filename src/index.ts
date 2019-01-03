import { setContext } from './context';
import { Var, showExpr, abs, app } from './exprs';
import { Plain } from './names';
import { CTVar, CVar } from './elems';
import { TVar, showType } from './types';
import { infer } from './inference';
import { setLogging } from './logging';

/**
TODO:
  - pretty printing of types
  - kind inference
*/

const $ = Plain;
const [x, y, z] = ['x', 'y', 'z'].map($);

const Bool = $('Bool');
const True = $('True');

setContext([
  CTVar(Bool),
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
  console.log(showType(type));
  console.log(`${time}`);
} catch(err) {
  console.log('' + err);
}
