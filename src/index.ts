import { setContext, addAll } from './context';
import { Var, showExpr, abs, app } from './exprs';
import { Plain } from './names';
import { CTVar, CVar } from './elems';
import { TVar, prettyType, tforall, tfun, tapp } from './types';
import { infer } from './inference';
import { setLogging } from './logging';
import { setupKinds, kType, kfun } from './kinds';

/**
TODO:
  - check that metas are solved in the correct order in unification,
    and that the to-be-bound type/kind is wellformed
  - constraints
*/

const $ = Plain;
const v = Var;
const tv = TVar;
const [x, y, z, t] = ['x', 'y', 'z', 't'].map($);

const Bool = $('Bool');
const True = $('True');
const List = $('List');
const single = $('single');
const extract = $('extract');

setContext();
setupKinds();
addAll([
  CTVar(Bool, kType),
  CVar(True, tv(Bool)),

  CTVar(List, kfun(kType, kType)),
  CVar(single, tforall([[t, kType]], tfun(tv(t), tapp(tv(List), tv(t))))),
  CVar(extract, tforall([[t, kType]], tfun(tapp(tv(List), tv(t)), tv(t)))),
]);

const expr = app(v(extract), app(v(single), v(True)));
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
