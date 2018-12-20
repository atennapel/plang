import { Env } from "./env";
import { Let, Var, showExpr, abs, app } from "./exprs";
import { inferTop } from "./inference";
import { showType, tfun, TVar, TConst, tfuns } from "./types";

const tv = TVar;
const v = Var;

const Bool = TConst('Bool');
const Int = TConst('Int');

const microtime = require('microtime');
const env: Env = {
  True: Bool,
  zero: Int,
  k: tfuns(tv(0), tv(1), tv(0)),
};
const expr = app(v('k'), v('True'));
console.log(showExpr(expr));
let time = microtime.now();
const type = inferTop(env, expr);
time = microtime.now() - time;
console.log(showType(type));
console.log(`${time}`);
