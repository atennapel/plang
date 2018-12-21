import { Env } from "./env";
import { Let, Var, showExpr, abs, app, Anno } from "./exprs";
import { inferTop } from "./inference";
import { showType, tfun, TVar, TConst, tfuns, tapp } from "./types";
import { KType, kfun } from "./kinds";

const tv = TVar;
const v = Var;

const Bool = TConst('Bool');
const Int = TConst('Int');
const List = TConst('List', kfun(KType, KType));

const ta = tv(0);
const tb = tv(1);

const microtime = require('microtime');
const env: Env = {
  True: Bool,
  zero: Int,
  k: tfuns(ta, tb, ta),
  singleton: tfuns(ta, tapp(List, ta)),
  list: List,
  f: tfuns(Bool, Int, Bool),
};
const expr = Let('id', Anno(abs(['x'], v('x')), tfuns(ta, ta)), app(v('f'), app(v('id'), v('True')), app(v('id'), v('zero'))));
console.log(showExpr(expr));
let time = microtime.now();
const type = inferTop(env, expr);
time = microtime.now() - time;
console.log(showType(type));
console.log(`${time}`);
