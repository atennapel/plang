import { Env } from "./env";
import { Let, Var, showExpr, abs, app } from "./exprs";
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
};
const expr = app(v('singleton'), v('True'));
console.log(showExpr(expr));
let time = microtime.now();
const type = inferTop(env, expr);
time = microtime.now() - time;
console.log(showType(type));
console.log(`${time}`);
