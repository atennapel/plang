import { Env } from "./env";
import { Let, Var, showExpr, abs, app, Anno, Select, Inject } from "./exprs";
import { inferTop } from "./inference";
import { showType, tfun, TVar, TConst, tfuns, tapp, TRowEmpty, TRowExtend, TRecord } from "./types";
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
  fix: tfuns(tfuns(ta, ta), ta),
  empty: tapp(TRecord, TRowEmpty),
  objX: tapp(TRecord, TRowExtend('x', Bool, TRowEmpty)),
  objY: tapp(TRecord, TRowExtend('y', Bool, TRowEmpty)),
  objYX: tapp(TRecord, TRowExtend('y', Bool, TRowExtend('x', Int, TRowEmpty))),
};
const expr = app(Inject('Just'), v('True'));
console.log(showExpr(expr));

try {
  let time = microtime.now();
  const type = inferTop(env, expr);
  time = microtime.now() - time;
  console.log(showType(type));
  console.log(`${time}`);
} catch(err) {
  console.log('' + err);
}
