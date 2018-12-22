import { Env } from "./env";
import { Let, Var, showExpr, abs, app, Anno, Select, Inject, Extend, Case } from "./exprs";
import { inferTop } from "./inference";
import { showType, tfun, TVar, TConst, tfuns, tapp, TRowEmpty, TRowExtend, TRecord, TVariant } from "./types";
import { KType, kfun, KRow } from "./kinds";

const tv = TVar;
const v = Var;

const Bool = TConst('Bool');
const Int = TConst('Int');
const List = TConst('List', kfun(KType, KType));

const ta = tv(0);
const tb = tv(1);
const tr = tv(1, KRow);

const microtime = require('microtime');
const env: Env = {
  True: Bool,
  zero: Int,
  inc: tfuns(Int, Int),
  k: tfuns(ta, tb, ta),
  singleton: tfuns(ta, tapp(List, ta)),
  list: List,
  fix: tfuns(tfuns(ta, ta), ta),
  empty: tapp(TRecord, TRowEmpty),
  end: tfuns(tapp(TVariant, TRowEmpty), ta),
  objX: tapp(TRecord, TRowExtend('x', Bool, TRowEmpty)),
  objY: tapp(TRecord, TRowExtend('y', Bool, TRowEmpty)),
  objYX: tapp(TRecord, TRowExtend('y', Bool, TRowExtend('x', Int, TRowEmpty))),
};

const expr = app(
  Case('Just'),
  v('inc'),
  app(
    Case('Nothing'),
    app(v('k'), v('zero')),
    v('end'),
  ),
);
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
