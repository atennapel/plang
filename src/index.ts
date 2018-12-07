import { inferGen } from "./inference";
import { initial, extendContextMut } from "./context";
import { throwEither } from "./either";
import { abs, Var, app, showExpr } from "./exprs";
import { showForall, TVar, Forall, tfun, tapp, TFun, teffs } from "./types";
import { kType, kfun, kEff } from "./kinds";

const v = Var;
const tv = TVar;

const ctx = extendContextMut(initial,
  {},
  {
    Void: kType,
    Unit: kType,
    Bool: kType,

    List: kfun(kType, kType),

    Flip: kEff,
  },
  {
    Void: Forall([['t', kType]], tfun(tv('Void'), tv('t'))),
    Unit: Forall([], tv('Unit')),
    True: Forall([], tv('Bool')),
    False: Forall([], tv('Bool')),
    
    id: Forall([['t', kType]], tfun(tv('t'), tv('t'))),
    singleton: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')))),

    flip: Forall([], TFun(tv('Unit'), teffs([tv('Flip')]), tv('Bool'))),
  },
);
const expr = app(v('flip'), v('Unit'));
console.log(`${showExpr(expr)}`);
const res = throwEither(inferGen(ctx, expr));
console.log(`${showForall(res)}`);
