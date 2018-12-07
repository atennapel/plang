import { inferGen } from "./inference";
import { initial, extendContextMut } from "./context";
import { throwEither } from "./either";
import { abs, Var, app, showExpr } from "./exprs";
import { showForall, TVar, Forall, tfun, tapp } from "./types";
import { kType, kfun } from "./kinds";

const v = Var;
const tv = TVar;

const ctx = extendContextMut(initial,
  {},
  {
    Unit: kType,

    List: kfun(kType, kType),
  },
  {
    Unit: Forall([], tv('Unit')),
    
    id: Forall([['t', kType]], tfun(tv('t'), tv('t'))),
    singleton: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')))),
  },
);
const expr = abs(['x'], app(v('singleton'), app(v('singleton'), v('x'))));
console.log(`${showExpr(expr)}`);
const res = throwEither(inferGen(ctx, expr));
console.log(`${showForall(res)}`);
