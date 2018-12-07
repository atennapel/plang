import { inferGen } from "./inference";
import { initial, extendContextMut } from "./context";
import { throwEither } from "./either";
import { abs, Var, app, showExpr } from "./exprs";
import { showForall, TVar, Forall, tfun, tapp, TFun, teffs } from "./types";
import { kType, kfun, kEff, kEffs } from "./kinds";

/*
TODO:
  - handlers
  - polymorphic effects
  - polymorphic operations
  - row polymorphic records/variants
  - type recursion
  - adts

Bugs:
    - app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t')))))))
      effects dissapear ^
*/

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
    State: kEff,
  },
  {
    Void: Forall([['t', kType]], tfun(tv('Void'), tv('t'))),
    Unit: Forall([], tv('Unit')),
    True: Forall([], tv('Bool')),
    False: Forall([], tv('Bool')),

    and: Forall([], tfun(tv('Bool'), tv('Bool'), tv('Bool'))),
    
    id: Forall([['t', kType]], tfun(tv('t'), tv('t'))),
    singleton: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')))),

    Nil: Forall([['t', kType]], tapp(tv('List'), tv('t'))),
    Cons: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')), tapp(tv('List'), tv('t')))),

    flip: Forall([], TFun(tv('Unit'), teffs([tv('Flip')]), tv('Bool'))),
    get: Forall([], TFun(tv('Unit'), teffs([tv('State')]), tv('Bool'))),

    fix: Forall([['t', kType]], tfun(tfun(tv('t'), tv('t')), tv('t'))),
    caseList: Forall([['t', kType], ['r', kType], ['e', kEffs]], tfun(tv('r'), tfun(tv('t'), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r'))), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r')))),
  },
);
const expr = app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t')))))));
console.log(`${showExpr(expr)}`);
let time = Date.now();
const res = throwEither(inferGen(ctx, expr, true));
time = Date.now() - time;
console.log(`${showForall(res)}`);
console.log(`${time}ms`);
