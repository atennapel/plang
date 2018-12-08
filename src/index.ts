import { inferGen } from "./inference";
import { initial, extendContextMut } from "./context";
import { throwEither } from "./either";
import { abs, Var, app, showExpr } from "./exprs";
import { showForall, TVar, Forall, tfun, tapp, TFun, teffs, prettyForall } from "./types";
import { kType, kfun, kEff, kEffs } from "./kinds";

/*
Questions:
  - how much needs to be opened and closed?
  - rethink closeFun (maybe after adding Forall?)
TODO:
  - pretty forall and kinds
  - replace recursion with iteration where convenient
  - fix openFun and closeFun
  - pretty printer
  - handlers
  - polymorphic effects
  - polymorphic operations
  - row polymorphic records/variants
  - type recursion
  - adts

Bugs:
    effects dissapear:
    - app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t')))))))
    - abs(['f', 'x'], app(v('and'), app(v('f'), v('x')), v('True')))
    - app(v('app'), v('flip'))

    effects can be ommitted:
    - \x y -> x
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

    not: Forall([], tfun(tv('Bool'), tv('Bool'))),
    and: Forall([], tfun(tv('Bool'), tv('Bool'), tv('Bool'))),
    
    id: Forall([['t', kType]], tfun(tv('t'), tv('t'))),
    singleton: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')))),

    Nil: Forall([['t', kType]], tapp(tv('List'), tv('t'))),
    Cons: Forall([['t', kType]], tfun(tv('t'), tapp(tv('List'), tv('t')), tapp(tv('List'), tv('t')))),

    flip: Forall([], TFun(tv('Unit'), teffs([tv('Flip')]), tv('Bool'))),
    get: Forall([], TFun(tv('Unit'), teffs([tv('State')]), tv('Bool'))),

    fix: Forall([['t', kType]], tfun(tfun(tv('t'), tv('t')), tv('t'))),
    caseList: Forall([['t', kType], ['r', kType], ['e', kEffs]], tfun(tv('r'), tfun(tv('t'), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r'))), TFun(tapp(tv('List'), tv('t')), tv('e'), tv('r')))),

    app: Forall([['a', kType], ['b', kType]], tfun(tfun(tv('a'), tv('b')), tv('a'), tv('b'))),
  },
);
//const expr = app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t')))))));
const expr = abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t'))))));
//const expr = abs(['f', 'x'], app(v('and'), v('True'), app(v('f'), v('x'))));
//const expr = abs(['x', 'y'], v('x'));
//const expr = abs(['f', 'x', 'b'], app(v('f'), v('x')));
//const expr = app(abs(['f'], app(v('f'), v('flip'))), abs(['g'], app(v('g'), v('Unit'))));
console.log(`${showExpr(expr)}`);
let time = Date.now();
const res = throwEither(inferGen(ctx, expr, true));
time = Date.now() - time;
console.log(`${prettyForall(res)}`);
console.log(`${time}ms`);
