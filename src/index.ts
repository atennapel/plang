import { inferGen } from "./inference";
import { initial, extendContextMut } from "./context";
import { throwEither } from "./either";
import { abs, Var, app, showExpr, lets } from "./exprs";
import { showForall, TVar, Forall, tfun, tapp, TFun, teffs, prettyForall, prettyType, TMeta, Type, TFunP, tEffsEmpty } from "./types";
import { kType, kfun, kEff, kEffs } from "./kinds";
import { fresh } from "./names";
import { prune } from "./unification";

/*
Questions:
  - how much needs to be opened and closed?
  - rethink closeFun (maybe after adding Forall?)
TODO:
  - replace recursion with iteration where convenient
  - fix openFun and closeFun
  - handlers
  - polymorphic effects
  - polymorphic operations
  - row polymorphic records/variants
  - type recursion
  - adts
  - pretty printer exprs

Bugs:
    - app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(v('rec'), v('f'), v('t')))))))
    - app(abs(['f'], app(v('f'), v('flip'))), abs(['g'], app(v('g'), v('Unit'))))
*/

const v = Var;
const tv = TVar;

const ctx = extendContextMut(initial,
  {},
  {
    Void: kType,
    Unit: kType,
    Bool: kType,

    Pair: kfun(kType, kType, kType),

    List: kfun(kType, kType),

    Flip: kEff,
    State: kEff,
  },
  {
    Void: Forall([['t', kType]], tfun(tv('Void'), tv('t'))),
    Unit: Forall([], tv('Unit')),
    True: Forall([], tv('Bool')),
    False: Forall([], tv('Bool')),

    pair: Forall([['a', kType], ['b', kType]], tfun(tv('a'), tv('b'), tapp(tv('Pair'), tv('a'), tv('b')))),
    fst: Forall([['a', kType], ['b', kType]], tfun(tapp(tv('Pair'), tv('a'), tv('b')), tv('a'))),
    snd: Forall([['a', kType], ['b', kType]], tfun(tapp(tv('Pair'), tv('a'), tv('b')), tv('b'))),
    
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

    uncurry: Forall([['a', kType], ['b', kType], ['c', kType], ['e', kEffs]], tfun(tfun(tv('a'), TFun(tv('b'), tv('e'), tv('c'))), TFun(tapp(tv('Pair'), tv('a'), tv('b')), tv('e'), tv('c')))),
    curry: Forall([['a', kType], ['b', kType], ['c', kType], ['e', kEffs]], tfun(TFun(tapp(tv('Pair'), tv('a'), tv('b')), tv('e'), tv('c')), tfun(tv('a'), TFun(tv('b'), tv('e'), tv('c'))))),
  },
);
// const expr = app(v('fix'), abs(['rec', 'f'], app(v('caseList'), v('Nil'), abs(['h', 't'], app(v('Cons'), app(v('f'), v('h')), app(Pure(app(v('rec'), v('f'))), v('t')))))));
/*const expr = app(v('curry'), app(v('fix'), abs(['rec', 'p'],
  app(
    v('caseList'),
    v('Nil'),
    abs(['h', 't'], app(v('Cons'), app(app(v('fst'), v('p')), v('h')), app(v('rec'), app(v('pair'), app(v('fst'), v('p')), v('t'))))),
    app(v('snd'), v('p')),
  )
)));*/
const expr = lets([['a', v('True')], ['b', abs(['x'], v('x'))]], abs(['u'], app(v('pair'), v('a'), v('b'))));
console.log(`${showExpr(expr)}`);
let time = Date.now();
const res = throwEither(inferGen(ctx, expr, true));
time = Date.now() - time;
console.log(`${prettyForall(res)}`);
console.log(`${time}ms`);
