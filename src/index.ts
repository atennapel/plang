import { inferVal, synthgenVal } from "./il/inference";
import { ctvar, cvar } from "./il/elems";
import { kType, kEff } from "./il/kinds";
import initialContext from "./il/initial";
import { tvar, teffsextend, teffs, tfun, tfuns } from "./il/types";
import { name } from "./NameRep";
import { teffsempty } from "./backup/types";
import { abs, lt, app, vr, apps, abss } from "./surface/exprs";
import { exprToValIL } from "./surface/surface";
import { log } from "./il/TC";
import NameRepSupply from "./NameSupply";

/*
TODO:
  - find right position to open effs in inference
  - row polymorphism (records/variants)
  - infer computation (how to generalize?)
  - generalization in let?
*/

const tv = tvar;
const x = name('x');
const y = name('y');
const t = name('t');
const r = name('r');

const Void = name('Void');
const Unit = name('Unit');
const Bool = name('Bool');
const True = name('True');
const False = name('False');
const List = name('List');
const singleton = name('singleton');
const test = name('test');
const recX = name('recX');
const pure = name('pure');
const Flip = name('Flip');
const Flip2 = name('Flip2');
const flip = name('flip');
const flip2 = name('flip2');
const add = name('add');

const ctx = initialContext.add(
  //ctvar(Void, kType),

  ctvar(Flip, kEff),
  ctvar(Flip2, kEff),

  ctvar(Unit, kType),
  cvar(Unit, tv(Unit)),

  //ctvar(Bool, kType),
  //cvar(True, tv(Bool)),
  //cvar(False, tv(Bool)),

  //ctvar(List, kfun(kType, kType)),
  //cvar(singleton, tforall(t, kType, tfun(tv(t), tapp(tvar(List), tv(t))))),

  //cvar(test, tforall(t, kType, tfun(tapp(tv(List), tv(t)), tv(t)))),

  //cvar(recX, tapp(tRec, trowextend(y, tvar(Unit), trowextend(x, tvar(Bool), trowempty())))),

  cvar(flip, tfun(tv(Unit), teffs(tv(Flip)), tv(Unit))),
  cvar(flip2, tfun(tv(Unit), teffs(tv(Flip2)), tv(Unit))),

  cvar(add, tfuns(tv(Unit), tv(Unit), tv(Unit))),
);

const expr = abss(['x', 'y'], vr('x'));
console.log('' + expr);
const prog = exprToValIL(expr)
  .chain(v => log(`${v}`)
  .then(synthgenVal(v)));
console.log('' + prog.run(ctx, new NameRepSupply(0)).val);
