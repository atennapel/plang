import { inferVal } from "./il/inference";
import { abs, vr } from "./il/values";
import { ret, app, lt } from "./il/computations";
import { ctvar, cvar } from "./il/elems";
import { kType, kEff } from "./il/kinds";
import initialContext from "./il/initial";
import { tvar, teffsextend, teffs, tfun } from "./il/types";
import { name } from "./NameRep";
import { teffsempty } from "./backup/types";

/*
TODO:
  - open and close effs at the right time (maybe not in unification/subsumption)
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
);

const expr = abs(x, lt(y, app(vr(flip), vr(Unit)), app(vr(flip2), vr(Unit))));
console.log('' + expr);
console.log('' + inferVal(ctx, expr));
