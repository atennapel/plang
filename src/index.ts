import { infer } from "./il/inference";
import { abs, vr } from "./il/values";
import { ret } from "./il/computations";
import { ctvar, cvar } from "./il/elems";
import { kType } from "./il/kinds";
import initialContext from "./il/initial";
import { tvar } from "./il/types";
import { name } from "./NameRep";

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

const ctx = initialContext.add(
  //ctvar(Void, kType),

  ctvar(Unit, kType),
  cvar(Unit, tv(Unit)),

  //ctvar(Bool, kType),
  //cvar(True, tv(Bool)),
  //cvar(False, tv(Bool)),

  //ctvar(List, kfun(kType, kType)),
  //cvar(singleton, tforall(t, kType, tfun(tv(t), tapp(tvar(List), tv(t))))),

  //cvar(test, tforall(t, kType, tfun(tapp(tv(List), tv(t)), tv(t)))),

  //cvar(recX, tapp(tRec, trowextend(y, tvar(Unit), trowextend(x, tvar(Bool), trowempty())))),
);

const expr = ret(abs(x, ret(vr(x))));
console.log('' + expr);
console.log('' + infer(ctx, expr));
