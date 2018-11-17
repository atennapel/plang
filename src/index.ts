import { infer } from "./inference";
import { initialContext, kType } from "./initial";
import { abs, vr, abss, apps, anno, absty, absT, appT } from "./exprs";
import { name } from "./generic/NameRep";
import { ctvar, cvar } from "./elems";
import { tvar, tfun, tforall, tapp } from "./types";
import { kfun } from "./kinds";

/*
TODO:
- unification
- TApp in subtyping
- switch to unification in subtyping
- checkty, synthappty for TApp
- tfun as a higher-kinded type
- rows
- records and variants
*/

const tv = tvar;
const x = name('x');
const y = name('y');
const t = name('t');

const Void = name('Void');
const Unit = name('Unit');
const Bool = name('Bool');
const True = name('True');
const False = name('False');
const List = name('List');
const singleton = name('singleton');
const test = name('test');

const ctx = initialContext.add(
  ctvar(Void, kType),

  ctvar(Unit, kType),
  cvar(Unit, tv(Unit)),

  ctvar(Bool, kType),
  cvar(True, tv(Bool)),
  cvar(False, tv(Bool)),

  ctvar(List, kfun(kType, kType)),
  cvar(singleton, tforall(t, kType, tfun(tv(t), tapp(tvar(List), tv(t))))),

  cvar(test, tforall(t, kType, tfun(tapp(tv(List), tv(t)), tv(t)))),
);

const id = abs(x, vr(x));
console.log(
  '' + infer(ctx, apps(vr(test), apps(vr(singleton), vr(True)))),
);
