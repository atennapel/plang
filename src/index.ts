import { infer } from "./inference";
import { initialContext, kType } from "./initial";
import { abs, vr, abss, apps, anno, absty } from "./exprs";
import { name } from "./generic/NameRep";
import { ctvar, cvar } from "./elems";
import { tvar, tfun, tforall } from "./types";

const x = name('x');
const y = name('y');
const t = name('t');

const Void = name('Void');
const Unit = name('Unit');
const Bool = name('Bool');
const True = name('True');
const False = name('False');

const ctx = initialContext.add(
  ctvar(Void, kType),

  ctvar(Unit, kType),
  cvar(Unit, tvar(Unit)),

  ctvar(Bool, kType),
  cvar(True, tvar(Bool)),
  cvar(False, tvar(Bool)),
);

const id = abs(x, vr(x));
console.log(
  '' + infer(ctx, absty(x, tvar(Bool), vr(x))),
);
