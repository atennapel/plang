import { inferVal, synthgenVal, synthgenComp } from "./il/inference";
import { ctvar, cvar } from "./il/elems";
import { kType, kEff } from "./il/kinds";
import initialContext from "./il/initial";
import Type, { tvar, teffsextend, teffs, tfun, tfuns } from "./il/types";
import { name } from "./NameRep";
import { teffsempty } from "./backup/types";
import { abs, lt, app, vr, apps, abss } from "./surface/exprs";
import { exprToValIL, exprToCompIL } from "./surface/surface";
import { log } from "./il/TC";
import NameRepSupply from "./NameSupply";
import { Left, Right } from "./Either";
import compileToJS from "./il/javascriptBackend";
import Expr from "./il/exprs";
import Val from "./il/values";
import Comp from "./il/computations";
import parse from "./surface/parser";

/*
TODO:
  - think about rewriting with a TComp type added
  - tapp in instL, instR and instUnify
  - infer computation (how to generalize?)
  - handlers types/syntax/typechecking
  - row polymorphism (records/variants)
  - pretty printing kinds/types/expressions
BUGS:
  - list map
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
const Flip = name('Flip');
const flip = name('flip');
const add = name('add');

const ctx = initialContext.add(
  //ctvar(Void, kType),

  ctvar(Flip, kEff),

  ctvar(Unit, kType),
  cvar(Unit, tv(Unit)),

  ctvar(Bool, kType),
  //cvar(True, tv(Bool)),
  //cvar(False, tv(Bool)),

  //ctvar(List, kfun(kType, kType)),
  //cvar(singleton, tforall(t, kType, tfun(tv(t), tapp(tvar(List), tv(t))))),

  //cvar(test, tforall(t, kType, tfun(tapp(tv(List), tv(t)), tv(t)))),

  //cvar(recX, tapp(tRec, trowextend(y, tvar(Unit), trowextend(x, tvar(Bool), trowempty())))),

  cvar(flip, tfun(tv(Unit), teffs(tv(Flip)), tv(Bool))),

  cvar(add, tfuns(tv(Unit), tv(Unit), tv(Unit))),
);

const expr = '[ :x (flip ()) x ]';
console.log('' + expr);
const p = parse(expr);
console.log(''+p);
const prog = exprToCompIL(p)
  .chain(v => log(`${v}`)
  .then(synthgenComp(v)
  .map(({ type, eff }) => ({ expr: v, type, eff }))));
const res = prog.run(ctx, new NameRepSupply(0)).val;
if (res.isError()) {
  console.log(''+(res as Left<string, { expr: Comp, type: Type, eff: Type }>).error);
} else {
  const x = (res as Right<string, { expr: Comp, type: Type, eff: Type }>).val;
  console.log(`${x.type}!${x.eff}`);
  const comp = compileToJS(x.expr);
  console.log(''+comp);
  try {
    const r = eval(comp);
    console.log(r);
  } catch(err) {
    console.log(''+err);
  }
}
