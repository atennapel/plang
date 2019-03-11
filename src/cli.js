const { showExpr } = require('./exprs');
const { prettyType, TCon, TVar, TApp, makeTRowExtend, TFun, tRowEmpty, tRec } = require('./types');
const { kType, kRow } = require('./kinds');
const { parse } = require('./parser');
const { compile } = require('./compiler');
const { infer } = require('./inference');

const tv = TVar(0, kType);
const tr = TVar(1, kRow);

const tBool = TCon('Bool', kType);

const env = {
  getX: TFun(TApp(tRec, makeTRowExtend('x', tv, tr)), tv),
  objX: TApp(tRec, makeTRowExtend('x', tBool, tRowEmpty)),
  objY: TApp(tRec, makeTRowExtend('y', tBool, tRowEmpty)),
  objXY: TApp(tRec, makeTRowExtend('y', tBool, makeTRowExtend('x', tBool, tRowEmpty))),
};

try {
  const script = 'getX objXY';
  console.log(script);
  const expr = parse(script);
  console.log(showExpr(expr));
  const ty = infer(env, expr);
  console.log(prettyType(ty));
  const comp = compile(expr);
  console.log(comp);
  const res = eval(comp);
  console.log(`${res}`);
  console.log(res);
} catch (err) {
  console.log(`${err}`);
  //console.log(err);
}
