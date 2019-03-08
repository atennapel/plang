const { showExpr } = require('./exprs');
const { showType } = require('./types');
const { parse } = require('./parser');
const { compile } = require('./compiler');
const { infer } = require('./inference');

const env = { };

try {
  const script = '\\x -> x';
  console.log(script);
  const expr = parse(script);
  console.log(showExpr(expr));
  const ty = infer(env, expr);
  console.log(showType(ty));
  const comp = compile(expr);
  console.log(comp);
  const res = eval(comp);
  console.log(`${res}`);
  console.log(res);
} catch (err) {
  console.log(`${err}`);
}
