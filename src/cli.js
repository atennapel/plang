const { showExpr } = require('./exprs');
const { parse } = require('./parser');
const { compile } = require('./compiler');

try {
  const script = '(\\x -> x) \\x -> x';
  console.log(script);
  const expr = parse(script);
  console.log(showExpr(expr));
  const comp = compile(expr);
  console.log(comp);
  const res = eval(comp);
  console.log(`${res}`);
  console.log(res);
} catch (err) {
  console.log(`${err}`);
}
