const { showExpr } = require('./exprs');
const { parse } = require('./parser');

console.log(showExpr(parse('(\\x y z -> x (y z))')));
