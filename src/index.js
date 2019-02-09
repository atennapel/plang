const { showExpr } = require('./exprs');
const { showType } = require('./types');
const { parseDefs } = require('./parser');
const { compileDefs } = require('./compiler');
const { inferDefs } = require('./inference');

const sc = `
  Box = 0 -> 0

  id = \\x. x
  const = \\x y. x
  flip = \\f x y. f y x
  compose = \\f g x. f (g x)
  dup = \\f x. f x x
  z = \\f x. x
  s = \\n f x. f (n f x)
  main = s (s (s z))
`;
const ds = parseDefs(sc);
const env = inferDefs(ds);
console.log(showType(env.main));
const c = compileDefs(ds);
console.log(c);
console.log(eval(`() => {${c}; return main}`)()(x => x + 1)(0));
