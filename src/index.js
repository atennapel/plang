const { showExpr } = require('./exprs');
const { showType, TCon, TFun, TVar } = require('./types');
const { parseDefs } = require('./parser');
const { compileDefs } = require('./compiler');
const { inferDefs } = require('./inference');

const tenv = {
  Id: {
    tcon: TCon('Id'),
    tvs: [],
    etvs: [],
    utvs: [0],
    type: TFun(TVar(0), TVar(0)),
  },
};
const env = {
};

const sc = `
  main = (\\Id id -> id) (Id \\x -> x)
`;
const ds = parseDefs(sc);
const tyenv = inferDefs(ds, tenv, env);
console.log(showType(tyenv.main));
const c = compileDefs(ds);
console.log(c);
console.log(eval(`() => {${c}; return main}`)()(x => x + 1)(0));
