const { showType } = require('./types');
const { parseDefs } = require('./parser');
const { compileDefs } = require('./compiler');
const { inferDefs } = require('./inference');

const sc = require('fs').readFileSync(process.argv[2], 'utf8');
const ds = parseDefs(sc);
const tyenv = inferDefs(ds);
console.log(showType(tyenv.main));
const c = compileDefs(ds);
console.log(c);
console.log(eval(`() => {${c}; return main}`)()(x => x + 1)(0));
