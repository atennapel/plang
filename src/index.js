const { showType } = require('./types');
const { parseType, tokenize } = require('./parser');

const s = '-> a';
const ts = tokenize(s);
ts.unshift('('); ts.push(')');
const t = parseType(ts.reverse());
console.log(showType(t));
