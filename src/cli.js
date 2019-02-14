const { showType } = require('./types');
const { parseDefs } = require('./parser');
const { compileDefs } = require('./compiler');
const { inferDefs } = require('./inference');

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  const ds = parseDefs(sc);
  const tyenv = inferDefs(ds);
  // console.log(showType(tyenv.main));
  const c = compileDefs(ds);
  console.log(c);
  process.exit();
}

const { run: _run } = require('./repl');
const _readline = require('readline').createInterface(process.stdin, process.stdout);
console.log('REPL');
process.stdin.setEncoding('utf8');
function _input() {
  _readline.question('> ', function(_i) {
    _run(_i, (s, e) => {
      console.log(s);
      setImmediate(_input, 0);
    });
  });
};
_input();
