const { parse } = require('./parser');
const { compile } = require('./compiler');
const { infer } = require('./inference');

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  const ds = parse(sc);
  infer({}, ds);
  const c = compile(ds);
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
