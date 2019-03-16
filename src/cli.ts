import { parseTerm } from './parser';
import { infer } from './inference';
import { compile } from './compiler';

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  const ds = parseTerm(sc);
  infer(ds);
  const c = compile(ds);
  console.log(c);
  process.exit();
}

import { run } from './repl';
const _readline = require('readline').createInterface(process.stdin, process.stdout);
console.log('REPL');
process.stdin.setEncoding('utf8');
function _input() {
  _readline.question('> ', function(_i: string) {
    run(_i, (s: string, e?: boolean) => {
      console.log(s);
      setImmediate(_input, 0);
    });
  });
};
_input();
