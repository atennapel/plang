import { run } from './repl';
import { parse } from './parser';
import { infer } from './inference';
import { initialEnv } from './env';
import { compile } from './compiler';

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  const ds = parse(sc);
  const ty = infer(initialEnv, ds);
  const c = compile(ds);
  console.log(c);
  process.exit();
}

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
