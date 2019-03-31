import { run } from './repl';
import { parseDefs } from './parser';
import { inferDefs } from './inference';
import { initialEnv } from './env';
import { compileDefs } from './compiler';

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  const ds = parseDefs(sc);
  inferDefs(initialEnv, ds);
  const c = compileDefs(ds, x => `const ${x}`);
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
