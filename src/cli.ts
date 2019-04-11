import { run, init } from './repl';
import { parseDefs } from './parser';
import { inferDefs } from './inference';
import { getInitialEnv } from './env';
import { compileDefs } from './compiler';

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  parseDefs(sc, {}).then(ds => {
    inferDefs(getInitialEnv(), ds);
    const c = compileDefs(ds, x => `const ${x}`);
    console.log(c);
    process.exit();
  }).catch(err => {
    console.error(err);
    process.exit();
  });
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
init();
_input();
