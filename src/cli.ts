import { run, init } from './repl';
import { parseDefs } from './parser';
import { inferDefs } from './inference';
import { getInitialEnv, Env } from './env';
import { reduceDefs } from './compilerMachine';
import { GEnv, LNil, makeClos, MApp, MAbs, MBVar } from './machine';
import { showReifyClos } from './reification';
import { tforall, tfunFrom, TVar } from './types';
import { kType } from './kinds';

if (process.argv[2]) {
  const sc = require('fs').readFileSync(process.argv[2], 'utf8');
  parseDefs(sc, {}).then(ds => {
    const tenv: Env = getInitialEnv();
    tenv.global.unsafeFix = tforall([['t', kType]], tfunFrom([tfunFrom([TVar('t'), TVar('t')]), TVar('t')]));
    inferDefs(tenv, ds);

    const _part = MAbs(MApp(MBVar(1), MAbs(MApp(MApp(MBVar(1), MBVar(1)), MBVar(0)))));
    const _ycomb = MAbs(MApp(_part, _part));
    const _yval = makeClos(_ycomb, LNil);
    const env: GEnv = { unsafeFix: _yval };
    reduceDefs(env, ds);

    if (tenv.global.main && env.main) {
      console.log(showReifyClos(env.main, tenv.global.main, env));
    }
    process.exit();
  }).catch(err => {
    console.error(err);
    process.exit();
  });
} else {
  const _readline = require('readline').createInterface(process.stdin, process.stdout);
  console.log('REPL');
  process.stdin.setEncoding('utf8');
  function _input() {
    _readline.question('> ', function(_i: string) {
      run(_i, (s: string, e?: boolean) => {
        console.log(s);
        setImmediate(_input, 0);
      },
      (s: string) => console.log(s),
      (cb: (s: string) => void) => _readline.question('prompt: ', cb));
    });
  };
  init();
  _input();
}
