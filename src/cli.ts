import * as readline from 'readline';
import run from './repl';

const rl = readline.createInterface(process.stdin, process.stdout);

console.log('REPL');
process.stdin.setEncoding('utf8');
function input() {
  rl.question('> ', function(i: string) {
    run(i, out => {
      console.log(out);
      setTimeout(input, 0);
    });
  });
};
input();
