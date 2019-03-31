import { Abs, Var, showTerm } from './terms';
import { infer } from './inference';
import { initialEnv } from './env';
import { showTy } from './types';

const env = initialEnv;
const term = Abs('x', Var('x'));
try {
  console.log(showTerm(term));
  const ty = infer(env, term);
  console.log(showTy(ty));
} catch (err) {
  console.log(`${err}`);
}

