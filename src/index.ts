import { Abs, Var, showTerm, PVar, Pat, PWildcard, Term, Ann, abs } from './terms';
import { infer } from './inference';
import { getInitialEnv } from './env';
import { showTy, TFun, TVar, TForall } from './types';
import { setConfig } from './config';
import { compile } from './compiler';

const tv = TVar;

const pv = PVar;
const _ = PWildcard;

const v = Var;

setConfig({
  debug: false,
  showKinds: false,
});

const env = getInitialEnv();
const term = Ann(abs([pv('x'), _], v('x')), TForall(['t1', 't2'], [], TFun(tv('t1'), TFun(tv('t2'), tv('t1')))));
try {
  console.log(showTerm(term));
  const ty = infer(env, term);
  console.log(showTy(ty));
  const comp = compile(term);
  console.log(comp);
  const ev = eval(comp);
  console.log(ev);
} catch (err) {
  console.log(() => `${err}`);
}

