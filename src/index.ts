import { Abs, Var, showTerm, PVar, Pat, PWildcard, Term, Ann } from './terms';
import { infer } from './inference';
import { initialEnv } from './env';
import { showTy, TFun, TVar, TForall } from './types';

const tv = TVar;

const pv = PVar;
const _ = PWildcard;

const v = Var;
const abs = (ns: Pat[], body: Term) =>
  ns.reduceRight((x, y) => Abs(y, x), body);

const env = initialEnv;
const term = Ann(abs([pv('x'), _], v('x')), TForall(['t1', 't2'], [], TFun(tv('t1'), TFun(tv('t2'), tv('t1')))));
try {
  console.log(showTerm(term));
  const ty = infer(env, term);
  console.log(showTy(ty));
} catch (err) {
  console.log(`${err}`);
}

