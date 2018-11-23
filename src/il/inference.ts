import TC, { error, log, apply } from './TC';
import Type, { teffsempty } from './types';
import { Val } from './values';
import { Comp, isReturn } from './computations';
import Either from '../Either';
import Context from './context';
import NameRepSupply from '../NameSupply';
import { subsume } from './subsumption';

type SynthResult = { type: Type, eff: Type };

const applySynthResult = (res: SynthResult): TC<SynthResult> =>
  apply(res.type).chain(type => apply(res.eff).map(eff => ({ type, eff })));

const synthVal = (expr: Val): TC<Type> =>
  log(`synthVal ${expr}`).chain(() => {
    return error(`cannot synthVal ${expr}`);
  }).chain(apply);

const synthComp = (expr: Comp): TC<SynthResult> =>
  log(`synthComp ${expr}`).chain(() => {
    if (isReturn(expr))
      return synthVal(expr.val).map(type => ({ type, eff: teffsempty() }));
    return error(`cannot synthComp ${expr}`);
  }).chain(applySynthResult);

const checkVal = (expr: Val, type: Type): TC<void> =>
  log(`checkVal ${expr} : ${type}`).chain(() => {
    return synthVal(expr)
      .chain(te => apply(te))
      .chain(te => apply(type)
      .chain(ta => subsume(te, ta)));
  });

const checkComp = (expr: Comp, type: Type, eff: Type): TC<void> =>
  log(`checkComp ${expr} : ${type}!${eff}`).chain(() => {
    return synthComp(expr)
      .chain(({ type: ty, eff: ef }) => apply(ty)
      .chain(ty => apply(ef)
      .chain(ef => apply(type)
      .chain(tyE => apply(eff)
      .chain(efE => subsume(ty, tyE)
      .then(subsume(ef, efE)))))));
  });

const synthapp = (type: Type, expr: Val): TC<SynthResult> =>
  log(`synthapp ${type} @ ${expr}`).chain(() => {
    return error(`cannot synthapp ${type} @ ${expr}`);
  }).chain(applySynthResult);

const synthgen = (expr: Comp): TC<Type> =>
  synthComp(expr).map(({ type }) => type);

export const infer = (ctx: Context, expr: Comp): Either<string, Type> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
