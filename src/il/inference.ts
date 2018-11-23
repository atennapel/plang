import TC, { error } from './TC';
import Type from './types';
import { Val } from './values';
import { Comp } from './computations';
import Either from '../Either';
import Context from './context';
import NameRepSupply from '../NameSupply';

type SynthResult = { type: Type, eff: Type };

const synthVal = (expr: Val): TC<Type> => error(`unimplemented`);

const synthComp = (expr: Comp): TC<SynthResult> => error(`unimplemented`);

const checkVal = (expr: Val, type: Type, eff: Type): TC<void> => error(`unimplemented`);

const checkComp = (expr: Comp, type: Type, eff: Type): TC<void> => error(`unimplemented`);

const synthappty = (type: Type, expr: Val): TC<SynthResult> => error(`unimplemented`);

const synthgen = (expr: Comp): TC<Type> =>
  synthComp(expr).map(({ type }) => type);

export const infer = (ctx: Context, expr: Comp): Either<string, Type> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
