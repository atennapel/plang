import { log, ok, TypeN, TC } from './TC';

export const unify = (a: TypeN, b: TypeN): TC<void> =>
  log(`subtype ${a} <: ${b}`).fail(`unimplemented`);
