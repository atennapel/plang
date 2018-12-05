export type Maybe<T> = Nothing | Just<T>;

export interface Nothing { tag: 'Nothing' };
export const Nothing: Maybe<any> = { tag: 'Nothing' };

export interface Just<T> { tag: 'Just', val: T };
export const Just = <T>(val: T): Maybe<T> => ({ tag: 'Just', val });

export type CasesMaybe<T, R> = { Just: (val: T) => R, Nothing: () => R };
export const caseOf = <T, R>(val: Maybe<T>, cs: CasesMaybe<T, R>): R =>
  val.tag === 'Just' ? cs.Just(val.val) : cs.Nothing();

export const show = <T>(val: Maybe<T>, showVal: (val: T) => string = x => `${x}`) =>
  caseOf(val, {
    Just: val => `Just(${showVal(val)})`,
    Nothing: () => 'Nothing',
  });

export const map = <A, B>(val: Maybe<A>, fn: (val: A) => B): Maybe<B> =>
  caseOf(val, {
    Just: val => Just(fn(val)),
    Nothing: () => Nothing,
  });

export const throw_ = <T>(val: Maybe<T>): T =>
  caseOf(val, {
    Just: val => val,
    Nothing: () => { throw new Error('throw_ called on Nothing') },
  });
