export type Maybe<T> = Nothing | Just<T>;

export interface Nothing { readonly tag: 'Nothing' };
const _Nothing = { tag: 'Nothing' };
export const Nothing = <T>(): Maybe<T> => _Nothing as any;

export interface Just<T> { readonly tag: 'Just', readonly val: T };
export const Just = <T>(val: T): Maybe<T> => ({ tag: 'Just', val });

export const isNothing = <T>(val: Maybe<T>): val is Nothing => val.tag === 'Nothing';
export const isJust = <T>(val: Maybe<T>): val is Just<T> => val.tag === 'Just';

export type CasesMaybe<T, R> = { Just: (val: T) => R, Nothing: () => R };
export const caseMaybe = <T, R>(val: Maybe<T>, cs: CasesMaybe<T, R>): R =>
  val.tag === 'Just' ? cs.Just(val.val) : cs.Nothing();

export const showMaybe = <T>(val: Maybe<T>, showVal: (val: T) => string = x => `${x}`) =>
  caseMaybe(val, {
    Just: val => `Just(${showVal(val)})`,
    Nothing: () => 'Nothing',
  });

export const mapMaybe = <A, B>(val: Maybe<A>, fn: (val: A) => B): Maybe<B> =>
  caseMaybe(val, {
    Just: val => Just(fn(val)),
    Nothing: Nothing as () => Maybe<B>,
  });

export const throwMaybe = <T>(val: Maybe<T>): T =>
  caseMaybe(val, {
    Just: val => val,
    Nothing: () => { throw new Error('throw_ called on Nothing') },
  });
