import { Maybe, caseMaybe } from "./maybe";

export type Either<E, T> = Left<E> | Right<T>;

export interface Left<E> { readonly tag: 'Left', readonly val: E };
export const Left = <E, T>(val: E): Either<E, T> => ({ tag: 'Left', val });

export interface Right<T> { readonly tag: 'Right', readonly val: T };
export const Right = <E, T>(val: T): Either<E, T> => ({ tag: 'Right', val });

export const isLeft = <E, T>(val: Either<E, T>): val is Left<E> => val.tag === 'Left';
export const isRight = <E, T>(val: Either<E, T>): val is Right<T> => val.tag === 'Right';

export type CasesEither<E, T, R> = { Left: (val: E) => R, Right: (val: T) => R };
export const caseOf = <E, T, R>(val: Either<E, T>, cs: CasesEither<E, T, R>): R =>
  val.tag === 'Left' ? cs.Left(val.val) : cs.Right(val.val);

export const showEither = <E, T>(val: Either<E, T>, showVal: (val: T) => string = x => `${x}`, showErr: (val: E) => string = x => `${x}`) =>
  caseOf(val, {
    Left: val => `Left(${showErr(val)})`,
    Right: val => `Right(${showVal(val)})`,
  });

export const mapEither = <E, A, B>(val: Either<E, A>, fn: (val: A) => B): Either<E, B> =>
  caseOf(val, {
    Left: () => val as any,
    Right: val => Right(fn(val)),
  });

export const throwEither = <E, T>(val: Either<E, T>): T =>
  caseOf(val, {
    Left: val => { throw val },
    Right: val => val,
  });

export const fromMaybe = <E, T>(val: Maybe<T>, err: () => E): Either<E, T> =>
  caseMaybe(val, {
    Just: val => Right<E, T>(val),
    Nothing: () => Left<E, T>(err()),
  });
