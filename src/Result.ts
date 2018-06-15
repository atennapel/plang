export abstract class Result<E, T> {
  constructor() {}
  static ok<E, T>(t: T) { return new Ok<E, T>(t) }
  static err<E, T>(e: E) { return new Err<E, T>(e) }
  abstract map<R>(fn: (val: T) => R): Result<E, R>;
  abstract then<R>(fn: (val: T) => Result<E, R>): Result<E, R>;
	abstract catch(fn: (err: E) => Result<E, T>): Result<E, T>;
}

export class Ok<E, T> extends Result<E, T> {
  constructor(readonly val: T) { super() }
  toString() { return 'Ok(' + this.val + ')' }
  map<R>(fn: (val: T) => R): Result<E, R> { return Result.ok<E, R>(fn(this.val)) }
  then<R>(fn: (val: T) => Result<E, R>) { return fn(this.val) }
  catch(fn: (err: E) => Result<E, T>): Result<E, T> { return this }
  not<R>(fn: (val: T) => Result<E, R>): Result<E, R> { return fn(this.val) }
}
export function isOk<E, T>(x: Result<E, T>): x is Ok<E, T> { return x instanceof Ok }

export class Err<E, T> extends Result<E, T> {
  constructor(readonly err: E) { super() }
  toString() { return 'Err(' + this.err + ')' }
  map<R>(fn: (val: T) => R) { return Result.err<E, R>(this.err) }
  then<R>(fn: (val: T) => Result<E, R>) { return Result.err<E, R>(this.err) }
	catch(fn: (err: E) => Result<E, T>) { return fn(this.err) }
}
export function isErr<E, T>(x: Result<E, T>): x is Err<E, T> { return x instanceof Err }
