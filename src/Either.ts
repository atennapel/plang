export default abstract class Either<E, T> {

  abstract toString(): string;

  abstract isError(): boolean;

  static err<E, T>(err: E) {
    return new Left<E, T>(err);
  }
  static of<E, T>(val: T) {
    return new Right<E, T>(val);
  }

  abstract map<R>(fn: (val: T) => R): Either<E, R>;
  abstract chain<R>(fn: (val: T) => Either<E, R>): Either<E, R>;

}

export class Left<E, T> extends Either<E, T> {

  constructor(
    public readonly error: E,
  ) { super() }

  toString() {
    return `Left(${this.error})`;
  }

  isError(): this is Left<E, T> {
    return true;
  }

  map<R>(fn: (val: T) => R): Either<E, R> {
    return this as any;
  }
  chain<R>(fn: (val: T) => Either<E, R>): Either<E, R> {
    return this as any;
  }

}

export class Right<E, T> extends Either<E, T> {

  constructor(
    public readonly val: T,
  ) { super() }

  toString() {
    return `Right(${this.val})`;
  }

  isError(): this is Left<E, T> {
    return false;
  }

  map<R>(fn: (val: T) => R): Either<E, R> {
    return new Right<E, R>(fn(this.val));
  }
  chain<R>(fn: (val: T) => Either<E, R>): Either<E, R> {
    return fn(this.val);
  }

} 
