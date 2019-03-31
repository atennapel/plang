// immutable singly-linked list
export default abstract class List<T> {

  static from<T>(a: T[]): List<T> {
    let c: List<T> = Nil.new();
    for (let i = a.length - 1; i >= 0; i--)
      c = new Cons<T>(a[i], c);
    return c;
  }
  static of<T>(...a: T[]): List<T> { return List.from(a) }

  static nil<T>(): List<T> {
    return Nil.new();
  }
  static cons<T>(head: T, tail: List<T>): List<T> {
    return new Cons(head, tail);
  }

  isEmpty(): boolean {
    return this instanceof Nil;
  }

  take(amount: number = -1): T[] {
    let c: List<T> = this;
    const r: T[] = [];
    while (c instanceof Cons) {
      if (amount >= 0 && r.length >= amount) break;
      r.push(c._head);
      c = c._tail;
    }
    return r;
  }

  abstract case<R>(fnil: () => R, fcons: (head: T, tail: List<T>) => R): R;

  each(fn: (val: T) => void): void {
    let l: List<T> = this;
    while (l instanceof Cons) {
      fn(l._head);
      l = l._tail;
    }
  }

  first(fn: (val: T) => boolean): T | null {
    let l: List<T> = this;
    while (l instanceof Cons) {
      const h = l._head;
      if (fn(h)) return h;
      l = l._tail;
    }
    return null;
  }

  foldl<R>(fcons: (acc: R, head: T) => R, fnil: R): R {
    let l: List<T> = this;
    let c: R = fnil;
    while (l instanceof Cons) {
      c = fcons(c, l._head);
      l = l._tail;
    }
    return c;
  }
  
  abstract foldr<R>(fcons: (head: T, acc: R) => R, fnil: R): R;

  abstract map<R>(fn: (val: T) => R): List<R>;
  abstract filter(fn: (val: T) => boolean): List<T>;
  abstract mapMaybe<R>(fn: (val: T) => R | null): List<R>;

  abstract append(other: List<T>): List<T>;
  abstract flatMap<R>(fn: (val: T) => List<R>): List<R>;

  abstract zip<R>(other: List<R>): List<[T, R]>;

}

export class Nil<T> extends List<T> {

  private constructor() { super() }

  private static readonly _nil: Nil<any> = new Nil<any>();
  static new<T>(): Nil<T> { return Nil._nil as Nil<T> }

  case<R>(fnil: () => R, fcons: (head: T, tail: List<T>) => R): R {
    return fnil();
  }

  foldr<R>(fcons: (head: T, acc: R) => R, fnil: R): R {
    return fnil;
  }

  map<R>(fn: (val: T) => R): List<R> {
    return this as any as List<R>;
  }
  filter(fn: (val: T) => boolean): List<T> {
    return this;
  }
  mapMaybe<R>(fn: (val: T) => R | null): List<R> {
    return this as any as List<R>;
  }

  append(other: List<T>): List<T> {
    return other;
  }
  flatMap<R>(fn: (val: T) => List<R>): List<R> {
    return this as any as List<R>;
  }

  zip<R>(other: List<R>): List<[T, R]> {
    return this as any as List<[T, R]>;
  }

}

export class Cons<T> extends List<T> {

  constructor(
    public readonly _head: T,
    public readonly _tail: List<T>,
  ) { super() }

  static new<T>(head: T, tail: List<T>): Cons<T> {
    return new Cons(head, tail);
  }

  case<R>(fnil: () => R, fcons: (head: T, tail: List<T>) => R): R {
    return fcons(this._head, this._tail);
  }

  foldr<R>(fcons: (head: T, acc: R) => R, fnil: R): R {
    return fcons(this._head, this._tail.foldr(fcons, fnil));
  }

  map<R>(fn: (val: T) => R): List<R> {
    return new Cons(fn(this._head), this._tail.map(fn));
  }
  filter(fn: (val: T) => boolean): List<T> {
    return fn(this._head) ? new Cons(this._head, this._tail.filter(fn)) : this._tail.filter(fn);
  }
  mapMaybe<R>(fn: (val: T) => R | null): List<R> {
    const x = fn(this._head);
    return x === null ? this._tail.mapMaybe(fn) : new Cons(x, this._tail.mapMaybe(fn));
  }

  append(other: List<T>): List<T> {
    return new Cons(this._head, this._tail.append(other));
  }
  flatMap<R>(fn: (val: T) => List<R>): List<R> {
    return fn(this._head).append(this._tail.flatMap(fn));
  }

  zip<R>(other: List<R>): List<[T, R]> {
    return other.case(
      () => other as any as List<[T, R]>,
      (h, t) =>
        new Cons([this._head, h] as [T, R], this._tail.zip(t)),
    );
  }

}
