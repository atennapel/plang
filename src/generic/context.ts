import Either from './Either';

export default class Context<E> {

  constructor(
    public readonly elems: E[],
  ) { }

  static empty<E>() {
    return new Context<E>([]);
  }
  static of<E>(...es: E[]) {
    return new Context(es);
  }
  static from<E>(es: E[]) {
    return new Context(es);
  }

  toString() {
    return `[${this.elems.join(', ')}]`;
  }

  find<R, T extends E = E>(fn: (elem: E) => elem is T, then: (val: T) => R, other: () => R): R {
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      if (fn(a[i])) return then(a[i] as any);
    }
    return other();
  }

  findAll<R>(fn: (elem: E) => R | null): R[] {
    const ret: R[] = [];
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      const c = fn(a[i]);
      if (c !== null) ret.push(c);
    }
    return ret;
  }

  add(...es: E[]) {
    return new Context(this.elems.concat(es));
  }
  addAll(es: E[]) {
    return new Context(this.elems.concat(es));
  }
  static addAll<E>(es: E[]): (ctx: Context<E>) => Context<E> {
    return (ctx: Context<E>) => ctx.addAll(es);
  }
  static add<E>(...es: E[]): (ctx: Context<E>) => Context<E> {
    return (ctx: Context<E>) => ctx.addAll(es);
  }

  append(that: Context<E>) {
    return new Context(this.elems.concat(that.elems));
  }
  static append<E>(that: Context<E>): (ctx: Context<E>) => Context<E> {
    return ctx => ctx.append(that);
  }

  split(fn: (elem: E) => boolean): [Context<E>, Context<E>] {
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      const c = a[i];
      if (fn(c)) {
        return [new Context(this.elems.slice(0, i)), new Context(this.elems.slice(i + 1))];
      }
    }
    return [this, Context.empty()];
  }

  replace(fn: (elem: E) => boolean, es: E[]) {
    const ret = this.split(fn);
    return ret[0].append(Context.from(es)).append(ret[1]);
  }

  ordered(a: (e: E) => boolean, b: (e: E) => boolean): boolean {
    const as = this.elems;
    for (let i = as.length - 1; i >= 0; i--) {
      if (a(as[i])) return false;
      if (b(as[i])) return true;
    }
    return true;
  }

}
