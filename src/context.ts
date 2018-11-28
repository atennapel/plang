import Elem from './elems';

export default class Context {

  constructor(
    public readonly elems: Elem[],
  ) { }

  static empty() {
    return new Context([]);
  }
  static of(...es: Elem[]) {
    return new Context(es);
  }
  static from(es: Elem[]) {
    return new Context(es);
  }

  toString() {
    return `[${this.elems.join(', ')}]`;
  }

  find<R, T extends Elem>(fn: (elem: Elem) => elem is T, then: (val: T) => R, other: () => R): R {
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      if (fn(a[i])) return then(a[i] as any);
    }
    return other();
  }

  findAll<R>(fn: (elem: Elem) => R | null): R[] {
    const ret: R[] = [];
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      const c = fn(a[i]);
      if (c !== null) ret.push(c);
    }
    return ret;
  }

  add(...es: Elem[]) {
    return new Context(this.elems.concat(es));
  }
  addAll(es: Elem[]) {
    return new Context(this.elems.concat(es));
  }
  static addAll(es: Elem[]): (ctx: Context) => Context {
    return (ctx: Context) => ctx.addAll(es);
  }
  static add(...es: Elem[]): (ctx: Context) => Context {
    return (ctx: Context) => ctx.addAll(es);
  }

  append(that: Context) {
    return new Context(this.elems.concat(that.elems));
  }
  static append(that: Context): (ctx: Context) => Context {
    return ctx => ctx.append(that);
  }

  split(fn: (elem: Elem) => boolean): [Context, Context] {
    const a = this.elems;
    for (let i = a.length - 1; i >= 0; i--) {
      const c = a[i];
      if (fn(c)) {
        return [new Context(this.elems.slice(0, i)), new Context(this.elems.slice(i + 1))];
      }
    }
    return [this, Context.empty()];
  }

  replace(fn: (elem: Elem) => boolean, es: Elem[]) {
    const ret = this.split(fn);
    return ret[0].append(Context.from(es)).append(ret[1]);
  }

  ordered(a: (e: Elem) => boolean, b: (e: Elem) => boolean): boolean {
    const as = this.elems;
    for (let i = as.length - 1; i >= 0; i--) {
      if (a(as[i])) return false;
      if (b(as[i])) return true;
    }
    return true;
  }

}
