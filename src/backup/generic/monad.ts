import Either, { Right, Left } from './Either';
import { INameSupply } from './NameSupply';
import Context from './context';

export default class TCM<C, S, E, T> {

  constructor(
    public readonly run: (ctx: C, supply: S) =>
      { ctx: C, supply: S, val: Either<E, T> },
  ) { }

  static of<C, S, E, T>(val: T) {
    return new TCM<C, S, E, T>((ctx, supply) => ({ ctx, supply, val: Either.of(val) }));
  }
  static void<C, S, E>(): TCM<C, S, E, void> {
    return TCM.of(undefined);
  }
  static error<C, S, E, T>(err: E) {
    return new TCM<C, S, E, T>((ctx, supply) => ({ ctx, supply, val: Either.err(err) }));
  }

  map<R>(fn: (val: T) => R): TCM<C, S, E, R> {
    return new TCM<C, S, E, R>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      return { ctx: ret.ctx, supply: ret.supply, val: ret.val.map(fn) };
    });
  }

  chain<R>(fn: (val: T) => TCM<C, S, E, R>): TCM<C, S, E, R> {
    return new TCM<C, S, E, R>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      if (ret.val.isError()) return ret as any;
      return fn((ret.val as Right<E, T>).val).run(ret.ctx, ret.supply);
    });
  }

  ap<R>(fn: TCM<C, S, E, (val: T) => R>) {
    return this.chain(val => fn.map(fn => fn(val)));
  }

  then<R>(that: TCM<C, S, E, R>) {
    return this.chain(() => that);
  }

  void(): TCM<C, S, E, void> {
    return this.map(() => undefined);
  }

  catch(fn: (err: E) => TCM<C, S, E, T>) {
    return new TCM<C, S, E, T>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      if (ret.val.isError()) return fn((ret.val as Left<E, T>).error).run(ctx, supply);
      return ret;
    });
  }

  static if<C, S, E, T>(c: TCM<C, S, E, boolean>, a: TCM<C, S, E, T>, b: TCM<C, S, E, T>): TCM<C, S, E, T> {
    return c.chain(cb => cb ? a : b);
  }

  static check<C, S, E>(c: boolean, msg: E): TCM<C, S, E, void> {
    return c ? TCM.void() : TCM.error(msg);
  }

  checkIs<R extends T>(fn: (val: T) => val is R, msg: (val: T) => E): TCM<C, S, E, R> {
    return this.chain(x => TCM.check<C, S, E>(fn(x), msg(x)).map(() => x as R));
  }

  fail(msg: E): TCM<C, S, E, never> {
    return this.chain(() => TCM.error<C, S, E, never>(msg));
  }

  // context
  static getCtx<C, S, E>(): TCM<C, S, E, C> {
    return new TCM((ctx, supply) => ({ ctx, supply, val: Either.of(ctx) }));
  }
  static getSupply<C, S, E>(): TCM<C, S, E, S> {
    return new TCM((ctx, supply) => ({ ctx, supply, val: Either.of(supply) }));
  }
  static putCtx<C, S, E>(ctx: C): TCM<C, S, E, void> {
    return new TCM((_, supply) => ({ ctx, supply, val: Either.of(undefined) }));
  }
  static putSupply<C, S, E>(supply: S): TCM<C, S, E, void> {
    return new TCM((ctx, _) => ({ ctx, supply, val: Either.of(undefined) }));
  }

  static updateCtx<C, S, E>(fn: (ctx: C) => C): TCM<C, S, E, void> {
    return TCM.getCtx<C, S, E>().chain(ctx => TCM.putCtx(fn(ctx)));
  }
  static updateSupply<C, S, E>(fn: (supply: S) => S): TCM<C, S, E, void> {
    return TCM.getSupply<C, S, E>().chain(supply => TCM.putSupply<C, S, E>(fn(supply)));
  }

  // fresh
  static freshName<T, N, C, S extends INameSupply<T, N, S>, E>(val: T): TCM<C, S, E, N> {
    return TCM.getSupply<C, S, E>().chain(sup => {
      const { name, supply } = sup.fresh(val);
      return TCM.putSupply<C, S, E>(supply).map(() => name);
    });
  }
  static freshNames<T, N, C, S extends INameSupply<T, N, S>, E>(ns: T[]): TCM<C, S, E, N[]> {
    return ns.reduce((c, n) => c.chain(a => TCM.freshName<T, N, C, S, E>(n).map(x => a.concat([x]))), TCM.of<C, S, E, N[]>([] as N[]));
  }

  // context
  static pop<El, S, E>(fn: (elem: El) => boolean): TCM<Context<El>, S, E, Context<El>> {
    return TCM.getCtx<Context<El>, S, E>().chain(ctx => {
      const [left, right] = ctx.split(fn);
      return TCM.putCtx<Context<El>, S, E>(left).map(() => right);
    });
  }

  static replace<El, S, E>(fn: (elem: El) => boolean, es: El[]): TCM<Context<El>, S, E, void> {
    return TCM.updateCtx(ctx => ctx.replace(fn, es));
  }

}
