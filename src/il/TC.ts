import Either, { Right, Left } from '../Either';
import NameSupply from '../NameSupply';
import Context from './context';
import NameRep, { name } from '../NameRep';
import Elem, { cmarker, isCMarker, isCTMeta, CKVar, CTVar, CTMeta, CVar, CMarker, isCVar, isCKVar, isCTVar } from './elems';
import Type, { isTVar, isTMeta, isTApp, isTForall, tforall, tapp, isTEffsExtend, isTEffsEmpty, teffsextend, isTFun, tfun } from './types';
import { impossible } from '../utils';

export default class TC<T> {

  constructor(
    public readonly run: (ctx: Context, supply: NameSupply) =>
      { ctx: Context, supply: NameSupply, val: Either<string, T> },
  ) { }

  static of<T>(val: T) {
    return new TC((ctx, supply) => ({ ctx, supply, val: Either.of(val) }));
  }
  static void(): TC<void> {
    return TC.of(undefined);
  }
  static error<T>(err: string) {
    return new TC<T>((ctx, supply) => ({ ctx, supply, val: Either.err(err) }));
  }

  map<R>(fn: (val: T) => R): TC<R> {
    return new TC<R>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      return { ctx: ret.ctx, supply: ret.supply, val: ret.val.map(fn) };
    });
  }

  chain<R>(fn: (val: T) => TC<R>): TC<R> {
    return new TC<R>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      if (ret.val.isError()) return ret as any;
      return fn((ret.val as Right<string, T>).val).run(ret.ctx, ret.supply);
    });
  }
  
  chain2<A, B>(fn: (a: T, b: A) => TC<B>, that: TC<A>): TC<B> {
    return this.chain(a => that.chain(b => fn(a, b)));
  }
  chain3<A, B, C>(fn: (a: T, b: A, c: B) => TC<C>, that: TC<A>, that2: TC<B>): TC<C> {
    return this.chain(a => that.chain(b => that2.chain(c => fn(a, b, c))));
  }

  ap<R>(fn: TC<(val: T) => R>) {
    return this.chain(val => fn.map(fn => fn(val)));
  }

  then<R>(that: TC<R>) {
    return this.chain(() => that);
  }

  void(): TC<void> {
    return this.map(() => undefined);
  }

  catch(fn: (err: string) => TC<T>) {
    return new TC<T>((ctx, supply) => {
      const ret = this.run(ctx, supply);
      if (ret.val.isError()) return fn((ret.val as Left<string, T>).error).run(ctx, supply);
      return ret;
    });
  }

  static if<T>(c: TC<boolean>, a: TC<T>, b: TC<T>): TC<T> {
    return c.chain(cb => cb ? a : b);
  }

  static check(c: boolean, msg: string): TC<void> {
    return c ? TC.void() : TC.error(msg);
  }

  checkIs<R extends T>(fn: (val: T) => val is R, msg: (val: T) => string): TC<R> {
    return this.chain(x => TC.check(fn(x), msg(x)).map(() => x as R));
  }

  fail(msg: string): TC<never> {
    return this.chain(() => TC.error(msg));
  }

  // context
  static getContext(): TC<Context> {
    return new TC((ctx, supply) => ({ ctx, supply, val: Either.of(ctx) }));
  }
  static getNameSupply(): TC<NameSupply> {
    return new TC((ctx, supply) => ({ ctx, supply, val: Either.of(supply) }));
  }
  static putContext(ctx: Context): TC<void> {
    return new TC((_, supply) => ({ ctx, supply, val: Either.of(undefined) }));
  }
  static putNameSupply(supply: NameSupply): TC<void> {
    return new TC((ctx, _) => ({ ctx, supply, val: Either.of(undefined) }));
  }

  static updateContext(fn: (ctx: Context) => Context): TC<void> {
    return TC.getContext().chain(ctx => TC.putContext(fn(ctx)));
  }
  static updateNameSupply(fn: (supply: NameSupply) => NameSupply): TC<void> {
    return TC.getNameSupply().chain(supply => TC.putNameSupply(fn(supply)));
  }

  // fresh
  static freshName(val: NameRep): TC<NameRep> {
    return TC.getNameSupply().chain(sup => {
      const { name, supply } = sup.fresh(val);
      return TC.putNameSupply(supply).map(() => name);
    });
  }
  static freshNames(ns: NameRep[]): TC<NameRep[]> {
    return ns.reduce((c, n) => c.chain(a => TC.freshName(n).map(x => a.concat([x]))), TC.of([] as NameRep[]));
  }

  // context
  static pop(fn: (elem: Elem) => boolean): TC<Context> {
    return TC.getContext().chain(ctx => {
      const [left, right] = ctx.split(fn);
      return TC.putContext(left).map(() => right);
    });
  }

  static replace(fn: (elem: Elem) => boolean, es: Elem[]): TC<void> {
    return TC.updateContext(ctx => ctx.replace(fn, es));
  }

}

export const pure = <T>(val: T): TC<T> => TC.of(val);
export const error = <T>(err: string): TC<T> => TC.error(err);
export const ok = pure(undefined);

export const log = (msg: any): TC<void> => getCtx.map(ctx => { console.log(`${msg} in ${ctx}`); return undefined });

export const iff = <T>(c: TC<boolean>, a: TC<T>, b: TC<T>): TC<T> => TC.if(c, a, b);
export const check = (c: boolean, msg: string): TC<void> => TC.check(c, msg);

export const getCtx = TC.getContext();
export const updateCtx = TC.updateContext;

export const freshName = (val: NameRep): TC<NameRep> => TC.freshName(val);
export const freshNames = (ns: NameRep[]): TC<NameRep[]> => TC.freshNames(ns);

export const pop = (fn: (elem: Elem) => boolean): TC<Context> => TC.pop(fn);
export const replace = (fn: (elem: Elem) => boolean, es: Elem[]): TC<void> => TC.replace(fn, es);

export const withElems = <T>(es: Elem[], action: TC<T>): TC<T> =>
  freshName(name('wm'))
    .chain(name => updateCtx(Context.addAll([cmarker(name) as Elem].concat(es)))
    .then(action)
    .chain(val => pop(isCMarker(name))
    .map(() => val)));

export const freshWithElems = <T>(val: NameRep[], es: (ns: NameRep[]) => Elem[], action: (ns: NameRep[]) => TC<T>): TC<T> =>
  freshNames(val).chain(ns => withElems(es(ns), action(ns)));

export const ordered = (a: NameRep, b: NameRep) =>
  getCtx.map(ctx => ctx.ordered(isCTMeta(a), isCTMeta(b)));

export const find = <R, T extends Elem>(fn: (e: Elem) => e is T, then: (val: T) => TC<R>, other: () => TC<R>): TC<R> =>
  getCtx.chain(ctx => ctx.find(fn, then, other));
export const findKVar = (name: NameRep): TC<CKVar> => find(isCKVar(name), e => pure(e), () => error(`kvar ${name} not found`));
export const findTVar = (name: NameRep): TC<CTVar> => find(isCTVar(name), e => pure(e), () => error(`tvar ${name} not found`));
export const findTMeta = (name: NameRep): TC<CTMeta> => find(isCTMeta(name), e => pure(e), () => error(`tmeta ${name} not found`));
export const findMarker = (name: NameRep): TC<CMarker> => find(isCMarker(name), e => pure(e), () => error(`marker ${name} not found`));
export const findVar = (name: NameRep): TC<CVar> => find(isCVar(name), e => pure(e), () => error(`var ${name} not found`));

export const apply = (type: Type): TC<Type> => {
  if (isTVar(type)) return pure(type);
  if (isTMeta(type))
    return getCtx.chain(ctx => ctx.find(
      isCTMeta(type.name),
      e => e.type ? apply(e.type): pure<Type>(type),
      () => pure(type)
    ));
  if (isTApp(type))
    return apply(type.left)
      .chain(left => apply(type.right)
      .map(right => tapp(left, right)));
  if (isTFun(type))
    return apply(type.left)
      .chain(left => apply(type.eff)
      .chain(eff => apply(type.right)
      .map(right => tfun(left, eff, right))));
  if (isTForall(type))
    return apply(type.type).map(body => tforall(type.name, type.kind, body));
  if (isTEffsExtend(type))
    return apply(type.type)
      .chain(left => apply(type.rest)
      .map(right => teffsextend(left, right)));
  if (isTEffsEmpty(type)) return pure(type);
  return impossible();
}
