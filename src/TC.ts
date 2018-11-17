import NameRep, { name } from './generic/NameRep';
import Elem, { cmarker, isCMarker, isCTMeta, isCKVar, CKVar, isCTVar, isCVar, CTVar, CTMeta, CMarker, CVar } from './elems';
import Context from './generic/context';
import NameRepSupply from './generic/NameSupply';
import TCM from './generic/monad';
import Type, { tfun, tforall, isTVar, isTFun, isTMeta, isTForall, isTApp, tapp } from './types';
import { impossible } from './utils';
import Expr from './exprs';
import Kind from './kinds';

export type ElemN = Elem<NameRep>;
export type KindN = Kind<NameRep>;
export type TypeN = Type<NameRep>;
export type ExprN = Expr<NameRep>;
export type Ctx = Context<ElemN>;

export type TC<T> = TCM<Context<ElemN>, NameRepSupply, string, T>;

export const pure = <T>(val: T): TC<T> => TCM.of(val);
export const error = <T>(err: string): TC<T> => TCM.error(err);
export const ok = pure(undefined);

export const log = (msg: any): TC<void> => getCtx.map(ctx => { console.log(`${msg} in ${ctx}`); return undefined });

export const iff = <T>(c: TC<boolean>, a: TC<T>, b: TC<T>): TC<T> => TCM.if(c, a, b);
export const check = (c: boolean, msg: string): TC<void> => TCM.check(c, msg);

export const getCtx = TCM.getCtx() as TC<Context<Elem<NameRep>>>;
export const updateCtx = (fn: (ctx: Ctx) => Ctx) => TCM.updateCtx(fn) as TC<void>;

export const freshName = (val: NameRep): TC<NameRep> => TCM.freshName(val);
export const freshNames = (ns: NameRep[]): TC<NameRep[]> => TCM.freshNames(ns);

export const pop = (fn: (elem: Elem<NameRep>) => boolean): TC<Context<Elem<NameRep>>> => TCM.pop(fn);
export const replace = (fn: (elem: Elem<NameRep>) => boolean, es: Elem<NameRep>[]): TC<void> => TCM.replace(fn, es);

export const withElems = <T>(es: Elem<NameRep>[], action: TC<T>): TC<T> =>
  freshName(name('wm'))
    .chain(name => TCM.updateCtx<Context<Elem<NameRep>>, NameRepSupply, string>(Context.addAll([cmarker(name) as Elem<NameRep>].concat(es)))
    .then(action)
    .chain(val => pop(isCMarker(name))
    .map(() => val)));

export const freshWithElems = <T>(val: NameRep[], es: (ns: NameRep[]) => Elem<NameRep>[], action: (ns: NameRep[]) => TC<T>): TC<T> =>
  freshNames(val).chain(ns => withElems(es(ns), action(ns)));

export const ordered = (a: NameRep, b: NameRep) =>
  getCtx.map(ctx => ctx.ordered(isCTMeta(a), isCTMeta(b)));

export const apply = (type: Type<NameRep>): TC<Type<NameRep>> => {
  if (isTVar(type)) return pure(type);
  if (isTMeta(type))
    return getCtx.chain(ctx => ctx.find(
      isCTMeta(type.name),
      e => e.type ? apply(e.type): pure<Type<NameRep>>(type),
      () => pure<Type<NameRep>>(type)
    ));
  if (isTFun(type))
    return apply(type.left)
      .chain(left => apply(type.right)
      .map(right => tfun(left, right)));
  if (isTApp(type))
    return apply(type.left)
      .chain(left => apply(type.right)
      .map(right => tapp(left, right)));
  if (isTForall(type))
    return apply(type.type).map(body => tforall(type.name, type.kind, body));
  return impossible();
}

export const find = <R, T extends ElemN>(fn: (e: ElemN) => e is T, then: (val: T) => TC<R>, other: () => TC<R>): TC<R> =>
  getCtx.chain(ctx => ctx.find(fn, then, other));
export const findKVar = (name: NameRep): TC<CKVar<NameRep>> => find(isCKVar(name), e => pure(e), () => error(`kvar ${name} not found`));
export const findTVar = (name: NameRep): TC<CTVar<NameRep>> => find(isCTVar(name), e => pure(e), () => error(`tvar ${name} not found`));
export const findTMeta = (name: NameRep): TC<CTMeta<NameRep>> => find(isCTMeta(name), e => pure(e), () => error(`tmeta ${name} not found`));
export const findMarker = (name: NameRep): TC<CMarker<NameRep>> => find(isCMarker(name), e => pure(e), () => error(`marker ${name} not found`));
export const findVar = (name: NameRep): TC<CVar<NameRep>> => find(isCVar(name), e => pure(e), () => error(`var ${name} not found`));
