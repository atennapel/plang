import TC, { error, log, apply, findVar, freshName, withElems, updateCtx, freshNames, pop, findTMeta, replace, ok, check } from './TC';
import Type, { teffsempty, tvar, tforall, isTForall, tmeta, tfun, tforalls, isTFun, isTMeta, TFun, TForall, tcomp, TComp, isTComp, tpure } from './types';
import Val, { isVar, isAbs, isAbsT, vr, isAnno } from './values';
import Comp, { isReturn, isApp, isAppT, isLet } from './computations';
import Either from '../Either';
import Context from './context';
import NameRepSupply from '../NameSupply';
import { subsume } from './subsumption';
import { wfKind, wfType, checkKind } from './wf';
import Elem, { ctvar, cvar, ctmeta, CTMeta, cmarker, isCMarker, isCTMeta } from './elems';
import Kind, { kType, kEffs, kComp } from './kinds';
import NameRep, { name } from '../NameRep';
import { assocGet } from '../utils';
import { unify, openEffs, closeEffs } from './unification';
import { isTEffsEmpty } from '../backup/types';

const orderedUnsolved = (ctx: Context, type: Type): [NameRep, Kind][] => {
  const u = ctx.findAll(e => e instanceof CTMeta && !e.type ? [e.name, e.kind] as [NameRep, Kind] : null);
  const r: [NameRep, Kind][] = [];
  const es = type.freeTMeta();
  for(let i = 0; i < es.length; i++) {
    const n = es[i];
    const k = assocGet(u, n);
    if(k && !assocGet(r, n))
      r.push([n, k]);
  }
  return r;
};

const generalize = (action: TC<Type>): TC<Type> =>
  freshName(name('m'))
    .chain(m => updateCtx(Context.add(cmarker(m)))
    .then(action
    .chain(apply)
    .chain(ty => log(`gen: ${ty}`).map(() => ty)
    .chain(ty => pop(isCMarker(m))
    .map(right => {
      const u = orderedUnsolved(right, ty);
      return tforalls(u, u.reduce((t, [n, _]) =>
        t.substTMeta(n, tvar(n)),isTComp(ty) ? ty : tcomp(ty, teffsempty())));
    }))))
    .chain(apply)
    //.map(closeTFun)
    .chain(ty => log(`gen done: ${ty}`).map(() => ty)));

const synthVal = (expr: Val): TC<Type> =>
  log(`synthVal ${expr}`).chain(() => {
    if (isVar(expr)) return findVar(expr.name).map(e => e.type);

    if (isAbs(expr)) {
      const type = expr.type;
      if (type)
        return wfType(type)
          .chain(k => checkKind(kType, k, `abstraction argument ${expr}`))
          .then(generalize(
            freshNames([expr.name, name('t'), name('e')])
            .chain(([x, b, e]) => updateCtx(Context.add(ctmeta(b, kType), ctmeta(e, kEffs), cvar(x, type)))
            .then(checkComp(expr.open(vr(x)), tcomp(tmeta(b), tmeta(e))))
            .map(() => tfun(type, tcomp(tmeta(b), tmeta(e)))))))
      else
        return generalize(
          freshNames([expr.name, expr.name, name('t'), name('e')])
          .chain(([x, a, b, e]) => updateCtx(Context.add(ctmeta(a, kType), ctmeta(b, kType), ctmeta(e, kEffs), cvar(x, tmeta(a))))
          .then(checkComp(expr.open(vr(x)), tcomp(tmeta(b), tmeta(e))))
          .map(() => tfun(tmeta(a), tcomp(tmeta(b), tmeta(e))))));
    }

    if (isAbsT(expr))
      return wfKind(expr.kind)
        .then(freshName(expr.name)
        .chain(x => withElems([ctvar(x, expr.kind)], synthComp(expr.openTVar(tvar(x))))
        .map(type => tforall(x, expr.kind, type))));

    if (isAnno(expr))
      return wfType(expr.type)
        .chain(k => checkKind(kType, k, `annotation ${expr}`))
        .then(checkVal(expr.expr, expr.type)).map(() => expr.type);

    return error(`cannot synthVal ${expr}`);
  })
  .chain(apply)
  .chain(ty => wfType(ty)
  .chain(k => checkKind(kType, k, `synthVal end ${expr} : ${ty}`)
  .chain(() => log(`synthVal done ${expr} : ${ty}`).map(() => ty))));

const synthComp = (expr: Comp): TC<Type> =>
  log(`synthComp ${expr}`).chain(() => {
    if (isReturn(expr))
      return synthVal(expr.val).map(type => tcomp(type, teffsempty()));

    if (isApp(expr))
      return synthVal(expr.left)
        .chain(ty => apply(ty))
        .chain(ty => synthapp(ty, expr.right));

    if (isAppT(expr))
      return wfType(expr.right)
        .chain(ka => synthVal(expr.left)
        .checkIs(isTForall, ty => `not a forall in left side of ${expr}: got ${ty}`)
        .chain(ty => checkKind(ty.kind, ka, `${expr}`)
        .map(() => ty.open(expr.right))));

    if (isLet(expr))
      return synthComp(expr.expr)
        .checkIs(isTComp, ty => `expected computation type but got ${ty} in left side of ${expr}`)
        .chain(ty => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty.type)], synthComp(expr.open(vr(x)))
        .checkIs(isTComp, ty2 => `expected computation type but got ${ty2} in right side of ${expr}`)
        .chain(ty2 => openEffs(ty2.eff)
        .chain(ef2open => openEffs(ty.eff).chain2(unify, TC.of(ef2open))
        .then(apply(ef2open).chain(closeEffs)
        .map(ef2open => tcomp(ty2.type, ef2open))))))));

    return error(`cannot synthComp ${expr}`);
  })
  .chain(apply)
  .chain(ty => wfType(ty)
  .chain(k => checkKind(kComp, k, `synthComp end ${expr} : ${ty}`)
  .chain(() => log(`synthComp done ${expr} : ${ty}`)
  .map(() => ty))));

const checkVal = (expr: Val, type: Type): TC<void> =>
  log(`checkVal ${expr} : ${type}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkVal(expr, type.open(tvar(x)))));
    if (isTFun(type) && isAbs(expr) && !expr.type)
      return freshName(expr.name).chain(x => withElems([cvar(x, type.left)], checkComp(expr.open(vr(x)), type.right)));
    return synthVal(expr)
      .chain(te => apply(te))
      .chain(te => apply(type)
      .chain(ta => subsume(te, ta)));
  });

const checkComp = (expr: Comp, type: Type): TC<void> =>
  log(`checkComp ${expr} : ${type}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkComp(expr, type.open(tvar(x)))));
    if (isReturn(expr))
      return TC.of(type).checkIs(isTComp, ty => `expected computation type but got ${ty} in checking ${expr}`)
        .chain(ty => checkVal(expr.val, ty.type));
    if (isLet(expr))
      return synthComp(expr.expr)
        .checkIs(isTComp, ty => `expected computation type but got ${ty} in left side of checking ${expr}`)
        .chain(ty => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty.type)], checkComp(expr.open(vr(x)), type))));
    return synthComp(expr)
      .chain(ty => apply(ty)
      .chain(ty => apply(type)
      .chain(type => subsume(ty, type))));
  });

const synthapp = (type: Type, expr: Val): TC<Type> =>
  log(`synthapp ${type} @ ${expr}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name)
        .chain(x => updateCtx(Context.add(ctmeta(x, type.kind)))
        .then(synthapp(type.open(tmeta(x)), expr)));
    if (isTMeta(type))
      return findTMeta(type.name)
        .chain(e => freshNames([type.name, type.name])
        .chain(([a1, a2]) => replace(isCTMeta(type.name), [
          ctmeta(a2, kComp), ctmeta(a1, kType), e.solve(tfun(tmeta(a1), tmeta(a2)))
        ])
        .then(checkVal(expr, tmeta(a1))
        .map(() => tmeta(a2)))));
    if (isTFun(type)) return checkVal(expr, type.left).map(() => type.right);
    return error(`cannot synthapp ${type} @ ${expr}`);
  })
  .chain(apply)
  .chain(ty => wfType(ty)
  .chain(k => checkKind(kComp, k, `synthapp end ${type} @ ${expr}`)
  .chain(() => log(`synthapp done ${type} @ ${expr}`)
  .map(() => ty))));

export const synthgenVal = (expr: Val): TC<Type> =>
  generalize(synthVal(expr))
    .chain(apply)
    .chain(ty => wfType(ty)
    .chain(k => checkKind(kType, k, `synthgenVal of ${expr} : ${ty}`)
    .map(() => ty)));

export const synthgenComp = (expr: Comp): TC<Type> =>
  generalize(synthComp(expr))
    .map(ty => isTComp(ty) ? ty : tpure(ty))
    .chain(apply)
    .chain(ty => wfType(ty)
    .chain(k => checkKind(kComp, k, `synthgenComp of ${expr} : ${ty}`)
    .map(() => ty)));

export const inferVal = (ctx: Context, expr: Val): Either<string, Type> =>
  synthgenVal(expr).run(ctx, new NameRepSupply(0)).val;
