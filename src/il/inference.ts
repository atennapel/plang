import TC, { error, log, apply, findVar, freshName, withElems, updateCtx, freshNames, pop, findTMeta, replace } from './TC';
import Type, { teffsempty, tvar, tforall, isTForall, tmeta, tfun, tforalls, isTFun, isTMeta } from './types';
import { Val, isVar, isAbs, isAbsT, vr, isAnno } from './values';
import { Comp, isReturn, isApp, isAppT, isLet } from './computations';
import Either from '../Either';
import Context from './context';
import NameRepSupply from '../NameSupply';
import { subsume } from './subsumption';
import { wfKind, wfType, checkKind } from './wf';
import Elem, { ctvar, cvar, ctmeta, CTMeta, cmarker, isCMarker, isCTMeta } from './elems';
import Kind, { kType, kEffs } from './kinds';
import NameRep, { name } from '../NameRep';
import { assocGet } from '../utils';
import { unify } from './unification';

type SynthResult = { type: Type, eff: Type };

const applySynthResult = (res: SynthResult): TC<SynthResult> =>
  apply(res.type).chain(type => apply(res.eff).map(eff => ({ type, eff })));

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
    .chain(ty => pop(isCMarker(m))
    .map(right => {
      const u = orderedUnsolved(right, ty);
      return tforalls(u, u.reduce((t, [n, _]) => t.substTMeta(n, tvar(n)), ty));
    }))))
    .chain(apply);

const synthVal = (expr: Val): TC<Type> =>
  log(`synthVal ${expr}`).chain(() => {
    if (isVar(expr)) return findVar(expr.name).map(e => e.type);
    if (isAbs(expr)) {
      const type = expr.type;
      if (type)
        return wfType(type)
          .chain(k => checkKind(kType, k, `abstraction argument ${expr}`))
          .then(generalize(
            freshNames([expr.name, expr.name])
            .chain(([x, b]) => updateCtx(Context.add(ctmeta(b, kcomp(kType)), cvar(x, type)))
            .then(checkty(expr.open(vr(x)), tmeta(b)))
            .map(() => tfun(type, tmeta(b))))))
      else
        return generalize(
          freshNames([expr.name, expr.name, expr.name])
          .chain(([x, a, b]) => updateCtx(Context.add<Elem>(ctmeta(a, kType), ctmeta(b, kcomp(kType)), cvar(x, tmeta(a))))
          .then(checkty(expr.open(vr(x)), tmeta(b)))
          .map(() => tfun(tmeta(a), tmeta(b)))));
    }
    if (isAbsT(expr))
      return wfKind(expr.kind)
        .then(freshName(expr.name)
        .chain(x => withElems([ctvar(x, expr.kind)], synthComp(expr.openTVar(tvar(x))))
        .map(ty => tforall(x, expr.kind, ty))));
    if (isAnno(expr))
      return wfType(expr.type)
        .chain(k => checkKind(kType, k, `annotation ${expr}`))
        .then(checkVal(expr.expr, expr.type)).map(() => expr.type);

    return error(`cannot synthVal ${expr}`);
  }).chain(apply);

const synthComp = (expr: Comp): TC<SynthResult> =>
  log(`synthComp ${expr}`).chain(() => {
    if (isReturn(expr))
      return synthVal(expr.val).map(type => ({ type, eff: teffsempty() }));

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

    // combine effects
    if (isLet(expr))
      return synthComp(expr.expr)
        .chain(({ type: ty, eff: ef }) => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty)], synthComp(expr.open(vr(x)))
        .chain(({ type: ty2, eff: ef2 }) => unify(ef, ef2))));

    return error(`cannot synthComp ${expr}`);
  }).chain(applySynthResult);

const checkVal = (expr: Val, type: Type): TC<void> =>
  log(`checkVal ${expr} : ${type}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkVal(expr, type.open(tvar(x)))));
    if (isTFun(type) && isAbs(expr) && !expr.type)
      return freshName(expr.name).chain(x => withElems([cvar(x, type.left)], checkComp(expr.open(vr(x)), type.right, type.eff)));
    return synthVal(expr)
      .chain(te => apply(te))
      .chain(te => apply(type)
      .chain(ta => subsume(te, ta)));
  });

const checkComp = (expr: Comp, type: Type, eff: Type): TC<void> =>
  log(`checkComp ${expr} : ${type}!${eff}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkComp(expr, type.open(tvar(x)), eff)));
    if (isLet(expr))
      return synthComp(expr.expr)
        .chain(({ type: ty, eff: ef }) => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty)], checkComp(expr.open(vr(x)), type, ef))));
    return synthComp(expr)
      .chain(({ type: ty, eff: ef }) => apply(ty)
      .chain(ty => apply(ef)
      .chain(ef => apply(type)
      .chain(tyE => apply(eff)
      .chain(efE => subsume(ty, tyE)
      .then(subsume(ef, efE)))))));
  });

const synthapp = (type: Type, expr: Val): TC<SynthResult> =>
  log(`synthapp ${type} @ ${expr}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name)
        .chain(x => updateCtx(Context.add(ctmeta(x, type.kind)))
        .then(synthapp(type.open(tmeta(x)), expr)));
    if (isTMeta(type))
      return findTMeta(type.name)
        .chain(e => freshNames([type.name, type.name, type.name])
        .chain(([a1, a2, a3]) => replace(isCTMeta(type.name), [
          ctmeta(a2, kType), ctmeta(a3, kEffs), ctmeta(a1, kType), e.solve(tfun(tmeta(a1), tmeta(a3), tmeta(a2)))
        ])
        .then(checkVal(expr, tmeta(a1))
        .map(() => ({ type: tmeta(a2), eff: tmeta(a3) })))));
    if (isTFun(type)) return checkVal(expr, type.left).map(() => ({ type: type.right, eff: type.eff }));
    return error(`cannot synthapp ${type} @ ${expr}`);
  }).chain(applySynthResult);

const synthgen = (expr: Comp): TC<Type> =>
  synthComp(expr).map(({ type }) => type);

export const infer = (ctx: Context, expr: Comp): Either<string, Type> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
