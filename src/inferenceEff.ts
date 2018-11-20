import { ExprN, TypeN, TC, Ctx, log, apply, findVar, freshName, withElems, error, updateCtx, findTMeta, freshNames, replace, ElemN, pop, KindN, check, pure } from './TC';
import Either from './generic/Either';
import NameRepSupply from './generic/NameSupply';
import { isVar, isAbs, isApp, isAnno, vr, isAbsT, isAppT, isSelect, isInject, isRestrict, isExtendRec, isExtendVar, isEmptyRecord, isCaseVar, isLet } from './exprs';
import { impossible, assocGet } from './utils';
import { wfType, checkKind, wfKind } from './wf';
import { kType, matchTFun, tfun, kRow, tRec, tVar, tfuns, matchTFunE } from './initial';
import { isTForall, tvar, isTMeta, tmeta, tforalls, tforall, trowextend, tapp, trowempty } from './types';
import { subtype } from './subtype';
import { ctvar, cvar, ctmeta, isCTMeta, isCMarker, cmarker, CTMeta } from './elems';
import Context from './generic/context';
import NameRep, { name } from './generic/NameRep';

const pureEff = (type: TypeN) => pure(({ type, eff: trowempty() }));
const applyEff = (t: { type: TypeN, eff: TypeN }) =>
  apply(t.type)
  .chain(type => apply(t.eff)
  .map(eff => ({ type, eff })));

const orderedUnsolved = (ctx: Ctx, type: TypeN): [NameRep, KindN][] => {
  const u = ctx.findAll(e => e instanceof CTMeta && !e.type ? [e.name, e.kind] as [NameRep, KindN] : null);
  const r: [NameRep, KindN][] = [];
  const es = type.freeTMeta();
  for(let i = 0; i < es.length; i++) {
    const n = es[i];
    const k = assocGet(u, n);
    if(k && !assocGet(r, n))
      r.push([n, k]);
  }
  return r;
};

const generalize = (action: TC<TypeN>): TC<TypeN> =>
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

const synthtyEff = (expr: ExprN): TC<{ type: TypeN, eff: TypeN }> =>
  log(`synth ${expr}`).chain(() => {
    if (isVar(expr)) return findVar(expr.name).chain(e => pureEff(e.type));
    if (isAbs(expr)) {
      const type = expr.type;
      if (type)
        return wfType(type)
          .chain(k => checkKind(kType, k, `abstraction argument ${expr}`))
          .then(generalize(
            freshNames([expr.name, expr.name])
            .chain(([x, b]) => updateCtx(Context.add<ElemN>(ctmeta(b, kType), cvar(x, type)))
            .then(checkty(expr.open(vr(x)), tmeta(b)))
            .map(() => tfun(type, tmeta(b))))))
      else
        return generalize(
          freshNames([expr.name, expr.name, expr.name])
          .chain(([x, a, b]) => updateCtx(Context.add<ElemN>(ctmeta(a, kType), ctmeta(b, kType), cvar(x, tmeta(a))))
          .then(checkty(expr.open(vr(x)), tmeta(b)))
          .map(() => tfun(tmeta(a), tmeta(b)))));
    }
    if (isAbsT(expr))
      return wfKind(expr.kind)
        .then(freshName(expr.name)
        .chain(x => withElems([ctvar(x, expr.kind)], synthty(expr.openTVar(tvar(x))))
        .map(ty => tforall(x, expr.kind, ty))));
    if (isApp(expr))
      return synthty(expr.left)
        .chain(ty => apply(ty))
        .chain(ty => synthappty(ty, expr.right));
    if (isAppT(expr))
      return wfType(expr.right)
        .chain(ka => synthty(expr.left)
        .checkIs(isTForall, ty => `not a forall in left side of ${expr}: got ${ty}`)
        .chain(ty => checkKind(ty.kind, ka, `${expr}`)
        .map(() => ty.open(expr.right))));
    if (isAnno(expr))
      return wfType(expr.type)
        .chain(k => checkKind(kType, k, `annotation ${expr}`))
        .then(checkty(expr.expr, expr.type)).map(() => expr.type);

    if (isLet(expr))
      return synthty(expr.expr)
        .chain(ty => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty)], synthty(expr.open(vr(x))))));

    if (isSelect(expr)) {
      const t = name('t');
      const r = name('r');
      return pureEff(tforalls([[t, kType], [r, kRow]], tfun(tapp(tRec, trowextend(expr.label, tvar(t), tvar(r))), tvar(t))));
    }
    if (isRestrict(expr)) {
      const t = name('t');
      const r = name('r');
      return pureEff(tforalls([[t, kType], [r, kRow]], tfun(tapp(tRec, trowextend(expr.label, tvar(t), tvar(r))), tvar(r))));
    }
    if (isExtendRec(expr)) {
      const t = name('t');
      const r = name('r');
      return pureEff(tforalls([[t, kType], [r, kRow]], tfuns(tvar(t), tapp(tRec, tvar(r)), tapp(tRec, trowextend(expr.label, tvar(t), tvar(r))))));
    }

    if (isInject(expr)) {
      const t = name('t');
      const r = name('r');
      return pureEff(tforalls([[t, kType], [r, kRow]], tfun(tvar(t), tapp(tVar, trowextend(expr.label, tvar(t), tvar(r))))));
    }
    if (isExtendVar(expr)) {
      const t = name('t');
      const r = name('r');
      return pureEff(tforalls([[t, kType], [r, kRow]], tfuns(tapp(tVar, tvar(r)), tapp(tVar, trowextend(expr.label, tvar(t), tvar(r))))));
    }
    if (isCaseVar(expr)) {
      const a = name('a');
      const b = name('b');
      const r = name('r');
      return pureEff(tforalls([[a, kType], [b, kType], [r, kRow]], tfuns(
        tfun(tvar(a), tvar(b)),
        tfun(tapp(tVar, tvar(r)), tvar(b)),
        tapp(tVar, trowextend(expr.label, tvar(a), tvar(r))),
        tvar(b),
      )));
    }

    if (isEmptyRecord(expr))
      return pureEff(tapp(tRec, trowempty()));

    return impossible();
  }).chain(applyEff);

const checktyEff = (expr: ExprN, type: TypeN, eff: TypeN): TC<void> =>
  log(`check ${expr} : ${type} ! ${eff}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checktyEff(expr, type.open(tvar(x)), eff)));
    const fun = matchTFunE(type);
    if (fun && isAbs(expr) && !expr.type)
      return freshName(expr.name).chain(x => withElems([cvar(x, fun.left)], checktyEff(expr.open(vr(x)), fun.right, fun.eff)));
    if (isLet(expr))
      return synthtyEff(expr.expr)
        .chain(({ type: ty, eff }) => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty)], checktyEff(expr.open(vr(x)), type, eff))));
    return synthtyEff(expr)
      .chain(({ type: te, eff: ef }) => apply(te)
      .chain(te => apply(type)
      .chain(ta => subtype(te, ta)
      .then(apply(ef)
      .chain(ef => subtype(ef, eff))))));
  });

const synthappty = (type: TypeN, expr: ExprN): TC<TypeN> =>
  log(`synthapp ${type} @ ${expr}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name)
        .chain(x => updateCtx(Context.add(ctmeta(x, type.kind)))
        .then(synthappty(type.open(tmeta(x)), expr)));
    if (isTMeta(type))
      return findTMeta(type.name)
        .chain(e => freshNames([type.name, type.name])
        .chain(([a1, a2]) => replace(isCTMeta(type.name), [
          ctmeta(a2, kType), ctmeta(a1, kType), e.solve(tfun(tmeta(a1), tmeta(a2)))
        ])
        .then(checkty(expr, tmeta(a1))
        .map(() => tmeta(a2)))));
    const fun = matchTFun(type);
    if (fun) return checkty(expr, fun.left).map(() => fun.right);
    return error(`cannot synthapp ${type} @ ${expr}`);
  }).chain(apply);

const synthgen = (expr: ExprN): TC<{ type: TypeN, eff: TypeN }> =>
  generalize(synthtyEff(expr))
    .chain(t => wfType(t.type)
    .chain(_ => wfType(t.eff)
    .map(_ => t)));

export const infer = (ctx: Ctx, expr: ExprN): Either<string, TypeN> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
