import { ExprN, TypeN, TC, Ctx, log, apply, findVar, freshName, withElems, error, updateCtx, findTMeta, freshNames, replace, ElemN, pop, KindN } from './TC';
import Either from './generic/Either';
import NameRepSupply from './generic/NameSupply';
import { isVar, isAbs, isApp, isAnno, vr } from './exprs';
import { impossible } from './utils';
import { wfType, checkKind } from './wf';
import { kType } from './initial';
import { isTForall, isTFun, tvar, isTMeta, tmeta, tfun, tforalls } from './types';
import { subtype } from './subtype';
import { ctvar, cvar, ctmeta, isCTMeta, isCMarker, cmarker, CTMeta } from './elems';
import Context from './generic/context';
import NameRep, { name } from './generic/NameRep';

const assocGet = <A extends { equals: (other: A) => boolean }, B>(arr: [A, B][], val: A): B | null => {
  for (let i = arr.length - 1; i >= 0; i--) {
    if (arr[i][0].equals(val)) return arr[i][1];
  }
  return null;
}

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
    }))));

const synthty = (expr: ExprN): TC<TypeN> =>
  log(`synth ${expr}`).chain(() => {
    if (isVar(expr)) return findVar(expr.name).map(e => e.type);
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
    if (isApp(expr)) return synthty(expr.left).chain(ty => apply(ty)).chain(ty => synthappty(ty, expr.right));
    if (isAnno(expr))
      return wfType(expr.type)
        .chain(k => checkKind(kType, k, `annotation ${expr}`))
        .then(checkty(expr.expr, expr.type)).map(() => expr.type);
    return impossible();
  });

const checkty = (expr: ExprN, type: TypeN): TC<void> =>
  log(`check ${expr} : ${type}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkty(expr, type.open(tvar(x)))));
    if (isTFun(type))
      return !isAbs(expr) || expr.type ? checksynthty(expr, type) :
        freshName(expr.name).chain(x => withElems([cvar(x, type.left)], checkty(expr.open(vr(x)), type.right)));
    return checksynthty(expr, type);
  });
const checksynthty = (expr: ExprN, type: TypeN): TC<void> =>
  log(`checksynth ${expr} : ${type}`).then(
    synthty(expr)
      .chain(te => apply(te))
      .chain(te => apply(type)
      .chain(ta => subtype(te, ta))));

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
    if (isTFun(type)) return checkty(expr, type.left).map(() => type.right);
    return error(`cannot synthapp ${type} @ ${expr}`);
  });

const synthgen = (expr: ExprN): TC<TypeN> => generalize(synthty(expr));

export const infer = (ctx: Ctx, expr: ExprN): Either<string, TypeN> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
