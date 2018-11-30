import TC, { error, log, apply, findVar, freshName, withElems, updateCtx, freshNames, pop, findTMeta, replace, ok, check, findOp, findEff, findOps, sequence } from './TC';
import Type, { teffsempty, tvar, tforall, isTForall, tmeta, tfun, tforalls, isTFun, isTMeta, TFun, TForall, isTEffsEmpty, teffs, teffsFrom } from './types';
import Either from './Either';
import Context from './context';
import NameRepSupply from './NameSupply';
import { subsume } from './subsumption';
import { wfKind, wfType, checkKind } from './wf';
import Elem, { ctvar, cvar, ctmeta, CTMeta, cmarker, isCMarker, isCTMeta } from './elems';
import Kind, { kType, kEffs } from './kinds';
import NameRep, { name } from './NameRep';
import { assocGet, objClone, any, objMapToArr, all } from './utils';
import { unify, openEffs, closeEffs, closeTFun, unifyEffs } from './unification';
import Expr, { isVar, isAbs, isApp, isLet, isAnno, vr, isHandler, abs, HandlerCase, isHOp, isHReturn } from './exprs';
import { impossible } from './backup/utils';

type TypeEff = { type: Type, eff: Type };
const typeEff = (type: Type, eff: Type) => ({ type, eff });
const applyTypeEff = (t: TypeEff): TC<TypeEff> =>
  apply(t.type).chain(ty => apply(t.eff).map(eff => typeEff(ty, eff)));

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

const generalize = (action: TC<TypeEff>): TC<TypeEff> =>
  freshName(name('m'))
    .chain(m => updateCtx(Context.add(cmarker(m)))
    .then(action
    .chain(applyTypeEff)
    .chain(ty => log(`gen: ${ty.type}!${ty.eff}`)
    .map(() => ty)
    .chain(ty => pop(isCMarker(m))
    .map(right => {
      const u = orderedUnsolved(right, ty.type);
      return typeEff(tforalls(u, u.reduce((t, [n, _]) => t.substTMeta(n, tvar(n)), ty.type)), ty.eff);
    }))))
    .chain(applyTypeEff)
    .map(({ type, eff }) => ({ type: closeTFun(type), eff }))
    .chain(ty => log(`gen done: ${ty.type}!${ty.eff}`)
    .map(() => ty)));

type HandlerState = { [key: string]: NameRep[] };
const emptyHandlerState: HandlerState = {};
const addOp = (s: HandlerState, eff: NameRep, op: NameRep): HandlerState => {
  const n: HandlerState = objClone(s);
  if (!s[eff.toString()]) n[eff.toString()] = [];
  if (!any(n[eff.toString()], n => n.equals(op))) n[eff.toString()].push(op);
  return n;
};
const allEffs = (s: HandlerState) => Object.keys(s).map(name);
const showHandlerState = (s: HandlerState): string => {
  const r: string[] = [];
  for (let k in s) r.push(`${k}: [${s[k].join(', ')}]`);
  return `{${r.join(', ')}}`;
};
const synthHandler = (cs: HandlerCase, t1: Type, te: Type): TC<{ type: TypeEff, state: HandlerState }> =>
  log(`synthHandler ${cs} with ${t1}`).chain(() => {
    if (isHOp(cs))
      return findOp(cs.op)
        .chain(op => findEff(op.eff)
        .then(synthHandler(cs.rest, t1, te)
        .chain(({ type: t2, state }) => freshNames([cs.x, cs.k])
        .chain(([x, k]) => withElems([cvar(x, op.paramty), cvar(k, tfun(op.returnty, t2.type, t2.eff))],
          checkTy(cs.open(vr(x), vr(k)), t2))
        .map(() => ({ type: t2, state: addOp(state, op.eff, op.name) }))))));

    if (isHReturn(cs))
      return freshName(cs.x)
        .chain(x => withElems([cvar(x, t1)], synth(cs.open(vr(x)))))
        .map(type => ({ type, state: emptyHandlerState }));
    
    return impossible();
  })
  .chain(({ type, state }) => applyTypeEff(type)
  .map(type => ({ type, state })))
  .chain(ty => log(`synthHandler done ${t1} and ${te} : ${ty.type.type}!${ty.type.eff} with ${showHandlerState(ty.state)}`)
  .map(() => ty));

const getCompleteEffs = (s: HandlerState) =>
  sequence(objMapToArr(s, (ops, eff) => findOps(name(eff))
    .map(allops => ({ eff, complete: all(allops, o => any(ops, o2 => o.name.equals(o2))) }))))
    .map(a => a.filter(o => o.complete).map(o => name(o.eff)));
const getIncompleteEffs = (s: HandlerState) =>
  sequence(objMapToArr(s, (ops, eff) => findOps(name(eff))
    .map(allops => ({ eff, complete: all(allops, o => any(ops, o2 => o.name.equals(o2))) }))))
    .map(a => a.filter(o => !o.complete).map(o => name(o.eff)));

const synth = (expr: Expr): TC<TypeEff> =>
  log(`synth ${expr}`).chain(() => {
    if (isVar(expr)) return findVar(expr.name).map(e => typeEff(e.type, teffsempty()));

    if (isAbs(expr)) {
      const type = expr.type;
      if (type)
        return wfType(type)
          .chain(k => checkKind(kType, k, `abstraction argument ${expr}`))
          .then(generalize(
            freshNames([expr.name, name('t'), name('e')])
            .chain(([x, b, e]) => updateCtx(Context.add(ctmeta(b, kType), ctmeta(e, kEffs), cvar(x, type)))
            .then(checkTy(expr.open(vr(x)), typeEff(tmeta(b), tmeta(e))))
            .map(() => typeEff(tfun(type, tmeta(b), tmeta(e)), teffsempty())))));
      else
        return generalize(
          freshNames([expr.name, expr.name, name('t'), name('e')])
            .chain(([x, a, b, e]) => updateCtx(Context.add(ctmeta(a, kType), ctmeta(b, kType), ctmeta(e, kEffs), cvar(x, tmeta(a))))
            .then(checkTy(expr.open(vr(x)), typeEff(tmeta(b), tmeta(e))))
            .map(() => typeEff(tfun(tmeta(a), tmeta(b), tmeta(e)), teffsempty()))));
    }

    if (isApp(expr))
      return synth(expr.left)
        .chain(applyTypeEff)
        .chain(ty => synthapp(ty.type, expr.right)
        .chain(res => apply(ty.eff)
        .chain(eff => unifyEffs(eff, res.eff))
        .map(eff => typeEff(res.type, eff))));

    if (isLet(expr))
      return synth(expr.expr)
        .chain(ty => freshName(expr.name)
        .chain(x => withElems([cvar(x, ty.type)], synth(expr.open(vr(x)))
        .chain(ty2 => unifyEffs(ty2.eff, ty.eff)
        .map(ef2open => typeEff(ty2.type, ef2open))))));

    if (isAnno(expr))
      return wfType(expr.type)
        .chain(k => checkKind(kType, k, `annotation ${expr}`))
        .then(checktyOpen(expr.expr, expr.type));

    if (isHandler(expr))
      return generalize(freshNames([name('a'), name('e')])
        .chain(([a, e]) => updateCtx(Context.add(ctmeta(a, kType), ctmeta(e, kEffs)))
        .then(synthHandler(expr.cases, tmeta(a), tmeta(e))
        .chain(({ type: tb, state }) => getIncompleteEffs(state)
        .chain(compl => log(`${allEffs(state)} ; ${compl.join(' ')}`)
        .then(apply(tmeta(a))
        .map(ta => typeEff(
          tfun(
            tfun(tvar(name('Unit')), ta, teffsFrom(allEffs(state).map(tvar), tmeta(e))),
            tb.type,
            teffsFrom(compl.map(tvar), tmeta(e)) //tb.eff
          ), teffsempty()))))))));

    return error(`cannot synth ${expr}`);
  })
  .chain(applyTypeEff)
  .chain(ty => log(`synth done ${expr} : ${ty.type}!${ty.eff}`)
  .map(() => ty));

const checktyOpen = (expr: Expr, type: Type): TC<TypeEff> =>
  freshName(name('e'))
    .chain(x => updateCtx(Context.add(ctmeta(x, kEffs)))
    .then(checkTy(expr, typeEff(type, tmeta(x))))
    .map(() => typeEff(type, tmeta(x))))
    .chain(applyTypeEff);

const checkTy = (expr: Expr, ty: TypeEff): TC<void> =>
  log(`check ${expr} : ${ty.type}!${ty.eff}`).chain(() => {
    const type = ty.type;
    const eff = ty.eff;
    if (isTForall(type))
      return freshName(type.name).chain(x => withElems([ctvar(x, type.kind)], checkTy(expr, typeEff(type.open(tvar(x)), eff))));

    if (isTFun(type) && isAbs(expr) && !expr.type)
      return freshName(expr.name)
        .chain(x => withElems([cvar(x, type.left)], checkTy(expr.open(vr(x)), typeEff(type.right, type.eff))));

    if (isLet(expr))
      return synth(expr.expr)
        .chain(({ type: ty, eff: ef }) => openEffs(ef)
        .chain(ef => subsume(ef, eff)
        .then(freshName(expr.name)
        .chain(x => withElems([cvar(x, ty)], checkTy(expr.open(vr(x)), typeEff(type, eff)))))));

    return synth(expr)
      .chain(({ type: t, eff: e }) => apply(t)
      .chain(a => apply(type)
      .chain(b => subsume(a, b)
      .then(apply(e)
      .chain(ea => apply(eff)
      .chain(eb => subsume(ea, eb)))))));
  });

const synthapp = (type: Type, expr: Expr): TC<TypeEff> =>
  log(`synthapp ${type} @ ${expr}`).chain(() => {
    if (isTForall(type))
      return freshName(type.name)
        .chain(x => TC.of(type.open(tmeta(x)))
        .then(updateCtx(Context.add(ctmeta(x, type.kind)))
        .then(synthapp(type.open(tmeta(x)), expr))));

    if (isTMeta(type))
      return findTMeta(type.name)
        .chain(e => freshNames([type.name, type.name, name('e')])
        .chain(([a1, a2, a3]) => replace(isCTMeta(type.name), [
          ctmeta(a2, kType), ctmeta(a3, kEffs), ctmeta(a1, kType), e.solve(tfun(tmeta(a1), tmeta(a2), tmeta(a3)))
        ])
        .then(checktyOpen(expr, tmeta(a1))
        .chain(({ eff }) => apply(tmeta(a3))
        .chain(ty => unifyEffs(ty, eff))
        .map(eff => typeEff(tmeta(a2), eff))))));

    if (isTFun(type))
      return checktyOpen(expr, type.left)
        .chain(({ eff }) => unifyEffs(eff, type.eff))
        .map(eff => typeEff(type.right, eff));

    return error(`cannot synthapp ${type} @ ${expr}`);
  })
  .chain(applyTypeEff)
  .chain(ty => log(`synthapp done ${type} @ ${expr} : ${ty.type}!${ty.eff}`)
  .map(() => ty));

export const synthgen = (expr: Expr): TC<TypeEff> =>
  generalize(synth(expr))
    .chain(applyTypeEff);

export const infer = (ctx: Context, expr: Expr): Either<string, TypeEff> =>
  synthgen(expr).run(ctx, new NameRepSupply(0)).val;
