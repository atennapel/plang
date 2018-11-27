import TC, { log, ok, error, check, freshName, withElems, apply, findTMeta, pop, updateCtx, iff, ordered, freshNames, replace, pure } from './TC';
import { wfType, checkKind } from './wf';
import Type, { isTVar, isTMeta, isTApp, isTForall, tmeta, tvar, isTEffsEmpty, TEffsExtend, isTEffsExtend, flattenTApp, teffsextend, headTApp, isTFun, tfun, flattenTForall, flattenEffs, teffsFrom, tforalls, teffsempty, tapp, isTComp, tcomp } from './types';
import Elem, { ctvar, ctmeta, isCTMeta, csolved } from './elems';
import NameRep, { name } from '../NameRep';
import Context from './context';
import { kEffs, kType, kEff, kComp } from './kinds';
import { any, remove } from '../utils';

/*
export const closeTFun = (type: Type): Type => {
  if (!isTForall(type)) return type;
  const f = flattenTForall(type);
  const body = f.type;
  if (!isTFun(body)) return type;
  const eff = body.eff;
  const feff = flattenEffs(eff);
  const tv = feff.rest;
  if (!isTVar(tv)) return type;
  const name = tv.name;
  if (body.left.containsTVar(name)) return type;
  if (body.right.containsTVar(name)) return type;
  if (any(feff.types, t => t.containsTVar(name))) return type;
  const neff = teffsFrom(feff.types);
  const args = remove(f.ns, ([n, k]) => name.equals(n));
  return tforalls(args, tfun(body.left, neff, body.right));
};
*/

export const closeEffs = (type: Type): TC<Type> =>
  log(`closeEffs ${type}`).chain(() => {
    if (isTEffsExtend(type))
      return closeEffs(type.rest).map(rest => teffsextend(type.type, rest));
    if (isTMeta(type))
      return TC.of(teffsempty());
    if (isTEffsEmpty(type))
      return TC.of(type);
    return error(`unexpected type in openEffs ${type}`);
  });

export const openEffs = (type: Type): TC<Type> =>
  log(`openEffs ${type}`).chain(() => {
    if (isTEffsExtend(type))
      return openEffs(type.rest).map(rest => teffsextend(type.type, rest));
    if (isTEffsEmpty(type))
      return freshName(name('e')).chain(e => updateCtx(Context.add(ctmeta(e, kEffs))).map(() => tmeta(e)));
    if (isTMeta(type))
      return TC.of(type);
    return error(`unexpected type in openEffs ${type}`);
  });

export const rewriteEffs = (head: Type, type: Type, msg: string): TC<TEffsExtend> =>
  log(`rewriteEffs ${head} in ${type}`).chain(() => {
    if (isTEffsExtend(type)) {
      if (headTApp(type.type).equals(headTApp(head))) return pure(type);
      return rewriteEffs(head, type.rest, msg)
        .map(rest => teffsextend(rest.type, teffsextend(type.type, rest.rest)));
    }
    if (isTMeta(type))
      return freshName(name('e'))
        .chain(r => replace(isCTMeta(type.name), [
          ctmeta(r, kEffs),
          csolved(type.name, kEffs, teffsextend(head, tmeta(r)))
        ])
        .map(() => teffsextend(head, tmeta(r))));
    return error(`cannot rewrite effs ${head} in ${type}: ${msg}`);
  });

const solveUnify = (a: NameRep, b: Type): TC<void> =>
  log(`solve unify ${a} = ${b}`).then(
    !b.isMono() ? error(`polymorphic type in solve unify ${a} = ${b}`) :
    findTMeta(a).chain(e =>
      pop(isCTMeta(a))
        .chain(right => wfType(b)
        .then(updateCtx(Context.append(Context.of(e.solve(b) as Elem).append(right)))))));

export const instUnify = (a: NameRep, b: Type): TC<void> =>
  log(`instUnify ${a} := ${b}`).then(
    isTMeta(b) ? iff(ordered(a, b.name), solveUnify(b.name, tmeta(a)), solveUnify(a, b)) :
      solveUnify(a, b).catch(err => log(`solveUnify failed: ${err}`).chain(() => {
        if (isTFun(b))
          return freshNames([a, a, a])
            .chain(([a1, a2, a3]) => replace(isCTMeta(a), [ctmeta(a2, kType), ctmeta(a3, kEffs), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tcomp(tmeta(a2), tmeta(a3))))])
            .then(instUnify(a1, b.left))
            .then(apply(b.right))
            .chain(type => unify(tcomp(tmeta(a2), tmeta(a3)), type)));
        if (isTComp(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kType), csolved(a, kComp, tcomp(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.type))
            .then(apply(b.eff))
            .chain(eff => instUnify(a2, eff)));
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instUnify(a, b.open(tvar(x)))));
        if (isTEffsExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kEff), csolved(a, kEffs, teffsextend(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.type))
            .then(apply(b.rest))
            .chain(type => instUnify(a2, type)));
        if (isTApp(b))
          return wfType(b)
            .chain(kAll => wfType(b.left)
            .chain(kLeft => wfType(b.right)
            .chain(kRight => freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kRight), ctmeta(a1, kLeft), csolved(a, kAll, tapp(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.left)
            .then(apply(b.right)
            .chain(right => instUnify(a2, right))))))));
        return error(`instUnify failed: ${a} = ${b}`); 
      })));

export const unify = (a: Type, b: Type): TC<void> =>
  log(`unify ${a} ~ ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => checkKind(k1, k2, `unify ${a} ~ ${b}`)
      .chain(() => {
        if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
        if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
        if (isTEffsEmpty(a) && isTEffsEmpty(b)) return ok;

        if (isTFun(a) && isTFun(b))
          return unify(a.left, b.left)
            .then(apply(a.right)
            .chain(ta => apply(b.right)
            .chain(tb => unify(ta, tb))));

        if (isTComp(a) && isTComp(b))
          return unify(a.type, b.type)
            .then(apply(a.eff)
            .chain(ta => apply(b.eff)
            .chain(tb => unify(ta, tb))));

        if (isTApp(a) && isTApp(b))
          return unify(a.left, b.left)
            .then(apply(a.right)
            .chain(ta => apply(b.right)
            .chain(tb => unify(ta, tb))));

        if (isTEffsExtend(a) && isTEffsExtend(b))
          return rewriteEffs(a.type, b, `${a} <: ${b}`)
            .chain(es => unify(a.type, es.type)
            .then(apply(a.rest)
            .chain(restA => apply(es.rest)
            .chain(restB => unify(restA, restB)))));
              
        if (isTForall(a) && isTForall(b))
          return checkKind(a.kind, b.kind, `unification of ${a} ~ ${b}`)
            .then(freshName(a.name)
            .chain(x => withElems([ctvar(x, a.kind)], unify(a.open(tvar(x)), b.open(tvar(x))))));
        if (isTMeta(a))
          return check(!b.containsTMeta(a.name), `occurs check failed L unify: ${a} in ${b}`)
            .then(instUnify(a.name, b));
        if (isTMeta(b))
          return check(!a.containsTMeta(b.name), `occurs check failed R unify: ${b} in ${a}`)
            .then(instUnify(b.name, a));
        return error(`unification failed: ${a} <: ${b}`);
      }))));
