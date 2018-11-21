import { log, ok, TypeN, TC, error, check, freshName, withElems, apply, findTMeta, pop, updateCtx, ElemN, iff, ordered, freshNames, replace, pure } from './TC';
import { wfType, checkKind } from './wf';
import { isTVar, isTMeta, isTApp, isTForall, tmeta, tvar, isTRowEmpty, TRowEmpty, isTRowExtend, trowextend, TRowExtend, isTComp, isTEffsEmpty, TEffsExtend, isTEffsExtend, flattenTApp, teffsextend, tcomp, headTApp } from './types';
import { ctvar, ctmeta, isCTMeta, csolved } from './elems';
import NameRep, { name } from './generic/NameRep';
import Context from './generic/context';
import { kType, kRow, matchTFun, tfun, kEffs, kEff } from './initial';
import { kcomp } from './kinds';

export const rewriteRow = (label: NameRep, type: TypeN, msg: string): TC<TRowExtend<NameRep>> =>
  log(`rewriteRow ${label} in ${type}`).chain(() => {
    if (isTRowExtend(type)) {
      if (type.label.equals(label)) return pure(type);
      return rewriteRow(label, type.rest, msg)
        .map(rest => trowextend(label, rest.type, trowextend(type.label, type.type, rest.rest)));
    }
    if (isTMeta(type))
      return freshNames([name('t'), name('r')])
        .chain(([t, r]) => replace(isCTMeta(type.name), [
          ctmeta(r, kRow),
          ctmeta(t, kType),
          csolved(type.name, kRow, trowextend(label, tmeta(t), tmeta(r)))
        ])
        .map(() => trowextend(label, tmeta(t), tmeta(r))));
    return error(`cannot rewrite row ${label} in ${type}: ${msg}`);
  });

export const rewriteEffs = (head: TypeN, type: TypeN, msg: string): TC<TEffsExtend<NameRep>> =>
  log(`rewriteEffs ${head} in ${type}`).chain(() => {
    if (isTEffsExtend(type)) {
      if (headTApp(type.type).equals(headTApp(head))) return pure(type);
      return rewriteEffs(head, type.rest, msg)
        .map(rest => teffsextend(rest.type, teffsextend(type.type, rest.rest)));
    }
    if (isTMeta(type))
      return freshName(name('r'))
      .chain(r => replace(isCTMeta(type.name), [
        ctmeta(r, kEffs),
        csolved(type.name, kEffs, teffsextend(head, tmeta(r)))
      ])
      .map(() => teffsextend(head, tmeta(r))));
    return error(`cannot rewrite effs ${head} in ${type}: ${msg}`);
  });

const solveUnify = (a: NameRep, b: TypeN): TC<void> =>
  log(`solve unify ${a} = ${b}`).then(
    // !b.isMono() ? error(`polymorphic type in solve unify ${a} = ${b}`) :
    findTMeta(a).chain(e =>
      pop(isCTMeta(a))
        .chain(right => wfType(b)
        .then(updateCtx(Context.append(Context.of(e.solve(b) as ElemN).append(right)))))));

const instUnify = (a: NameRep, b: TypeN): TC<void> =>
  log(`instUnify ${a} := ${b}`).then(
    isTMeta(b) ? iff(ordered(a, b.name), solveUnify(b.name, tmeta(a)), solveUnify(a, b)) :
      solveUnify(a, b).catch(err => log(`solveUnify failed: ${err}`).chain(() => {
        const fun = matchTFun(b);
        if (fun)
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kcomp(kType)), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, fun.left))
            .then(apply(fun.right))
            .chain(type => instUnify(a2, type)));
        if (isTComp(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kType), csolved(a, kType, tcomp(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.type))
            .then(apply(b.eff))
            .chain(type => instUnify(a2, type)));
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instUnify(a, b.open(tvar(x)))));
        if (isTRowExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kRow), ctmeta(a1, kType), csolved(a, kRow, trowextend(b.label, tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.type))
            .then(apply(b.rest))
            .chain(type => instUnify(a2, type)));
        if (isTEffsExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kEff), csolved(a, kRow, teffsextend(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, b.type))
            .then(apply(b.rest))
            .chain(type => instUnify(a2, type)));
        return error(`instUnify failed: ${a} = ${b}`); 
      })));

export const unify = (a: TypeN, b: TypeN): TC<void> =>
  log(`unify ${a} ~ ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => checkKind(k1, k2, `unify ${a} ~ ${b}`)
      .chain(() => {
        if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
        if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
        if (isTRowEmpty(a) && isTRowEmpty(b)) return ok;
        if (isTEffsEmpty(a) && isTEffsEmpty(b)) return ok;

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

        if (isTRowExtend(a) && isTRowExtend(b))
          return rewriteRow(a.label, b, `${a} <: ${b}`)
            .chain(row => unify(a.type, row.type)
            .then(apply(a.rest)
            .chain(restA => apply(row.rest)
            .chain(restB => unify(restA, restB)))));

        if (isTEffsEmpty(a) && isTEffsExtend(b))
          return freshName(name('r'))
            .chain(r => updateCtx(Context.add(
              ctmeta(r, kEffs),
            ))
            .then(unify(tmeta(r), b)));
        if (isTEffsExtend(a) && isTEffsEmpty(b))
          return freshName(name('r'))
            .chain(r => updateCtx(Context.add(
              ctmeta(r, kEffs),
            ))
            .then(unify(a, tmeta(r))));
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
