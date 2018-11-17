import { log, ok, TypeN, TC, error, check, freshName, withElems, apply, findTMeta, pop, updateCtx, ElemN, iff, ordered, freshNames, replace } from './TC';
import { wfType, checkKind } from './wf';
import { isTVar, isTMeta, isTApp, isTForall, tmeta, tvar } from './types';
import { ctvar, ctmeta, isCTMeta, csolved } from './elems';
import NameRep from './generic/NameRep';
import Context from './generic/context';
import { kType, matchTFun, tfun } from './initial';

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
      solveUnify(a, b).catch(() => {
        const fun = matchTFun(b);
        if (fun)
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kType), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a2)))])
            .then(instUnify(a1, fun.left))
            .then(apply(fun.right))
            .chain(type => instUnify(a2, type)));
        if (isTForall(b))
          freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instUnify(a, b.open(tvar(x)))));
        return error(`instUnify failed: ${a} = ${b}`); 
      }));

export const unify = (a: TypeN, b: TypeN): TC<void> =>
  log(`unify ${a} ~ ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => checkKind(k1, k2, `unify ${a} ~ ${b}`)
      .chain(() => {
        if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
        if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
        if (isTApp(a) && isTApp(b))
          return unify(a.left, b.left)
            .then(apply(a.right)
            .chain(ta => apply(b.right)
            .chain(tb => unify(ta, tb))));
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
