import { tmeta, tfun, tvar, isTFun, isTForall, isTMeta, isTVar } from './types';
import Context from './generic/context';
import { wfType, checkKind } from './wf';
import { kType } from './initial';
import { TypeN, TC, error, ok, pop, log, findTMeta, updateCtx, ElemN, ordered, iff, freshNames, replace, apply, freshName, withElems, check } from './TC';
import NameRep from './generic/NameRep';
import { isCTMeta, ctmeta, csolved, ctvar } from './elems';

const solve = (a: NameRep, b: TypeN): TC<void> =>
  log(`solve ${a} = ${b}`).then(
    !b.isMono() ? error(`polymorphic type in solve ${a} = ${b}`) :
    findTMeta(a).chain(e =>
      pop(isCTMeta(a))
        .chain(right => wfType(b)
        .then(updateCtx(Context.append(Context.of(e.solve(b) as ElemN).append(right)))))));

const instL = (a: NameRep, b: TypeN): TC<void> =>
  log(`instL ${a} := ${b}`).then(
    isTMeta(b) ? iff(ordered(a, b.name), solve(b.name, tmeta(a)), solve(a, b)) :
      solve(a, b).catch(() => {
        if (isTFun(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kType), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a2)))])
            .then(instR(b.left, a1))
            .then(apply(b.right))
            .chain(type => instL(a2, type)));
        if (isTForall(b))
          freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instL(a, b.open(tvar(x)))));
        return error(`instL failed: ${a} = ${b}`); 
      }));

const instR = (a: TypeN, b: NameRep): TC<void> =>
  log(`instR ${b} := ${a}`).then(
    isTMeta(a) ? iff(ordered(b, a.name), solve(a.name, tmeta(b)), solve(b, a)) :
      solve(b, a).catch(() => {
        if (isTFun(a))
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kType), ctmeta(b1, kType), csolved(b, kType, tfun(tmeta(b1), tmeta(b2)))])
            .then(instL(b1, a.left))
            .then(apply(a.right))
            .chain(type => instR(type, b2)));
        if (isTForall(a))
          freshName(a.name).chain(x => withElems([ctmeta(x, a.kind)], instR(a.open(tmeta(x)), b)));
        return error(`instR failed: ${b} = ${a}`);  
      }));

export const subtype = (a: TypeN, b: TypeN): TC<void> =>
  log(`subtype ${a} <: ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => checkKind(k1, k2, `subtype ${a} <: ${b}`)
      .chain(() => {
        if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
        if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
        if (isTFun(a) && isTFun(b))
          return subtype(b.left, a.left)
            .then(apply(a.right)
            .chain(ta => apply(b.right)
            .chain(tb => subtype(ta, tb))));
        if (isTForall(a))
          return freshName(a.name).chain(x => withElems([ctmeta(x, a.kind)], subtype(a.open(tmeta(x)), b)));
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], subtype(a, b.open(tvar(x)))));
        if (isTMeta(a))
          return check(!b.containsTMeta(a.name), `occurs check failed L: ${a} in ${b}`)
            .then(instL(a.name, b));
        if (isTMeta(b))
          return check(!a.containsTMeta(b.name), `occurs check failed R: ${b} in ${a}`)
            .then(instR(a, b.name));
        return error(`subtype failed: ${a} <: ${b}`);
      }))));
