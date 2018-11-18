import { tmeta, tvar, isTForall, isTMeta, isTVar, isTApp, isTRowEmpty, isTRowExtend } from './types';
import Context from './generic/context';
import { wfType, checkKind } from './wf';
import { kType, matchTFun, tfun, matchTRec, matchTVariant } from './initial';
import { TypeN, TC, error, ok, pop, log, findTMeta, updateCtx, ElemN, ordered, iff, freshNames, replace, apply, freshName, withElems, check } from './TC';
import NameRep from './generic/NameRep';
import { isCTMeta, ctmeta, csolved, ctvar } from './elems';
import { unify, rewriteRow } from './unification';

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
        const fun = matchTFun(b);
        if (fun)
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kType), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a2)))])
            .then(instR(fun.left, a1))
            .then(apply(fun.right))
            .chain(type => instL(a2, type)));
        if (isTForall(b))
          freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instL(a, b.open(tvar(x)))));
        return error(`instL failed: ${a} = ${b}`); 
      }));

const instR = (a: TypeN, b: NameRep): TC<void> =>
  log(`instR ${b} := ${a}`).then(
    isTMeta(a) ? iff(ordered(b, a.name), solve(a.name, tmeta(b)), solve(b, a)) :
      solve(b, a).catch(() => {
        const fun = matchTFun(a);
        if (fun)
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kType), ctmeta(b1, kType), csolved(b, kType, tfun(tmeta(b1), tmeta(b2)))])
            .then(instL(b1, fun.left))
            .then(apply(fun.right))
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
        if (isTRowEmpty(a) && isTRowEmpty(b)) return ok;
        
        const funa = matchTFun(a);
        const funb = matchTFun(b);
        if (funa && funb)
          return subtype(funb.left, funa.left)
            .then(apply(funa.right)
            .chain(ta => apply(funb.right)
            .chain(tb => subtype(ta, tb))));
        
        const reca = matchTRec(a);
        const recb = matchTRec(b);
        if (reca && recb) return subtype(reca, recb);
        const vara = matchTVariant(a);
        const varb = matchTVariant(b);
        if (vara && varb) return subtype(vara, varb);

        if (isTRowExtend(a) && isTRowExtend(b))
          return rewriteRow(a.label, b, `${a} <: ${b}`)
            .chain(row => subtype(a.type, row.type)
            .then(apply(a.rest)
            .chain(restA => apply(row.rest)
            .chain(restB => subtype(restA, restB)))));

        if (isTApp(a) && isTApp(b))
          return unify(a, b);
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
