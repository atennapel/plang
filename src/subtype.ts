import { tmeta, tvar, isTForall, isTMeta, isTVar, isTApp, isTRowEmpty, isTRowExtend, trowextend, isTComp, isTEffsEmpty, isTEffsExtend, tcomp, teffsextend, teffsempty } from './types';
import Context from './generic/context';
import { wfType, checkKind } from './wf';
import { kType, matchTRec, matchTVariant, kRow, matchTFun, tfun, kEffs, kEff } from './initial';
import { TypeN, TC, error, ok, pop, log, findTMeta, updateCtx, ElemN, ordered, iff, freshNames, replace, apply, freshName, withElems, check, getCtx } from './TC';
import NameRep, { name } from './generic/NameRep';
import { isCTMeta, ctmeta, csolved, ctvar } from './elems';
import { unify, rewriteRow, rewriteEffs } from './unification';
import { kcomp } from './kinds';

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
      solve(a, b).catch(err => log(`solve failed: ${err}`).chain(() => {
        const fun = matchTFun(b);
        if (fun)
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kcomp(kType)), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a2)))])
            .then(instR(fun.left, a1))
            .then(apply(fun.right))
            .chain(type => instL(a2, type)));

        if (isTComp(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kcomp(kEffs)), ctmeta(a1, kType), csolved(a, kType, tcomp(tmeta(a1), tmeta(a2)))])
            .then(instL(a1, b.type))
            .then(apply(b.eff))
            .chain(type => instL(a2, type)));
        
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instL(a, b.open(tvar(x)))));
        if (isTRowExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a1, kType), ctmeta(a2, kRow), csolved(a, kRow, trowextend(b.label, tmeta(a1), tmeta(a2)))])
            .then(instL(a1, b.type)
            .then(apply(b.rest))
            .chain(ty => instL(a1, ty))));

        if (isTEffsExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kEff), csolved(a, kRow, teffsextend(tmeta(a1), tmeta(a2)))])
            .then(instL(a1, b.type))
            .then(apply(b.rest))
            .chain(type => instL(a2, type)));
        return error(`instL failed: ${a} = ${b}`); 
      })));

const instR = (a: TypeN, b: NameRep): TC<void> =>
  log(`instR ${b} := ${a}`).then(
    isTMeta(a) ? iff(ordered(b, a.name), solve(a.name, tmeta(b)), solve(b, a)) :
      solve(b, a).catch(err => log(`solve failed: ${err}`).chain(() => {
        const fun = matchTFun(a);
        if (fun)
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kcomp(kType)), ctmeta(b1, kType), csolved(b, kType, tfun(tmeta(b1), tmeta(b2)))])
            .then(instL(b1, fun.left))
            .then(apply(fun.right))
            .chain(type => instR(type, b2)));

        if (isTComp(a))
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kcomp(kEffs)), ctmeta(b1, kType), csolved(b, kType, tcomp(tmeta(b1), tmeta(b2)))])
            .then(instR(a.type, b1))
            .then(apply(a.eff))
            .chain(type => instR(type, b2)));

        if (isTForall(a))
          return freshName(a.name).chain(x => withElems([ctmeta(x, a.kind)], instR(a.open(tmeta(x)), b)));
        if (isTRowExtend(a))
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b1, kType), ctmeta(b2, kRow), csolved(b, kRow, trowextend(a.label, tmeta(b1), tmeta(b2)))])
            .then(instR(a.type, b1)
            .then(apply(a.rest))
            .chain(ty => instR(ty, b2))));

        if (isTEffsExtend(a))
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kEffs), ctmeta(b1, kEff), csolved(b, kRow, teffsextend(tmeta(b1), tmeta(b2)))])
            .then(instR(a.type, b1))
            .then(apply(a.rest))
            .chain(type => instR(type, b2)));
        return error(`instR failed: ${b} = ${a}`);  
      })));

export const subtype = (a: TypeN, b: TypeN): TC<void> =>
  log(`subtype ${a} <: ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => {
      if (k2.equals(kcomp(k1))) return subtype(tcomp(a, teffsempty()), b);
      else return checkKind(k1, k2, `subtype ${a} <: ${b}`)
        .chain(() => {
          if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
          if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
          if (isTRowEmpty(a) && isTRowEmpty(b)) return ok;
          if (isTEffsEmpty(a) && isTEffsEmpty(b)) return ok;
          
          if (isTComp(a) && isTComp(b))
            return subtype(a.type, b.type)
              .then(apply(a.eff)
              .chain(ta => apply(b.eff)
              .chain(tb => subtype(ta, tb))));

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

          if (isTEffsEmpty(a) && isTEffsExtend(b))
            return freshName(name('r'))
              .chain(r => updateCtx(Context.add(
                ctmeta(r, kEffs),
              ))
              .then(subtype(tmeta(r), b)));
          if (isTEffsExtend(a) && isTEffsEmpty(b))
            return freshName(name('r'))
              .chain(r => updateCtx(Context.add(
                ctmeta(r, kEffs),
              ))
              .then(subtype(a, tmeta(r))));
          if (isTEffsExtend(a) && isTEffsExtend(b))
            return rewriteEffs(a.type, b, `${a} <: ${b}`)
              .chain(es => subtype(a.type, es.type)
              .then(apply(a.rest)
              .chain(restA => apply(es.rest)
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
        });
      })));
