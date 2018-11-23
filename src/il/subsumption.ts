import Type, { tmeta, tvar, isTForall, isTMeta, isTVar, isTApp, isTEffsEmpty, isTEffsExtend, teffsextend, teffsempty, isTFun, tfun } from './types';
import Context from './context';
import { wfType, checkKind } from './wf';
import TC, { error, ok, pop, log, findTMeta, updateCtx, ordered, iff, freshNames, replace, apply, freshName, withElems, check, getCtx } from './TC';
import NameRep, { name } from '../NameRep';
import Elem, { isCTMeta, ctmeta, csolved, ctvar } from './elems';
import { unify, rewriteEffs } from './unification';
import { kType, kEffs, kEff } from './kinds';

const solve = (a: NameRep, b: Type): TC<void> =>
  log(`solve ${a} = ${b}`).then(
    !b.isMono() ? error(`polymorphic type in solve ${a} = ${b}`) :
    findTMeta(a).chain(e =>
      pop(isCTMeta(a))
        .chain(right => wfType(b)
        .then(updateCtx(Context.append(Context.of(e.solve(b) as Elem).append(right)))))));

const instL = (a: NameRep, b: Type): TC<void> =>
  log(`instL ${a} := ${b}`).then(
    isTMeta(b) ? iff(ordered(a, b.name), solve(b.name, tmeta(a)), solve(a, b)) :
      solve(a, b).catch(err => log(`solve failed: ${err}`).chain(() => {
        if (isTFun(b))
          return freshNames([a, a, a])
            .chain(([a1, a2, a3]) => replace(isCTMeta(a), [ctmeta(a2, kType), ctmeta(a3, kEffs), ctmeta(a1, kType), csolved(a, kType, tfun(tmeta(a1), tmeta(a3), tmeta(a2)))])
            .then(instR(b.left, a1))
            .then(apply(b.eff))
            .chain(eff => instL(a3, eff))
            .then(apply(b.right))
            .chain(type => instL(a2, type)));
        
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], instL(a, b.open(tvar(x)))));

        if (isTEffsExtend(b))
          return freshNames([a, a])
            .chain(([a1, a2]) => replace(isCTMeta(a), [ctmeta(a2, kEffs), ctmeta(a1, kEff), csolved(a, kEffs, teffsextend(tmeta(a1), tmeta(a2)))])
            .then(instL(a1, b.type))
            .then(apply(b.rest))
            .chain(type => instL(a2, type)));
        return error(`instL failed: ${a} = ${b}`); 
      })));

const instR = (a: Type, b: NameRep): TC<void> =>
  log(`instR ${b} := ${a}`).then(
    isTMeta(a) ? iff(ordered(b, a.name), solve(a.name, tmeta(b)), solve(b, a)) :
      solve(b, a).catch(err => log(`solve failed: ${err}`).chain(() => {
        if (isTFun(a))
          return freshNames([b, b, b])
            .chain(([b1, b2, b3]) => replace(isCTMeta(b), [ctmeta(b2, kType), ctmeta(b3, kEffs), ctmeta(b1, kType), csolved(b, kType, tfun(tmeta(b1), tmeta(b3), tmeta(b2)))])
            .then(instL(b1, a.left))
            .then(apply(a.eff))
            .chain(eff => instR(eff, b3))
            .then(apply(a.right))
            .chain(type => instR(type, b2)));

        if (isTForall(a))
          return freshName(a.name).chain(x => withElems([ctmeta(x, a.kind)], instR(a.open(tmeta(x)), b)));

        if (isTEffsExtend(a))
          return freshNames([b, b])
            .chain(([b1, b2]) => replace(isCTMeta(b), [ctmeta(b2, kEffs), ctmeta(b1, kEff), csolved(b, kEffs, teffsextend(tmeta(b1), tmeta(b2)))])
            .then(instR(a.type, b1))
            .then(apply(a.rest))
            .chain(type => instR(type, b2)));
        return error(`instR failed: ${b} = ${a}`);  
      })));

export const subsume = (a: Type, b: Type): TC<void> =>
  log(`subsume ${a} <: ${b}`).then(
    wfType(a).chain(k1 => wfType(b).chain(k2 => checkKind(k1, k2, `subsume ${a} <: ${b}`)
      .chain(() => {
        if (isTVar(a) && isTVar(b) && a.name.equals(b.name)) return ok;
        if (isTMeta(a) && isTMeta(b) && a.name.equals(b.name)) return ok;
        if (isTEffsEmpty(a) && isTEffsEmpty(b)) return ok;
        
        if (isTFun(a) && isTFun(b))
          return subsume(b.left, a.left)
            .then(apply(a.right)
            .chain(ta => apply(b.right)
            .chain(tb => subsume(ta, tb))));
        
        if (isTEffsEmpty(a) && isTEffsExtend(b))
          return freshName(name('r'))
            .chain(r => updateCtx(Context.add(
              ctmeta(r, kEffs),
            ))
            .then(subsume(tmeta(r), b)));
        if (isTEffsExtend(a) && isTEffsEmpty(b))
          return freshName(name('r'))
            .chain(r => updateCtx(Context.add(
              ctmeta(r, kEffs),
            ))
            .then(subsume(a, tmeta(r))));
        if (isTEffsExtend(a) && isTEffsExtend(b))
          return rewriteEffs(a.type, b, `${a} <: ${b}`)
            .chain(es => subsume(a.type, es.type)
            .then(apply(a.rest)
            .chain(restA => apply(es.rest)
            .chain(restB => subsume(restA, restB)))));

        if (isTApp(a) && isTApp(b))
          return unify(a, b);
        if (isTForall(a))
          return freshName(a.name).chain(x => withElems([ctmeta(x, a.kind)], subsume(a.open(tmeta(x)), b)));
        if (isTForall(b))
          return freshName(b.name).chain(x => withElems([ctvar(x, b.kind)], subsume(a, b.open(tvar(x)))));
        if (isTMeta(a))
          return check(!b.containsTMeta(a.name), `occurs check failed L: ${a} in ${b}`)
            .then(instL(a.name, b));
        if (isTMeta(b))
          return check(!a.containsTMeta(b.name), `occurs check failed R: ${b} in ${a}`)
            .then(instR(a, b.name));
        return error(`subsume failed: ${a} <: ${b}`);
      }))));