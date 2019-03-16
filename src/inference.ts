import { Term, isAbs, Var, openAbs, showTerm, isVar, isApp, isAnn, isLet, openLet } from './terms';
import { Type, isTForall, matchTFun, openTForall, TVar, showType, TMeta, isTMeta, TFun, substTMetas, TForall, simplifyType } from './types';
import { namestore, context, apply } from './global';
import { wfContext, wfType } from './wellformedness';
import { infererr } from './error';
import { NameT, NameMap, createNameMap, insertNameMap, nameContains, getNameMap, showName } from './names';
import { subsume } from './subsumption';
import { CVar, CTVar, CKMeta, CTMeta } from './elems';
import { KMeta, kType } from './kinds';
import { checkKindType } from './kindInference';

const unsolvedInType = (unsolved: NameT[], type: Type, ns: NameT[] = []): NameT[] => {
  switch (type.tag) {
    case 'TVar': return ns;
    case 'TMeta': {
      const x = type.name;
      if (nameContains(unsolved, x) && !nameContains(ns, x)) ns.push(x);
      return ns;
    }
    case 'TApp': {
      unsolvedInType(unsolved, type.left, ns);
      return unsolvedInType(unsolved, type.right, ns);
    }
    case 'TForall':
      return unsolvedInType(unsolved, type.type, ns);
  }
};
const generalize = (unsolved: NameT[], type: Type): Type => {
  const ns = unsolvedInType(unsolved, type);
  const m: NameMap<TVar> = createNameMap();
  for (let i = 0, l = ns.length; i < l; i++) {
    const x = ns[i];
    const y = namestore.fresh(x);
    insertNameMap(x, TVar(y), m);
  }
  let c = substTMetas(type, m);
  for (let i = ns.length - 1; i >= 0; i--)
    c = TForall((getNameMap(ns[i], m) as TVar).name, c);
  return c;
};
const generalizeFrom = (marker: NameT, type: Type): Type =>
  generalize(context.leaveWithUnsolved(marker), type);

const typesynth = (term: Term): Type => {
  if (isVar(term)) {
    const x = context.lookup('CVar', term.name);
    if (!x) return infererr(`undefined var ${showName(term.name)}`);
    return x.type;
  }
  if (isAbs(term)) {
    const x = namestore.fresh(term.name);
    const a = namestore.fresh(term.name);
    const b = namestore.fresh(term.name);
    const ta = TMeta(a);
    const tb = TMeta(b);
    context.enter(x, CTMeta(a, kType), CTMeta(b, kType), CVar(x, ta));
    typecheck(openAbs(term, Var(x)), tb);
    const ty = apply(TFun(ta, tb));
    return generalizeFrom(x, ty);
  }
  if (isApp(term)) {
    const left = typesynth(term.left);
    return typeappsynth(apply(left), term.right);
  }
  if (isAnn(term)) {
    const ty = term.type;
    wfType(ty);
    checkKindType(ty);
    typecheck(term.term, ty);
    return ty;
  }
  if (isLet(term)) {
    const ty = typesynth(term.term);
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, ty));
    const rty = apply(typesynth(openLet(term, Var(x))));
    return generalizeFrom(x, rty);
  }
  return infererr(`cannot synth: ${showTerm(term)}`);
};

const typecheck = (term: Term, type: Type): void => {
  if (isTForall(type)) {
    const x = namestore.fresh(type.name);
    if (type.kind) {
      context.enter(x, CTVar(x, type.kind));
    } else {
      const k = namestore.fresh(type.name);
      context.enter(x, CKMeta(k), CTVar(x, KMeta(k)));
    }
    typecheck(term, openTForall(type, TVar(x)));
    context.leave(x);
    return;
  }
  const f = matchTFun(type);
  if (isAbs(term) && f) {
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, f.left));
    typecheck(openAbs(term, Var(x)), f.right);
    context.leave(x);
    return;
  }
  if (isLet(term)) {
    const ty = typesynth(term.term);
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, ty));
    typecheck(openLet(term, Var(x)), type);
    context.leave(x);
    return;
  }
  const ty = typesynth(term);
  return subsume(apply(ty), apply(type));
};

const typeappsynth = (type: Type, term: Term): Type => {
  if (isTForall(type)) {
    const x = namestore.fresh(type.name);
    if (type.kind) {
      context.add(CTMeta(x, type.kind));
    } else {
      const k = namestore.fresh(type.name);
      context.add(CKMeta(k), CTMeta(x, KMeta(k)));
    }
    return typeappsynth(openTForall(type, TMeta(x)), term);
  }
  if (isTMeta(type)) {
    const x = type.name;
    const a = namestore.fresh(x);
    const b = namestore.fresh(x);
    const ta = TMeta(a);
    const tb = TMeta(b);
    context.replace('CTMeta', x, [
      CTMeta(b, kType),
      CTMeta(a, kType),
      CTMeta(x, kType, TFun(ta, tb)),
    ])
    typecheck(term, ta);
    return tb;
  }
  const f = matchTFun(type);
  if (f) {
    typecheck(term, f.left);
    return f.right;
  }
  return infererr(`cannot typeappsynth: ${showType(type)} @ ${showTerm(term)}`);
};

export const infer = (term: Term): Type => {
  namestore.reset();
  wfContext();
  const m = namestore.fresh('m');
  context.enter(m);
  try {
    const ty = generalizeFrom(m, apply(typesynth(term)));
    checkKindType(ty);
    if (!context.isComplete()) return infererr(`incomplete context: ${context}`);
    return simplifyType(ty);
  } catch (err) {
    context.leave(m);
    throw err;
  }
};
