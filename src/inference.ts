import { Term, isAbs, Var, openAbs, showTerm, isVar, isApp, isAnn, isLet, openLet, abs, isIf } from './terms';
import { Type, isTForall, matchTFun, openTForall, TVar, showType, TMeta, isTMeta, TFun, substTMetas, TForall, simplifyType, tforallK, tfun, tappFrom, isTApp, TApp, tFun, TForallK, flattenTForall, tBool } from './types';
import { namestore, context, apply } from './global';
import { wfContext, wfType, wfKind } from './wellformedness';
import { infererr } from './error';
import { NameT, NameMap, createNameMap, insertNameMap, nameContains, getNameMap, showName, Name, eqName } from './names';
import { subsume } from './subsumption';
import { CVar, CTVar, CTMeta } from './elems';
import { kType, kfunFrom, kfun, Kind } from './kinds';
import { checkKindType, elaborateType } from './kindInference';
import { Def, showDef } from './definitions';
import { log } from './config';

const getByName = (cs: CTMeta[], x: NameT): CTMeta | null => {
  for (let i = 0, l = cs.length; i < l; i++) 
    if (eqName(cs[i].name, x)) return cs[i];
  return null;
};

const unsolvedInType = (unsolved: CTMeta[], type: Type, ns: CTMeta[] = []): CTMeta[] => {
  switch (type.tag) {
    case 'TVar': return ns;
    case 'TMeta': {
      const x = type.name;
      const c = getByName(unsolved, x);
      if (c && !getByName(ns, x)) ns.push(c);
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
const generalize = (unsolved: CTMeta[], type: Type): Type => {
  const ns = unsolvedInType(unsolved, type);
  const m: NameMap<TVar> = createNameMap();
  for (let i = 0, l = ns.length; i < l; i++) {
    const x = ns[i].name;
    const y = namestore.fresh(x);
    insertNameMap(x, TVar(y), m);
  }
  let c = substTMetas(type, m);
  for (let i = ns.length - 1; i >= 0; i--) {
    const e = ns[i];
    c = TForallK((getNameMap(e.name, m) as TVar).name, e.kind, c);
  }
  return c;
};
const generalizeFrom = (marker: NameT, type: Type): Type =>
  generalize(context.leaveWithUnsolved(marker), type);

const typesynth = (term: Term): Type => {
  log(`typesynth ${showTerm(term)}`);
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
    wfType(term.type);
    const ty = elaborateType(term.type);
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
  if (isIf(term)) {
    typecheck(term.cond, tBool);
    const ty = typesynth(term.then);
    typecheck(term.else_, ty);
    return ty;
  }
  return infererr(`cannot synth: ${showTerm(term)}`);
};

const typecheck = (term: Term, type: Type): void => {
  log(`typecheck ${showTerm(term)} : ${showType(type)}`);
  if (isTForall(type)) {
    if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
    const x = namestore.fresh(type.name);
    context.enter(x, CTVar(x, type.kind));
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
    typecheck(openLet(term, Var(x)), apply(type));
    context.leave(x);
    return;
  }
  if (isIf(term)) {
    typecheck(term.cond, tBool);
    typecheck(term.then, type);
    typecheck(term.else_, apply(type));
    return;
  }
  const ty = typesynth(term);
  return subsume(apply(ty), apply(type));
};

const typeappsynth = (type: Type, term: Term): Type => {
  log(`typeappsynth ${showType(type)} @ ${showTerm(term)}`);
  if (isTForall(type)) {
    if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
    const x = namestore.fresh(type.name);
    context.add(CTMeta(x, type.kind));
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
  // TODO: generalize the below for all type applications
  if (isTApp(type) && isTMeta(type.left)) {
    const x = type.left.name;
    const a = namestore.fresh(x);
    const ta = TMeta(a);
    context.replace('CTMeta', x, [
      CTMeta(a, kType),
      CTMeta(x, kfun(kType, kType), TApp(tFun, ta)),
    ]);
    typecheck(term, ta);
    return type.right;
  }
  if (isTApp(type) && isTApp(type.left) && isTMeta(type.left.left)) {
    const x = type.left.left.name;
    context.replace('CTMeta', x, [
      CTMeta(x, kfun(kType, kType, kType), tFun),
    ]);
    typecheck(term, type.left.right);
    return type.right;
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

export const inferDef = (def: Def): void => {
  log(`inferDef ${showDef(def)}`);
  switch (def.tag) {
    case 'DType': {
      const tname = def.name;
      const untname = Name(`un${tname.name}`);
      if (context.lookup('CTVar', tname))
        throw new TypeError(`type ${showName(tname)} is already defined`);
      if (context.lookup('CVar', tname))
        throw new TypeError(`${showName(tname)} is already defined`);
      if (context.lookup('CVar', untname))
        throw new TypeError(`${showName(untname)} is already defined`);
      const ty = tforallK(def.args, def.type);
      wfType(ty);
      const ety = elaborateType(ty);
      checkKindType(ety);
      const fl = flattenTForall(ety);
      const nargs = fl.args.slice(0, def.args.length);
      const ntype = tforallK(fl.args.slice(def.args.length), fl.type);
      context.add(
        CTVar(tname, kfunFrom(nargs.map(([_, k]) => k || kType).concat([kType]))),
        CVar(tname, tforallK(nargs, tfun(ntype, tappFrom([TVar(tname)].concat(nargs.map(([n]) => TVar(n))))))),
        CVar(untname, tforallK(nargs, tfun(tappFrom([TVar(tname)].concat(nargs.map(([n]) => TVar(n)))), ntype))),
      );
      return;
    }
    case 'DLet': {
      const name = def.name;
      if (context.lookup('CVar', name))
        throw new TypeError(`${showName(name)} is already defined`);
      const ty = infer(abs(def.args, def.term));
      context.add(CVar(name, ty));
      return;
    }
    case 'DDeclType': {
      const name = def.name;
      if (context.lookup('CTVar', name))
        throw new TypeError(`type ${showName(name)} is already defined`);
      wfKind(def.kind);
      context.add(CTVar(name, def.kind));
      return;
    }
    case 'DDeclare': {
      const name = def.name;
      if (context.lookup('CVar', name))
        throw new TypeError(`${showName(name)} is already defined`);
      wfType(def.type);
      const type = elaborateType(def.type);
      checkKindType(type);
      context.add(CVar(name, type));
      return;
    }
    case 'DForeign': return;
  }
};

export const inferDefs = (ds: Def[]): void =>
  ds.forEach(inferDef);
