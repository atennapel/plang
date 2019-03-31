import { Term, isAbs, Var, openAbs, showTerm, isVar, isApp, isAnn, isLet, openLet, abs, isIf, isQuery, Abs, App, Ann, If, Let } from './terms';
import { Type, isTForall, matchTFun, openTForall, TVar, showType, TMeta, isTMeta, TFun, substTMetas, TForall, simplifyType, tforallK, tfun, tappFrom, isTApp, TApp, tFun, TForallK, flattenTForall, tBool } from './types';
import { namestore, context, apply, storeContext, restoreContext } from './global';
import { wfContext, wfType, wfKind } from './wellformedness';
import { infererr, InferError } from './error';
import { NameT, NameMap, createNameMap, insertNameMap, nameContains, getNameMap, showName, Name, eqName } from './names';
import { subsume } from './subsumption';
import { CVar, CTVar, CTMeta, showElem, CQuery } from './elems';
import { kType, kfunFrom, kfun, Kind } from './kinds';
import { checkKindType, elaborateType } from './kindInference';
import { Def, showDef, DLet } from './definitions';
import { log } from './config';

const resolveImplicit = (type: Type): [NameT, Type] => {
  log(`resolveImplicit ${showType(type)}`);
  const x = context.first(e => {
    if (e.tag !== 'CVar') return null;
    storeContext();
    try {
      subsume(e.type, type);
      log(`select ${showName(e.name)} : ${showType(e.type)}`);
      return [e.name, e.type] as [NameT, Type];
    } catch (err) {
      if (!(err instanceof InferError)) throw err;
      return null;
    } finally {
      restoreContext();
    }
  });
  if (!x) return infererr(`no implicit value found for: ${showType(type)}`);
  return x;
};

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
const generalizeFrom = (marker: NameT, type: Type): Type => {
  console.log(`${context}`);
  const [us, qs] = context.leaveWithUnsolved(marker);
  console.log(qs.map(showElem).join(', '));
  return generalize(us, type);
};

const typesynth = (term: Term): [Type, Term] => {
  log(`typesynth ${showTerm(term)}`);
  if (isVar(term)) {
    const x = context.lookup('CVar', term.name);
    if (!x) return infererr(`undefined var ${showName(term.name)}`);
    return [x.type, term];
  }
  if (isAbs(term)) {
    const x = namestore.fresh(term.name);
    const a = namestore.fresh(term.name);
    const b = namestore.fresh(term.name);
    const ta = TMeta(a);
    const tb = TMeta(b);
    context.enter(x, CTMeta(a, kType), CTMeta(b, kType), CVar(x, ta));
    const body = typecheck(openAbs(term, Var(x)), tb);
    const ty = apply(TFun(ta, tb));
    return [generalizeFrom(x, ty), Abs(x, body)];
  }
  if (isApp(term)) {
    const [left, nleft] = typesynth(term.left);
    const [right, nright] = typeappsynth(apply(left), term.right);
    return [right, App(nleft, nright)];
  }
  if (isAnn(term)) {
    wfType(term.type);
    const ty = elaborateType(term.type);
    checkKindType(ty);
    const nterm = typecheck(term.term, ty);
    return [ty, Ann(nterm, ty)];
  }
  if (isLet(term)) {
    const [ty, nterm] = typesynth(term.term);
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, ty));
    const [uty, nbody] = typesynth(openLet(term, Var(x)));
    const rty = apply(uty);
    const gty = generalizeFrom(x, rty);
    return [gty, Let(x, nterm, nbody)];
  }
  if (isIf(term)) {
    const ncond = typecheck(term.cond, tBool);
    const [ty, nthen] = typesynth(term.then);
    const nelse = typecheck(term.else_, ty);
    return [ty, If(ncond, nthen, nelse)];
  }
  if (isQuery(term)) {
    const x = namestore.fresh('q');
    const tm = TMeta(x);
    context.add(CTMeta(x, kType), CQuery(x, tm));
    return [tm, Var(x)];
  }
  return infererr(`cannot synth: ${showTerm(term)}`);
};

const typecheck = (term: Term, type: Type): Term => {
  log(`typecheck ${showTerm(term)} : ${showType(type)}`);
  if (isTForall(type)) {
    if (!type.kind) return infererr(`forall lacks kind: ${showType(type)}`);
    const x = namestore.fresh(type.name);
    context.enter(x, CTVar(x, type.kind));
    const nterm = typecheck(term, openTForall(type, TVar(x)));
    context.leave(x);
    return nterm;
  }
  const f = matchTFun(type);
  if (isAbs(term) && f) {
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, f.left));
    const body = typecheck(openAbs(term, Var(x)), f.right);
    context.leave(x);
    return Abs(x, body);
  }
  if (isLet(term)) {
    const [ty, nterm] = typesynth(term.term);
    const x = namestore.fresh(term.name);
    context.enter(x, CVar(x, ty));
    const nbody = typecheck(openLet(term, Var(x)), apply(type));
    context.leave(x);
    return Let(x, nterm, nbody);
  }
  if (isIf(term)) {
    const ncond = typecheck(term.cond, tBool);
    const nthen = typecheck(term.then, type);
    const nelse = typecheck(term.else_, apply(type));
    return If(ncond, nthen, nelse);
  }
  if (isQuery(term)) {
    const x = namestore.fresh('q');
    context.add(CQuery(x, type));
    return Var(x);
  }
  const [ty, nterm] = typesynth(term);
  subsume(apply(ty), apply(type));
  return nterm;
};

const typeappsynth = (type: Type, term: Term): [Type, Term] => {
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
    const nterm = typecheck(term, ta);
    return [tb, nterm];
  }
  const f = matchTFun(type);
  if (f) {
    const nterm = typecheck(term, f.left);
    return [f.right, nterm];
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
    const nterm = typecheck(term, ta);
    return [type.right, nterm];
  }
  if (isTApp(type) && isTApp(type.left) && isTMeta(type.left.left)) {
    const x = type.left.left.name;
    context.replace('CTMeta', x, [
      CTMeta(x, kfun(kType, kType, kType), tFun),
    ]);
    const nterm = typecheck(term, type.left.right);
    return [type.right, nterm];
  }
  return infererr(`cannot typeappsynth: ${showType(type)} @ ${showTerm(term)}`);
};

export const infer = (term: Term): [Type, Term] => {
  namestore.reset();
  wfContext();
  const m = namestore.fresh('m');
  context.enter(m);
  try {
    const [uty, nterm] = typesynth(term);
    const ty = generalizeFrom(m, apply(uty));
    checkKindType(ty);
    if (!context.isComplete()) return infererr(`incomplete context: ${context}`);
    return [simplifyType(ty), nterm];
  } catch (err) {
    context.leave(m);
    throw err;
  }
};

export const inferDef = (def: Def): Def => {
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
      return def;
    }
    case 'DLet': {
      const name = def.name;
      if (context.lookup('CVar', name))
        throw new TypeError(`${showName(name)} is already defined`);
      const [ty, term] = infer(abs(def.args, def.term));
      context.add(CVar(name, ty));
      return DLet(name, [], term);
    }
    case 'DDeclType': {
      const name = def.name;
      if (context.lookup('CTVar', name))
        throw new TypeError(`type ${showName(name)} is already defined`);
      wfKind(def.kind);
      context.add(CTVar(name, def.kind));
      return def;
    }
    case 'DDeclare': {
      const name = def.name;
      if (context.lookup('CVar', name))
        throw new TypeError(`${showName(name)} is already defined`);
      wfType(def.type);
      const type = elaborateType(def.type);
      checkKindType(type);
      context.add(CVar(name, type));
      return def;
    }
    case 'DForeign': return def;
  }
};

export const inferDefs = (ds: Def[]): Def[] =>
  ds.map(inferDef);
