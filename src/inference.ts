import { Type, showTy, TVMap, substTVar, TSkol, isTFun, TFun, prune, tmetas, quantify, tfunFrom, tappFrom, TCon, TVar, tforall } from './types';
import { impossible, freshTMeta, freshTSkol, terr, resetId, skolemCheck, Name, freshKMeta } from './util';
import { Env, tmetasEnv, skolemCheckEnv, lookupVar, extendVar, extendVars, lookupTCon, cloneEnv } from './env';
import { Term, showTerm, Pat, showPat, abs } from './terms';
import { unifyTFun, unify } from './unification';
import { inferKind } from './kindInference';
import { kType, Kind, pruneKind } from './kinds';
import { log } from './config';
import { Def, showDef } from './definitions';
import { positivityCheck } from './positivity';

const tBool = TCon('Bool');
const tNat = TCon('Nat');
const tChar = TCon('Char');
const tStr = TCon('Str');

type Expected = Check | Infer;
interface Check {
  readonly tag: 'Check';
  readonly type: Type;
}
const Check = (type: Type): Check => ({ tag: 'Check', type });
interface Infer {
  readonly tag: 'Infer';
  type: Type | null;
}
const Infer = (): Infer => ({ tag: 'Infer', type: null });
const showEx = (ex: Expected): string => {
  if (ex.tag === 'Check') return `Check(${showTy(ex.type)})`;
  if (ex.tag === 'Infer')
    return `Infer(${ex.type ? showTy(ex.type) : '...'})`;
  return impossible('showEx');
};

const instantiate = (ty: Type): Type => {
  if (ty.tag !== 'TForall') return ty;
  const m: TVMap = {};
  const names = ty.names;
  for (let i = 0, l = names.length; i < l; i++) {
    const x = names[i];
    m[x] = freshTMeta(ty.kinds[i] as Kind, x);
  }
  return substTVar(m, ty.type);
};

const skolemise = (ty: Type, sk: TSkol[] = []): Type => {
  if (ty.tag === 'TForall') {
    const m: TVMap = {};
    const names = ty.names;
    for (let i = 0, l = names.length; i < l; i++) {
      const k = freshTSkol(names[i], ty.kinds[i] as Kind);
      m[names[i]] = k;
      sk.push(k);
    }
    return skolemise(substTVar(m, ty.type), sk);
  }
  if (isTFun(ty)) {
    const { left: { right: left }, right } = ty;
    const b = skolemise(right, sk);
    return TFun(left, b);
  }
  return ty;
};

const checkRho = (env: Env, term: Term, ty: Type): void =>
  tcRho(env, term, Check(ty));
const inferRho = (env: Env, term: Term): Type => {
  const i = Infer();
  tcRho(env, term, i);
  if (!i.type)
    return terr(`inferRho failed for ${showTerm(term)}`);
  return i.type;
};
const tcRho = (env: Env, term: Term, ex: Expected): void => {
  log(() => `tcRho ${showTerm(term)} with ${showEx(ex)}`)
  if (term.tag === 'Var') {
    const ty = lookupVar(env, term.name);
    if (!ty) return terr(`undefined var ${showTerm(term)}`);
    return instSigma(env, ty, ex);
  }
  if (term.tag === 'App') {
    const ty = inferRho(env, term.left);
    const { left: { right: left }, right } = unifyTFun(env, ty);
    checkSigma(env, term.right, left);
    return instSigma(env, right, ex);
  }
  if (term.tag === 'Abs') {
    if (ex.tag === 'Check') {
      const { left: { right: left }, right } = unifyTFun(env, ex.type);
      const bs = checkPat(env, term.pat, left);
      const nenv = extendVars(env, bs);
      return checkRho(nenv, term.body, right);
    } else if (ex.tag === 'Infer') {
      const [bs, ty] = inferPat(env, term.pat);
      const nenv = extendVars(env, bs);
      const bty = inferRho(nenv, term.body);
      ex.type = TFun(ty, bty);
      return;
    }
  }
  if (term.tag === 'Let') {
    const ty = inferSigma(env, term.val);
    const nenv = extendVar(env, term.name, ty);
    return tcRho(nenv, term.body, ex);
  }
  if (term.tag === 'Ann') {
    const type = inferKind(env, term.type);
    checkSigma(env, term.term, type);
    return instSigma(env, type, ex);
  }
  if (term.tag === 'If') {
    if (ex.tag === 'Check') {
      checkRho(env, term.cond, tBool);
      tcRho(env, term.ifTrue, ex);
      tcRho(env, term.ifFalse, ex);
      return;
    } else if (ex.tag === 'Infer') {
      checkRho(env, term.cond, tBool);
      const t1 = inferRho(env, term.ifTrue);
      const t2 = inferRho(env, term.ifFalse);
      subsCheck(env, t1, t2);
      subsCheck(env, t2, t1);
      ex.type = t1;
      return;
    }
  }
  if (term.tag === 'LitNat') {
    instSigma(env, tNat, ex);
    return;
  }
  if (term.tag === 'LitChar') {
    instSigma(env, tChar, ex);
    return;
  }
  if (term.tag === 'LitStr') {
    instSigma(env, tStr, ex);
    return;
  }
  return impossible('tcRho');
};

const checkPat = (env: Env, pat: Pat, ty: Type): [Name, Type][] =>
  tcPat(env, pat, Check(ty));
const inferPat = (env: Env, pat: Pat): [[Name, Type][], Type] => {
  const i = Infer();
  const bs = tcPat(env, pat, i);
  if (!i.type)
    return terr(`inferPat failed for ${showPat(pat)}`);
  return [bs, i.type];
};
const tcPat = (
  env: Env,
  pat: Pat,
  ex: Expected
): [Name, Type][] => {
  if (pat.tag === 'PWildcard') {
    if (ex.tag === 'Infer') ex.type = freshTMeta(kType);
    return [];
  }
  if (pat.tag === 'PVar') {
    if (ex.tag === 'Check') return [[pat.name, ex.type]];
    const ty = freshTMeta(kType);
    ex.type = ty;
    return [[pat.name, ty]];
  }
  if (pat.tag === 'PAnn') {
    const ty = inferKind(env, pat.type);
    const bs = checkPat(env, pat.pat, ty);
    instPatSigma(env, ty, ex);
    return bs;
  }
  if (pat.tag === 'PCon') {
    const ty = lookupVar(env, pat.name);
    if (!ty) return terr(`undefined constructor ${pat.name} in pattern`);
    const { left: { right: left }, right } = unifyTFun(env, instantiate(ty));
    const bs = checkPat(env, pat.pat, left);
    instPatSigma(env, right, ex);
    return bs;
  }
  return impossible('tcPat');
};

const instPatSigma = (env: Env, ty: Type, ex: Expected): void => {
  if (ex.tag === 'Check')
    return subsCheck(env, ex.type, ty);
  ex.type = ty;
};

const inferSigma = (env: Env, term: Term): Type => {
  const ty = inferRho(env, term);
  const etms = tmetasEnv(env);
  const tms = tmetas(prune(ty), etms);
  return quantify(tms, ty);
};

const checkSigma = (env: Env, term: Term, ty: Type): void => {
  const sk: TSkol[] = [];
  const rho = skolemise(ty, sk);
  checkRho(env, term, rho);
  skolemCheck(sk, prune(ty));
  skolemCheckEnv(sk, env);
};

const subsCheck = (env: Env, a: Type, b: Type): void => {
  log(() => `subsCheck ${showTy(a)} <: ${showTy(b)}`);
  const sk: TSkol[] = [];
  const rho = skolemise(b, sk);
  subsCheckRho(env, a, rho);
  skolemCheck(sk, prune(a));
  skolemCheck(sk, prune(b));
};
const subsCheckRho = (env: Env, a: Type, b: Type): void => {
  if (a.tag === 'TForall')
    return subsCheckRho(env, instantiate(a), b);
  if (isTFun(b))
    return subsCheckTFun(env, unifyTFun(env, a), b);
  if (isTFun(a))
    return subsCheckTFun(env, a, unifyTFun(env, b));
  return unify(env, a, b);
};
const subsCheckTFun = (env: Env, a: TFun, b: TFun): void => {
  subsCheck(env, b.left.right, a.left.right);
  return subsCheck(env, a.right, b.right);
};

const instSigma = (env: Env, ty: Type, ex: Expected): void => {
  if (ex.tag === 'Check')
    return subsCheckRho(env, ty, ex.type);
  ex.type = instantiate(ty);
};

export const infer = (env: Env, term: Term): Type => {
  log(() => `infer ${showTerm(term)}`);
  resetId();
  return prune(inferSigma(env, term));
};

export const inferDef = (env: Env, def: Def): void => {
  log(() => `inferDef ${showDef(def)}`);
  if (def.tag === 'DType') {
    const tname = def.name;
    if (lookupTCon(env, tname))
      return terr(`type ${tname} is already defined`);
    if (lookupVar(env, tname))
      return terr(`constructor ${tname} is already defined`);
    const t = def.type;
    const tc = TCon(tname);
    const b = tfunFrom([t, tappFrom([tc as Type].concat(def.args.map(([x, _]) => TVar(x))))]);
    const ty = tforall(def.args, b);
    const nenv = cloneEnv(env);
    nenv.tcons[tname] = freshKMeta();
    const ti = inferKind(nenv, ty);
    positivityCheck(tname, ty);
    env.global[tname] = ti;
    env.tcons[tname] = pruneKind(nenv.tcons[tname]);
    return;
  }
  if (def.tag === 'DLet') {
    const name = def.name;
    if (lookupVar(env, name))
      return terr(`${name} is already defined`);
    const ty = infer(env, abs(def.args, def.term));
    env.global[name] = ty;
    return;
  }
  return impossible('inferDef');
};

export const inferDefs = (env: Env, ds: Def[]): void => {
  for (let i = 0, l = ds.length; i < l; i++)
    inferDef(env, ds[i]);
};

