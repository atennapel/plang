import { impossible, Name } from './util';
import { Pat, Term, LitNat, LitInt, LitChar, App, Abs, PVar, PWildcard, appFrom, Var, abs } from './terms';
import { MFVar, MTerm, MApp, MAbs, MBVar, GEnv, MClos, reduce, makeClos, LNil } from './machine.new';
import { Def } from './definitions';

export const patToMachine = (pat: Pat): Name => {
  if (pat.tag === 'PWildcard') return '_';
  if (pat.tag === 'PVar') return pat.name;
  if (pat.tag === 'PAnn') return patToMachine(pat.pat);
  if (pat.tag === 'PCon') return patToMachine(pat.pat);
  return impossible('patToMachine');
};

const tIf = Var('if');
const tNil = MFVar('nil');
const tCons = MFVar('cons');
const tBZ = MFVar('BZ');
const tBT = MFVar('unsafeBT');
const tBTI = MFVar('BTI');
const tMakeInt = MFVar('makeInt');
const tRat = MFVar('rat');

type CMap = { [key: string]: number };

export const termToMachine = (term: Term, map: CMap = {}, level: number = 0): MTerm => {
  if (term.tag === 'Var') {
    const ix = map[term.name];
    if (typeof ix === 'number') return MBVar(level - ix - 1);
    return MFVar(term.name);
  }
  if (term.tag === 'Abs') {
    const x = patToMachine(term.pat);
    const nmap: CMap = {};
    for (let k in map) nmap[k] = map[k];
    nmap[x] = level;
    return MAbs(termToMachine(term.body, nmap, level + 1));
  }
  if (term.tag === 'App') return MApp(termToMachine(term.left, map, level), termToMachine(term.right, map, level));
  if (term.tag === 'Let') return termToMachine(App(Abs(PVar(term.name), term.body), term.val), map, level);
  if (term.tag === 'Ann') return termToMachine(term.term, map, level);
  if (term.tag === 'If')
    return termToMachine(appFrom([ tIf, term.cond, Abs(PWildcard, term.ifTrue), Abs(PWildcard, term.ifFalse) ]), map, level);
  if (term.tag === 'LitNat') {
    let n = BigInt(term.val);
    const r: MTerm[] = [];
    while (n > 0n) {
      if (n % 2n === 0n) { n /= 2n; r.push(tBT) }
      else { n = (n - 1n) / 2n; r.push(tBTI) }
    }
    let c: MTerm = tBZ;
    for (let i = r.length - 1; i >= 0; i--) c = MApp(r[i], c);
    return c;
  }
  if (term.tag === 'LitInt') {
    let t = termToMachine(LitNat(term.val), map, level);
    return term.neg ? MApp(MApp(tMakeInt, tBZ), t) : MApp(MApp(tMakeInt, t), tBZ);
  }
  if (term.tag === 'LitRat') {
    const a = termToMachine(LitInt(term.val1, term.neg), map, level);
    const b = termToMachine(LitNat(term.val2), map, level);
    return MApp(MApp(tRat, a), b);
  }
  if (term.tag === 'LitChar') {
    const n = term.val.charCodeAt(0);
    return termToMachine(LitNat(`${n}`), map, level);
  }
  if (term.tag === 'LitStr') {
    const val = term.val;
    let c: MTerm = tNil;
    for (let i = val.length - 1; i >= 0; i--)
      c = MApp(MApp(tCons, termToMachine(LitChar(val[i]), map, level)), c);
    return c;
  }
  return impossible('termToMachine');
};

export const reduceTerm = (genv: GEnv, term: Term): MClos => {
  const mterm = termToMachine(term);
  return reduce(genv, mterm);
};
export const reduceDefs = (global: GEnv, defs: Def[]): void => {
  for (let i = 0, l = defs.length; i < l; i++) {
    const d = defs[i];
    if (d.tag === 'DType') {
      global[d.name] = makeClos(MAbs(MBVar(0)), LNil);
    } else if (d.tag === 'DLet') {
      const n = d.name;
      const t = abs(d.args, d.term);
      const v = reduceTerm(global, t);
      global[n] = v;
    }
  }
};
