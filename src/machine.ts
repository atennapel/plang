import { Name, impossible } from './util';
import { Term, Pat, abs, Var } from './terms';
import List from './List';
import { Def } from './definitions';

type MTerm = MVar | MAbs | MApp | MConst | MAdd;

export interface MVar {
  readonly tag: 'MVar';
  readonly name: Name;
}
export const MVar = (name: Name): MVar => ({ tag: 'MVar', name });

export interface MApp {
  readonly tag: 'MApp';
  readonly left: MTerm;
  readonly right: MTerm;
}
export const MApp = (left: MTerm, right: MTerm): MApp =>
  ({ tag: 'MApp', left, right });
export const mappFrom = (ts: MTerm[]): MTerm =>
  ts.reduce(MApp);
export function mapp(...ts: MTerm[]) { return mappFrom(ts) };

export interface MAbs {
  readonly tag: 'MAbs';
  readonly name: Name;
  readonly body: MTerm;
}
export const MAbs = (name: Name, body: MTerm): MAbs =>
  ({ tag: 'MAbs', name, body });
export const mabs = (ns: Name[], body: MTerm) =>
  ns.reduceRight((x, y) => MAbs(y, x), body);

export interface MConst {
  readonly tag: 'MConst';
  readonly val: number;
}
export const MConst = (val: number): MConst =>
  ({ tag: 'MConst', val });

export interface MAdd {
  readonly tag: 'MAdd';
  readonly left: MTerm;
  readonly right: MTerm;
}
export const MAdd = (left: MTerm, right: MTerm): MAdd =>
  ({ tag: 'MAdd', left, right });

export const showMTerm = (term: MTerm): string => {
  if (term.tag === 'MVar') return term.name;
  if (term.tag === 'MConst') return JSON.stringify(term.val);
  if (term.tag === 'MAbs') return `(\\${term.name} -> ${showMTerm(term.body)})`;
  if (term.tag === 'MApp') return `(${showMTerm(term.left)} ${showMTerm(term.right)})`;
  if (term.tag === 'MAdd') return `(${showMTerm(term.left)} + ${showMTerm(term.right)})`;
  return impossible('showMTerm');
};

type Free = { [key: string]: boolean };
const freeMTerm = (term: MTerm, fr: Free = {}): Free => {
  if (term.tag === 'MVar') { fr[term.name] = true; return fr }
  if (term.tag === 'MAbs') { freeMTerm(term.body, fr); fr[term.name] = false; return fr }
  if (term.tag === 'MApp') { freeMTerm(term.left, fr); return freeMTerm(term.right, fr) }
  if (term.tag === 'MAdd') { freeMTerm(term.left, fr); return freeMTerm(term.right, fr) }
  return fr;
};

export const patToMachine = (pat: Pat): Name => {
  if (pat.tag === 'PWildcard') return '_';
  if (pat.tag === 'PVar') return pat.name;
  if (pat.tag === 'PAnn') return patToMachine(pat.pat);
  if (pat.tag === 'PCon') return patToMachine(pat.pat);
  return impossible('patToMachine');
};

export const termToMachine = (term: Term): MTerm => {
  if (term.tag === 'Var') return MVar(term.name);
  if (term.tag === 'Abs') return MAbs(patToMachine(term.pat), termToMachine(term.body));
  if (term.tag === 'App') return MApp(termToMachine(term.left), termToMachine(term.right));
  if (term.tag === 'Let') return MApp(MAbs(term.name, termToMachine(term.body)), termToMachine(term.val));
  if (term.tag === 'Ann') return termToMachine(term.term);
  return impossible('termToMachine');
};

export type Val = Clos | VConst;
export interface Clos { readonly tag: 'Clos', readonly abs: MAbs; readonly env: Env }
export const Clos = (abs: MAbs, env: Env): Clos => ({ tag: 'Clos', abs, env });
interface VConst { readonly tag: 'VConst', readonly val: number }
const VConst = (val: number): VConst => ({ tag: 'VConst', val });
export const showVal = (v: Val): string =>
  v.tag === 'VConst' ? `${v.val}` : `Clos(${showMTerm(v.abs)}, ${showEnv(v.env)})`;

export type Env = List<[Name, Val]>;
const extend = (env: Env, k: Name, v: Val): Env =>
  List.cons([k, v], env);
const lookup = (env: Env, k: Name): Val | null => {
  const r = env.first(([k2, _]) => k === k2);
  if (r) return r[1];
  return null;
};
export const showEnv = (env: Env): string => env.toString(([k, v]) => `${k} = ${showVal(v)}`);

type Frame = FFun | FArg | FFunAdd | FArgAdd;
interface FArg { readonly tag: 'FArg'; readonly term: MTerm; readonly env: Env }
const FArg = (term: MTerm, env: Env): FArg => ({ tag: 'FArg', term, env });
interface FFun { readonly tag: 'FFun'; readonly fn: Clos }
const FFun = (fn: Clos): FFun => ({ tag: 'FFun', fn });
interface FArgAdd { readonly tag: 'FArgAdd'; readonly term: MTerm; readonly env: Env }
const FArgAdd = (term: MTerm, env: Env): FArgAdd => ({ tag: 'FArgAdd', term, env });
interface FFunAdd { readonly tag: 'FFunAdd'; readonly val: number }
const FFunAdd = (val: number): FFunAdd => ({ tag: 'FFunAdd', val });
const showFrame = (f: Frame): string => {
  if (f.tag === 'FFun') return `FFun(${showVal(f.fn)})`;
  if (f.tag === 'FArg') return `FArg(${showMTerm(f.term)}, ${showEnv(f.env)})`;
  if (f.tag === 'FFunAdd') return `FFun(${f.val})`;
  if (f.tag === 'FArgAdd') return `FArg(${showMTerm(f.term)}, ${showEnv(f.env)})`;
  return impossible('showFrame');
};

interface State {
  readonly term: MTerm;
  readonly env: Env;
  readonly stack: List<Frame>;
}
export const State = (term: MTerm, env: Env = List.nil(), stack: List<Frame> = List.nil()): State =>
  ({ term, env, stack });
export const showState = (s: State): string =>
  `State(${showMTerm(s.term)}, ${showEnv(s.env)}, ${s.stack.toString(showFrame)})`;

const makeClos = (term: MAbs, env: Env): Clos => {
  const f = freeMTerm(term);
  const nenv = env.filter(([x, _]) => f[x]);
  return Clos(term, nenv);
};

const step = (state: State): State | null => {
  const { term, env, stack } = state;
  if (term.tag === 'MVar') {
    const v = lookup(env, term.name);
    if (!v) return null;
    if (v.tag === 'VConst') return State(MConst(v.val), env, stack);
    return State(v.abs, v.env, stack); 
  }
  if (term.tag === 'MApp')
    return State(term.left, env, List.cons(FArg(term.right, env), stack));
  if (term.tag === 'MAdd')
    return State(term.left, env, List.cons(FArgAdd(term.right, env), stack));
  if (stack.isNonEmpty()) {
    const top = stack.head();
    const tail = stack.tail();
    if (term.tag === 'MAbs' && top.tag === 'FArg')
      return State(top.term, top.env, List.cons(FFun(makeClos(term, env)), tail));
    if (term.tag === 'MAbs' && top.tag === 'FFun') {
      const abs = top.fn.abs;
      return State(abs.body, extend(top.fn.env, abs.name, makeClos(term, env)), tail);
    }
    if (term.tag === 'MConst' && top.tag === 'FFun') {
      const abs = top.fn.abs;
      return State(abs.body, extend(top.fn.env, abs.name, VConst(term.val)), tail);
    }
    if (term.tag === 'MConst' && top.tag === 'FArgAdd')
      return State(top.term, top.env, List.cons(FFunAdd(term.val), tail));
    if (term.tag === 'MConst' && top.tag === 'FFunAdd')
      return State(MConst(top.val + term.val), env, tail);
  }
  return null;
};
export const steps = (state: State): State => {
  let c = state;
  while (true) {
    // console.log(showState(c));
    const next = step(c);
    if (!next) return c;
    c = next;
  }
};
export const runState = (term: Term, env: Env = List.nil()): State =>
  steps(State(termToMachine(term), env));
export const runVal = (term: Term, env: Env = List.nil()): Val => {
  const st = runState(term, env);
  const t = st.term;
  return t.tag === 'MConst' ? VConst(t.val) : Clos(t as MAbs, st.env);
};
export const runEnv = (defs: Def[], env_: Env = List.nil()): Env => {
  let env: Env = env_;
  for (let i = 0, l = defs.length; i < l; i++) {
    const d = defs[i];
    if (d.tag === 'DType') {
      env = List.cons([d.name, Clos(MAbs('x', MVar('x')), List.nil())], env); 
    } else if (d.tag === 'DLet') {
      const n = d.name;
      const t = abs(d.args, d.term);
      const v = runVal(t, env);
      env = List.cons([n, v], env);
    }
  }
  return env;
};

// testing
/*
const v = MVar;
const z = mabs(['f', 'x'], v('x'));
const s = mabs(['n', 'f', 'x'], mapp(v('f'), mapp(v('n'), v('f'), v('x'))));
const inc = mabs(['x'], MAdd(v('x'), MConst(1)));
const st = mapp(mapp(s, z), inc, MConst(0));
steps(State(st));
*/
