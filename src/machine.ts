import { Name, impossible } from './util';
import { Term, Pat, abs } from './terms';
import List from './List';
import { Def } from './definitions';

export type MTerm = MVar | MAbs | MApp | MAtom | MPair | MPairC;

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

export interface MAtom {
  readonly tag: 'MAtom';
  readonly val: string;
}
export const MAtom = (val: string): MAtom =>
  ({ tag: 'MAtom', val });

export interface MPair {
  readonly tag: 'MPair';
  readonly left: Val;
  readonly right: Val;
}
export const MPair = (left: Val, right: Val): MPair =>
  ({ tag: 'MPair', left, right });

export interface MPairC {
  readonly tag: 'MPairC';
  readonly left: MTerm;
  readonly right: MTerm;
}
export const MPairC = (left: MTerm, right: MTerm): MPairC =>
  ({ tag: 'MPairC', left, right });

export const showMTerm = (term: MTerm): string => {
  if (term.tag === 'MVar') return term.name;
  if (term.tag === 'MAtom') return term.val;
  if (term.tag === 'MAbs') return `(\\${term.name} -> ${showMTerm(term.body)})`;
  if (term.tag === 'MApp') return `(${showMTerm(term.left)} ${showMTerm(term.right)})`;
  if (term.tag === 'MPair') return `(${showVal(term.left)}, ${showVal(term.right)})`;
  if (term.tag === 'MPairC') return `(${showMTerm(term.left)}, ${showMTerm(term.right)})`;
  return impossible('showMTerm');
};

type Free = { [key: string]: boolean };
const freeMTerm = (term: MTerm, fr: Free = {}): Free => {
  if (term.tag === 'MVar') { fr[term.name] = true; return fr }
  if (term.tag === 'MAbs') { freeMTerm(term.body, fr); fr[term.name] = false; return fr }
  if (term.tag === 'MApp') { freeMTerm(term.left, fr); return freeMTerm(term.right, fr) }
  if (term.tag === 'MPairC') { freeMTerm(term.left, fr); return freeMTerm(term.right, fr) }
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
  if (term.tag === 'If')
    return MApp(MApp(MApp(MVar('if'), termToMachine(term.cond)), MAbs('_', termToMachine(term.ifTrue))), MAbs('_', termToMachine(term.ifFalse)));
  return impossible('termToMachine');
};

export type Val = Clos | VAtom | VPair;
export interface Clos { readonly tag: 'Clos', readonly abs: MAbs; readonly env: Env }
export const Clos = (abs: MAbs, env: Env): Clos => ({ tag: 'Clos', abs, env });
export interface VAtom { readonly tag: 'VAtom', readonly val: string }
export const VAtom = (val: string): VAtom => ({ tag: 'VAtom', val });
export interface VPair { readonly tag: 'VPair', readonly left: Val, readonly right: Val }
export const VPair = (left: Val, right: Val): VPair => ({ tag: 'VPair', left, right });
export const showVal = (v: Val): string =>
  v.tag === 'VAtom' ? v.val :
  v.tag === 'VPair' ? `(${showVal(v.left)}, ${showVal(v.right)})` :
  `Clos(${showMTerm(v.abs)}, ${showEnv(v.env)})`;

export type Env = List<[Name, Val]>;
const extend = (env: Env, k: Name, v: Val): Env =>
  List.cons([k, v], env);
const lookup = (env: Env, k: Name): Val | null => {
  const r = env.first(([k2, _]) => k === k2);
  if (r) return r[1];
  return null;
};
export const showEnv = (env: Env): string => env.toString(([k, v]) => `${k} = ${showVal(v)}`);

type Frame = FFun | FArg | FFst | FSnd;
interface FArg { readonly tag: 'FArg'; readonly term: MTerm; readonly env: Env }
const FArg = (term: MTerm, env: Env): FArg => ({ tag: 'FArg', term, env });
interface FFun { readonly tag: 'FFun'; readonly abs: MAbs; readonly env: Env }
const FFun = (abs: MAbs, env: Env): FFun => ({ tag: 'FFun', abs, env });
interface FFst { readonly tag: 'FFst'; readonly term: MTerm; readonly env: Env }
const FFst = (term: MTerm, env: Env): FFst => ({ tag: 'FFst', term, env });
interface FSnd { readonly tag: 'FSnd'; readonly val: Val }
const FSnd = (val: Val): FSnd => ({ tag: 'FSnd', val });
const showFrame = (f: Frame): string => {
  if (f.tag === 'FFun') return `FFun(${showMTerm(f.abs)}, ${showEnv(f.env)})`;
  if (f.tag === 'FArg') return `FArg(${showMTerm(f.term)}, ${showEnv(f.env)})`;
  if (f.tag === 'FFst') return `FFst(${showMTerm(f.term)}, ${showEnv(f.env)})`;
  if (f.tag === 'FSnd') return `FSnd(${showVal(f.val)})`;
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
    if (v.tag === 'Clos') return State(v.abs, v.env, stack);
    return State(v.tag === 'VAtom' ? MAtom(v.val) : MPair(v.left, v.right), env, stack); 
  }
  if (term.tag === 'MApp')
    return State(term.left, env, List.cons(FArg(term.right, env), stack));
  if (term.tag === 'MPairC')
    return State(term.left, env, List.cons(FFst(term.right, env), stack));
  if (!stack.isNonEmpty()) return null;
  const top = stack.head();
  const tail = stack.tail();
  if (term.tag === 'MAbs') {
    if (top.tag === 'FArg')
      return State(top.term, top.env, List.cons(FFun(term, env), tail));
    if (top.tag === 'FFun') {
      const { name, body } = top.abs;
      return State(body, extend(top.env, name, makeClos(term, env)), tail);
    }
    if (top.tag === 'FFst')
      return State(top.term, top.env, List.cons(FSnd(makeClos(term, env)), tail));
    if (top.tag === 'FSnd')
      return State(MPair(top.val, makeClos(term, env)), env, tail);
  }
  if (term.tag === 'MAtom') {
    if (top.tag === 'FArg') return null;
    if (top.tag === 'FFun') {
      const { name, body } = top.abs;
      return State(body, extend(top.env, name, VAtom(term.val)), tail);
    }
    if (top.tag === 'FFst')
      return State(top.term, top.env, List.cons(FSnd(VAtom(term.val)), tail));
    if (top.tag === 'FSnd')
      return State(MPair(top.val, VAtom(term.val)), env, tail);
  }
  if (term.tag === 'MPair') {
    if (top.tag === 'FArg') return null;
    if (top.tag === 'FFun') {
      const { name, body } = top.abs;
      return State(body, extend(top.env, name, VPair(term.left, term.right)), tail);
    }
    if (top.tag === 'FFst')
      return State(top.term, top.env, List.cons(FSnd(VPair(term.left, term.right)), tail));
    if (top.tag === 'FSnd')
      return State(MPair(top.val, VPair(term.left, term.right)), env, tail);
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
export const stepsVal = (state: State): Val => {
  const st = steps(state);
  const t = st.term;
  return t.tag === 'MAtom' ? VAtom(t.val) :
    t.tag === 'MPair' ? VPair(t.left, t.right) :
    Clos(t as MAbs, st.env);
};
export const runState = (term: Term, env: Env = List.nil()): State =>
  steps(State(termToMachine(term), env));
export const runVal = (term: Term, env: Env = List.nil()): Val => {
  const st = runState(term, env);
  const t = st.term;
  return t.tag === 'MAtom' ? VAtom(t.val) :
    t.tag === 'MPair' ? VPair(t.left, t.right) :
    Clos(t as MAbs, st.env);
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

/*
// testing
const v = MVar;
const z = mabs(['f', 'x'], v('x'));
const s = mabs(['n', 'f', 'x'], mapp(v('f'), mapp(v('n'), v('f'), v('x'))));
const inc = mabs(['x'], MPairC(MAtom('S'), v('x')));
const st = mapp(mapp(s, mapp(s, z)), inc, MAtom('Z'));
steps(State(st));
*/

