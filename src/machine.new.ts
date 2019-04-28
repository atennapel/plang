import { Name, impossible } from './util';
import { log } from './config';

// ast
export type Ix = number;

export type MTerm
  = MBVar
  | MFVar
  | MAbs
  | MApp
  | MExec;

export interface MFVar {
  readonly tag: 'MFVar';
  readonly name: Name;
}
export const MFVar = (name: Name): MFVar => ({ tag: 'MFVar', name });

export interface MBVar {
  readonly tag: 'MBVar';
  readonly ix: Ix;
}
export const MBVar = (ix: Ix): MBVar => ({ tag: 'MBVar', ix });

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
  readonly body: MTerm;
}
export const MAbs = (body: MTerm): MAbs =>
  ({ tag: 'MAbs', body });

export interface MExec {
  readonly tag: 'MExec';
  readonly name: string;
  readonly fn: (state: MState) => boolean;
  readonly body: MTerm;
}
export const MExec = (name: string, fn: (state: MState) => boolean, body: MTerm): MExec =>
  ({ tag: 'MExec', name, fn, body });

export const showMTerm = (term: MTerm): string => {
  if (term.tag === 'MFVar') return `${term.name}`;
  if (term.tag === 'MBVar') return `${term.ix}`;
  if (term.tag === 'MAbs') return `(\\${showMTerm(term.body)})`;
  if (term.tag === 'MApp') return `(${showMTerm(term.left)} ${showMTerm(term.right)})`;
  if (term.tag === 'MExec') return `(exec ${term.name} ${showMTerm(term.body)})`;
  return impossible('showMTerm');
};

// environments
export type GEnv = { [key: string]: MClos };

export type LEnv = LNil | LCons;

export interface LNil {
  readonly tag: 'LNil';
}
export const LNil: LNil = { tag: 'LNil' };

export interface LCons {
  readonly tag: 'LCons';
  readonly head: MClos | null;
  readonly tail: LEnv;
}
export const LCons = (head: MClos | null, tail: LEnv): LCons =>
  ({ tag: 'LCons', head, tail });

const extend = (val: MClos, env: LEnv): LEnv => LCons(val, env);
const lookup = (env: LEnv, ix: Ix): MClos | null => {
  while (env.tag === 'LCons') {
    if (ix-- === 0) return env.head;
    env = env.tail;
  }
  return null;
};

export const showLEnv = (list: LEnv): string => {
  const r = [];
  while (list.tag === 'LCons') {
    r.push(list.head ? showMClos(list.head) : '_');
    list = list.tail;
  }
  return `[${r.join(', ')}]`;
};

// closures
export interface MClos {
  readonly abs: MAbs;
  readonly env: LEnv;
}
export const MClos = (abs: MAbs, env: LEnv): MClos =>
  ({ abs, env });

export const showMClos = (clos: MClos) =>
  `{${showMTerm(clos.abs)}@${showLEnv(clos.env)}}`;

type MFree = { [key: string]: boolean };
const free = (term: MTerm, fr: MFree, under: number): number => {
  if (term.tag === 'MFVar') return -1;
  if (term.tag === 'MBVar') {
    const ix = term.ix - under;
    if (ix >= 0) fr[ix] = true;
    return ix;
  }
  if (term.tag === 'MAbs') {
    const max = free(term.body, fr, under + 1);
    return max;
  }
  if (term.tag === 'MApp') {
    const a = free(term.left, fr, under);
    const b = free(term.right, fr, under);
    return Math.max(a, b);
  }
  if (term.tag === 'MExec') return free(term.body, fr, under);
  return impossible('free');
};
const makeClosEnv = (fr: MFree, max: number, env: LEnv, i: number): LEnv =>
  i > max || env.tag === 'LNil' ? LNil :
  LCons(fr[i] ? env.head : null, makeClosEnv(fr, max, env.tail, i + 1));
export const makeClos = (abs: MAbs, env: LEnv): MClos => {
  const fr: MFree = {};
  const max = free(abs, fr, 0);
  const nenv = makeClosEnv(fr, max, env, 0);
  return MClos(abs, nenv);
};

// continuations
export type MCont = MTop | MArg | MFun;

export interface MTop {
  readonly tag: 'MTop';
}
export const MTop: MTop = { tag: 'MTop' };

export interface MArg {
  readonly tag: 'MArg';
  readonly term: MTerm;
  readonly env: LEnv;
  readonly rest: MCont;
}
export const MArg = (term: MTerm, env: LEnv, rest: MCont): MArg =>
  ({ tag: 'MArg', term, env, rest });

export interface MFun {
  readonly tag: 'MFun';
  readonly body: MTerm;
  readonly env: LEnv;
  readonly rest: MCont;
}
export const MFun = (body: MTerm, env: LEnv, rest: MCont): MFun =>
  ({ tag: 'MFun', body, env, rest });

export const showMCont = (cont: MCont): string => {
  if (cont.tag === 'MTop') return 'Top';
  if (cont.tag === 'MArg') return `Arg(${showMTerm(cont.term)}, ${showLEnv(cont.env)}):${showMCont(cont.rest)}`;
  if (cont.tag === 'MFun') return `Fun(${showMTerm(cont.body)}, ${showLEnv(cont.env)}):${showMCont(cont.rest)}`;
  return impossible('showMCont');
};

// state
export interface MState {
  term: MTerm;
  env: LEnv;
  cont: MCont;
}
export const MState = (term: MTerm, env: LEnv, cont: MCont): MState =>
  ({ term, env, cont });

export const showMState = (st: MState): string =>
  `(${showMTerm(st.term)}, ${showLEnv(st.env)}, ${showMCont(st.cont)})`;

// evaluation
export const step = (genv: GEnv, state: MState): boolean => {
  const { term, env, cont } = state;
  if (term.tag === 'MFVar') {
    const v = genv[term.name];
    if (!v) return false;
    state.term = v.abs;
    state.env = v.env;
    return true;
  }
  if (term.tag === 'MBVar') {
    const v = lookup(env, term.ix);
    if (!v) return false;
    state.term = v.abs;
    state.env = v.env;
    return true;
  }
  if (term.tag === 'MApp') {
    state.term = term.left;
    state.cont = MArg(term.right, env, cont);
    return true;
  }
  if (term.tag === 'MExec') {
    state.term = term.body;
    return term.fn(state);
  }
  if (cont.tag === 'MArg') {
    state.term = cont.term;
    state.env = cont.env;
    state.cont = MFun(term.body, env, cont.rest);
    return true;
  }
  if (cont.tag === 'MFun') {
    state.term = cont.body;
    state.env = extend(MClos(term, env), cont.env);
    state.cont = cont.rest;
    return true;
  }
  return false;
};

export let stepCount = 0;
export const resetStepCount = () => { stepCount = 0 };
export const steps = (genv: GEnv, state: MState): MState => {
  log(() => showMState(state));
  while (step(genv, state)) {
    log(() => showMState(state));
    stepCount++;
  }
  return state;
};

export const initial = (term: MTerm): MState =>
  MState(term, LNil, MTop);

export const reduce = (genv: GEnv, term: MTerm): MClos => {
  const st = initial(term);
  const n = steps(genv, st);
  if (st.cont.tag !== 'MTop' || st.term.tag !== 'MAbs')
    throw new Error(`evaluation got stuck: ${showMState(st)}`);
  return makeClos(st.term, st.env);
};
