import { Name, impossible } from './util';
import { Term, Pat } from './terms';
import List from './List';
import { substTVar } from './types';

type MTerm = MVar | MAbs | MApp;

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

export interface MAbs {
  readonly tag: 'MAbs';
  readonly name: Name;
  readonly body: MTerm;
}
export const MAbs = (name: Name, body: MTerm): MAbs =>
  ({ tag: 'MAbs', name, body });
export const mabs = (ns: Name[], body: MTerm) =>
  ns.reduceRight((x, y) => MAbs(y, x), body);

export const showMTerm = (term: MTerm): string => {
  if (term.tag === 'MVar') return term.name;
  if (term.tag === 'MAbs') return `(\\${term.name} -> ${showMTerm(term.body)})`;
  if (term.tag === 'MApp') return `(${showMTerm(term.left)} ${showMTerm(term.right)})`;
  return impossible('showMTerm');
};

type Free = { [key: string]: boolean };
const freeMTerm = (term: MTerm, fr: Free = {}): Free => {
  if (term.tag === 'MVar') { fr[term.name] = true; return fr }
  if (term.tag === 'MAbs') { freeMTerm(term.body, fr); fr[term.name] = false; return fr }
  if (term.tag === 'MApp') { freeMTerm(term.left, fr); return freeMTerm(term.right, fr) }
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

type Val = Clos;
interface Clos { readonly tag: 'Clos', readonly abs: MAbs; readonly env: Env }
const Clos = (abs: MAbs, env: Env): Clos => ({ tag: 'Clos', abs, env });
const showVal = (v: Val): string => `Clos(${showMTerm(v.abs)}, ${showEnv(v.env)})`;

type Env = List<[Name, Val]>;
const emptyEnv: Env = List.nil();
const extend = (env: Env, k: Name, v: Val): Env =>
  List.cons([k, v], env);
const lookup = (env: Env, k: Name): Val | null => {
  const r = env.first(([k2, _]) => k === k2);
  if (r) return r[1];
  return null;
};
const showEnv = (env: Env): string => env.toString(([k, v]) => `${k} = ${showVal(v)}`);

type Frame = FFun | FArg;
interface FFun { readonly tag: 'FFun'; readonly fn: Clos }
const FFun = (fn: Clos): FFun => ({ tag: 'FFun', fn });
interface FArg { readonly tag: 'FArg'; readonly term: MTerm; readonly env: Env }
const FArg = (term: MTerm, env: Env): FArg => ({ tag: 'FArg', term, env });
const showFrame = (f: Frame): string => {
  if (f.tag === 'FFun') return `FFun(${showVal(f.fn)})`;
  if (f.tag === 'FArg') return `FArg(${showMTerm(f.term)}, ${showEnv(f.env)})`;
  return impossible('showFrame');
};

interface State {
  readonly term: MTerm;
  readonly env: Env;
  readonly stack: List<Frame>;
}
const State = (term: MTerm, env: Env = List.nil(), stack: List<Frame> = List.nil()): State =>
  ({ term, env, stack });
const showState = (s: State): string =>
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
    return State(v.abs, v.env, stack); 
  }
  if (term.tag === 'MApp')
    return State(term.left, env, List.cons(FArg(term.right, env), stack));
  if (stack.isNonEmpty()) {
    const top = stack.head();
    const tail = stack.tail();
    if (term.tag === 'MAbs' && top.tag === 'FArg')
      return State(top.term, top.env, List.cons(FFun(makeClos(term, env)), tail));
    if (term.tag === 'MAbs' && top.tag === 'FFun') {
      const abs = top.fn.abs;
      return State(abs.body, extend(top.fn.env, abs.name, makeClos(term, env)), tail);
    }
  }
  return null;
};
const steps = (state: State): State => {
  let c = state;
  while (true) {
    console.log(showState(c));
    const next = step(c);
    if (!next) return c;
    c = next;
  }
};
const run = (term: MTerm) => steps(State(term));

const subst = (t: MTerm, m: { [key: string]: MTerm }): MTerm => {
  if (t.tag === 'MVar') return m[t.name] || t;
  if (t.tag === 'MAbs') return MAbs(t.name, subst(t.body, m));
  if (t.tag === 'MApp') return MApp(subst(t.left, m), subst(t.right, m));
  return impossible('subst');
};
const evalClos = (c: Clos): MTerm => {
  const m: { [key: string]: MTerm } = {};
  c.env.each(([k, v]) => m[k] = evalClos(v));
  return subst(c.abs, m);
};
const compile = (t: MTerm): string => {
  if (t.tag === 'MVar') return t.name;
  if (t.tag === 'MAbs') return `(${t.name} => ${compile(t.body)})`;
  if (t.tag === 'MApp') return `${compile(t.left)}(${compile(t.right)})`;
  return impossible('compile');
};

const z = MAbs('f', MAbs('x', MVar('x')));
const s = MAbs('n', MAbs('f', MAbs('x', MApp(MVar('f'), MApp(MApp(MVar('n'), MVar('f')), MVar('x'))))));
const t = MApp(s, MApp(s, MApp(s, MApp(s, MApp(s, z)))));
console.log(showMTerm(t));
const st = run(t);
console.log(showMTerm(st.term));
const ev = evalClos(makeClos(st.term as MAbs, st.env));
console.log(showMTerm(ev));
const co = compile(ev);
console.log(co);
const xxx = eval(compile(ev));
console.log(xxx);
console.log(xxx((x:any) => x + 1)(0));

