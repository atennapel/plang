import { Name, impossible } from './util';
import { Term, Pat } from './terms';
import List from './List';

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

export const patToMachine = (pat: Pat): Name => {
  if (pat.tag === 'PWildcard') return '_';
  if (pat.tag === 'PVar') return pat.name;
  if (pat.tag === 'PAnn') return patToMachine(pat.pat);
  if (pat.tag === 'PCon') return patToMachine(pat.pat);
  return impossible('patToMachine');
}

export const termToMachine = (term: Term): MTerm => {
  if (term.tag === 'Var') return MVar(term.name);
  if (term.tag === 'Abs') return MAbs(patToMachine(term.pat), termToMachine(term.body));
  if (term.tag === 'App') return MApp(termToMachine(term.left), termToMachine(term.right));
  if (term.tag === 'Let') return MApp(MAbs(term.name, termToMachine(term.body)), termToMachine(term.val));
  if (term.tag === 'Ann') return termToMachine(term.term);
  return impossible('termToMachine');
};

interface Val {
  abs: MAbs;
  env: Env;
}
const Clos = (abs: MAbs, env: Env): Val =>
  ({ abs, env });

type Env = List<[Name, Val]>;
const emptyEnv: Env = List.nil();
const extend = (env: Env, k: Name, v: Val): Env =>
  List.cons([k, v], env);
const lookup = (env: Env, k: Name): Val | null => {
  const r = env.first(([k2, _]) => k === k2);
  if (r) return r[1];
  return null;
};

type Frame = FFun | FArg;
interface FFun { tag: 'FFun'; fn: Val }
const FFun = (fn: Val): FFun => ({ tag: 'FFun', fn });
interface FArg { tag: 'FArg'; term: MTerm; env: Env }
const FArg = (term: MTerm, env: Env): FArg => ({ tag: 'FArg', term, env });

interface State {
  term: MTerm;
  env: Env;
  stack: List<Frame>;
}
const State = (term: MTerm, env: Env = List.nil(), stack: List<Frame> = List.nil()): State =>
  ({ term, env, stack });

const step = (state: State): State | null => {
  const { term, env, stack } = state;
  const top = stack.case(() => null, h => h);
  if (term.tag === 'MVar') {
    const v = lookup(env, term.name);
    return v ? State(v.abs, v.env, stack) : null; 
  }
  if (term.tag === 'MApp')
    return State(term.left, env, List.cons(FArg(term.right, env), stack));
  if (term.tag === 'MAbs' && top && top.tag === 'FArg')
    return State(top.term, top.env, List.cons(FFun(Clos(term, env)), (stack as any)._tail));
  if (term.tag === 'MAbs' && top && top.tag === 'FFun')
    return State(top.fn.abs.body, extend(top.fn.env, top.fn.abs.name, Clos(term, env)), (stack as any)._tail);
  return null;
};
const steps = (state: State): State => {
  let c = state;
  while (true) {
    console.log(c);
    const next = step(c);
    if (!next) return c;
    c = next;
  }
};
const run = (term: MTerm) => steps(State(term));

const t = MApp(MAbs('x', MAbs('y', MVar('x'))), MAbs('y', MVar('y')));
console.log(run(t));
