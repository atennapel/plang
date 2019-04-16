import { Name, impossible } from './util';
import { Type, showTy } from './types';

export type Term
  = Var
  | App
  | Abs
  | Let
  | Ann
  | If
  | LitNat
  | LitChar
  | LitStr
  | Hole;

export interface Var {
  readonly tag: 'Var';
  readonly name: Name;
}
export const Var = (name: Name): Var => ({ tag: 'Var', name });

export interface App {
  readonly tag: 'App';
  readonly left: Term;
  readonly right: Term;
}
export const App = (left: Term, right: Term): App =>
  ({ tag: 'App', left, right });
export const appFrom = (ts: Term[]): Term =>
  ts.reduce(App);

export interface Abs {
  readonly tag: 'Abs';
  readonly pat: Pat;
  readonly body: Term;
}
export const Abs = (pat: Pat, body: Term): Abs =>
  ({ tag: 'Abs', pat, body });
export const abs = (ns: Pat[], body: Term) =>
  ns.reduceRight((x, y) => Abs(y, x), body);

export interface Let {
  readonly tag: 'Let';
  readonly name: Name;
  readonly val: Term;
  readonly body: Term;
}
export const Let = (name: Name, val: Term, body: Term): Let =>
  ({ tag: 'Let', name, val, body });

export interface Ann {
  readonly tag: 'Ann';
  readonly term: Term;
  readonly type: Type;
}
export const Ann = (term: Term, type: Type): Ann =>
  ({ tag: 'Ann', term, type });

export interface If {
  readonly tag: 'If';
  readonly cond: Term;
  readonly ifTrue: Term;
  readonly ifFalse: Term;
}
export const If = (cond: Term, ifTrue: Term, ifFalse: Term): If =>
  ({ tag: 'If', cond, ifTrue, ifFalse });

export interface LitNat {
  readonly tag: 'LitNat';
  readonly val: number;
}
export const LitNat = (val: number): LitNat =>
  ({ tag: 'LitNat', val });

export interface LitChar {
  readonly tag: 'LitChar';
  readonly val: string;
}
export const LitChar = (val: string): LitChar =>
  ({ tag: 'LitChar', val });

export interface LitStr {
  readonly tag: 'LitStr';
  readonly val: string;
}
export const LitStr = (val: string): LitStr =>
  ({ tag: 'LitStr', val });

export interface Hole {
  readonly tag: 'Hole';
  readonly name: string;
}
export const Hole = (name: string): Hole =>
  ({ tag: 'Hole', name });

export type Pat
  = PVar
  | PWildcard
  | PAnn
  | PCon;

export interface PVar {
  readonly tag: 'PVar';
  readonly name: Name;
}
export const PVar = (name: Name): PVar => ({ tag: 'PVar', name });

export interface PWildcard {
  readonly tag: 'PWildcard';
}
export const PWildcard: PWildcard = ({ tag: 'PWildcard' });

export interface PAnn {
  readonly tag: 'PAnn';
  readonly pat: Pat;
  readonly type: Type;
}
export const PAnn = (pat: Pat, type: Type): PAnn =>
  ({ tag: 'PAnn', pat, type });

export interface PCon {
  readonly tag: 'PCon';
  readonly name: Name;
  readonly pat: Pat;
}
export const PCon = (name: Name, pat: Pat): PCon =>
  ({ tag: 'PCon', name, pat });

export const showPat = (p: Pat): string => {
  if (p.tag === 'PVar') return p.name;
  if (p.tag === 'PWildcard') return '_';
  if (p.tag === 'PAnn')
    return `(${showPat(p.pat)} : ${showTy(p.type)})`;
  if (p.tag === 'PCon')
    return `(${p.name} ${showPat(p.pat)})`;
  return impossible('showPat');
};

export const showTerm = (t: Term): string => {
  if (t.tag === 'Var') return t.name;
  if (t.tag === 'Abs')
    return `(\\${showPat(t.pat)} -> ${showTerm(t.body)})`;
  if (t.tag === 'App')
    return `(${showTerm(t.left)} ${showTerm(t.right)})`;
  if (t.tag === 'Ann')
    return `(${showTerm(t.term)} : ${showTy(t.type)})`;
  if (t.tag === 'Let')
    return `(let ${t.name} = ${showTerm(t.val)} in ${showTerm(t.body)})`;
  if (t.tag === 'If')
    return `(if ${showTerm(t.cond)} then ${showTerm(t.ifTrue)} else ${showTerm(t.ifFalse)})`;
  if (t.tag === 'LitNat')
    return `${t.val}`;
  if (t.tag === 'LitChar')
    return `'${JSON.stringify(t.val).slice(1, -1)}'`;
  if (t.tag === 'LitStr')
    return JSON.stringify(t.val);
  if (t.tag === 'Hole')
    return `_${t.name}`;
  return impossible('showTerm');
};
