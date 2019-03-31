import { Name, impossible } from './util';
import { Type, showTy } from './types';

export type Term
  = Var
  | App
  | Abs
  | Let
  | Ann;

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

export interface Abs {
  readonly tag: 'Abs';
  readonly name: Name;
  readonly body: Term;
}
export const Abs = (name: Name, body: Term): Abs =>
  ({ tag: 'Abs', name, body });

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

export const showTerm = (t: Term): string => {
  if (t.tag === 'Var') return t.name;
  if (t.tag === 'Abs')
    return `(\\${t.name} -> ${showTerm(t.body)})`;
  if (t.tag === 'App')
    return `(${showTerm(t.left)} ${showTerm(t.right)})`;
  if (t.tag === 'Ann')
    return `(${showTerm(t.term)} : ${showTy(t.type)})`;
  if (t.tag === 'Let')
    return `(let ${t.name} = ${showTerm(t.val)} in ${showTerm(t.body)})`;
  return impossible('showTerm');
};
