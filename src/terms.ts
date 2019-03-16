import { NameT, showName, eqName } from './names';
import { Type, showType } from './types';

export type Term
  = Var
  | Abs
  | App
  | Ann;

export type TermTag = Term['tag'];

export interface Var {
  readonly tag: 'Var';
  readonly name: NameT;
}
export const Var = (name: NameT): Var => ({ tag: 'Var', name });
export const isVar = (term: Term): term is Var => term.tag === 'Var';

export interface Abs {
  readonly tag: 'Abs';
  readonly name: NameT;
  readonly body: Term;
}
export const Abs = (name: NameT, body: Term): Abs => ({ tag: 'Abs', name, body });
export const isAbs = (term: Term): term is Abs => term.tag === 'Abs';
export const abs = (ns: NameT[], body: Term): Term =>
  ns.reduceRight((t, n) => Abs(n, t), body);

export interface App {
  readonly tag: 'App';
  readonly left: Term;
  readonly right: Term;
}
export const App = (left: Term, right: Term): App => ({ tag: 'App', left, right });
export const isApp = (term: Term): term is App => term.tag === 'App';
export const appFrom = (ts: Term[]): Term => ts.reduce(App);
export const app = (...ts: Term[]): Term => appFrom(ts);

export interface Ann {
  readonly tag: 'Ann';
  readonly term: Term;
  readonly type: Type;
}
export const Ann = (term: Term, type: Type): Ann => ({ tag: 'Ann', term, type });
export const isAnn = (term: Term): term is Ann => term.tag === 'Ann';

export const flattenApp = (type: Term): Term[] => {
  let c = type;
  const r: Term[] = [];
  while (isApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};
export const flattenAbs = (type: Term): { args: NameT[], body: Term } => {
  let c = type;
  const args: NameT[] = [];
  while (isAbs(c)) {
    args.push(c.name);
    c = c.body;
  }
  return { args, body: c };
};
export const showTerm = (term: Term): string => {
  switch (term.tag) {
    case 'Var': return showName(term.name);
    case 'App':
      return flattenApp(term)
        .map(t => {
          const s = showTerm(t);
          return isApp(t) || isAbs(t) || isAnn(t) ? `(${s})` : s;
        })
        .join(' ');
    case 'Abs': {
      const f = flattenAbs(term);
      const args = f.args.map(showName).join(' ');
      return `\\${args} -> ${showTerm(f.body)}`;
    }
    case 'Ann': return `${showTerm(term.term)} : ${showType(term.type)}`;
  }
};

const substVar = (x: NameT, s: Term, term: Term): Term => {
  switch (term.tag) {
    case 'Var': return eqName(x, term.name) ? s : term;
    case 'Abs': {
      if (eqName(x, term.name)) return term;
      const body = substVar(x, s, term.body);
      return term.body === body ? term : Abs(term.name, body);
    }
    case 'App': {
      const left = substVar(x, s, term.left);
      const right = substVar(x, s, term.right);
      return term.left === left && term.right === right ? term : App(left, right);
    }
    case 'Ann': {
      const body = substVar(x, s, term.term);
      return term.term === body ? term : Ann(body, term.type);
    }
  }
};
export const openAbs = (a: Abs, s: Term): Term =>
  substVar(a.name, s, a.body);
