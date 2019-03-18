import { NameT, showName, eqName } from './names';
import { Type, showType } from './types';

export type Term
  = Var
  | Abs
  | App
  | Ann
  | Let
  | If
  | Query;

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

export interface Let {
  readonly tag: 'Let';
  readonly name: NameT;
  readonly term: Term;
  readonly body: Term;
}
export const Let = (name: NameT, term: Term, body: Term): Let =>
  ({ tag: 'Let', name, term, body });
export const isLet = (term: Term): term is Let => term.tag === 'Let';

export interface If {
  readonly tag: 'If';
  readonly cond: Term;
  readonly then: Term;
  readonly else_: Term;
}
export const If = (cond: Term, then: Term, else_: Term): If =>
  ({ tag: 'If', cond, then, else_ });
export const isIf = (term: Term): term is If => term.tag === 'If';

export interface Query {
  readonly tag: 'Query';
}
export const Query: Query = { tag: 'Query' };
export const isQuery = (term: Term): term is Query => term.tag === 'Query';

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
    case 'Let':
      return `(let ${showName(term.name)} = ${showTerm(term.term)} in ${showTerm(term.body)})`;
    case 'If':
      return `(if ${showTerm(term.cond)} then ${showTerm(term.then)} else ${showTerm(term.else_)})`;
    case 'Query': return '?';
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
    case 'Let': {
      const val = substVar(x, s, term.term);
      const body = eqName(x, term.name) ? term.body : substVar(x, s, term.body);
      return term.term === val && term.body === body ? term : Let(term.name, val, body);
    }
    case 'If': {
      const cond = substVar(x, s, term.cond);
      const then = substVar(x, s, term.then);
      const else_ = substVar(x, s, term.else_);
      return If(cond, then, else_);
    }
    case 'Query': return term;
  }
};
export const openAbs = (a: Abs, s: Term): Term =>
  substVar(a.name, s, a.body);
export const openLet = (a: Let, s: Term): Term =>
  substVar(a.name, s, a.body);