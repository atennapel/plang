import { Type, showType } from './types';

export type Term<N>
  = Var<N>
  | Abs<N>
  | App<N>
  | Ann<N>;

export type TermTag = Term<any>['tag'];

export interface Var<N> {
  readonly tag: 'Var';
  readonly name: N;
}
export const Var = <N>(name: N): Var<N> => ({ tag: 'Var', name });
export const isVar = <N>(term: Term<N>): term is Var<N> => term.tag === 'Var';

export interface Abs<N> {
  readonly tag: 'Abs';
  readonly name: N;
  readonly body: Term<N>;
}
export const Abs = <N>(name: N, body: Term<N>): Abs<N> => ({ tag: 'Abs', name, body });
export const isAbs = <N>(term: Term<N>): term is Abs<N> => term.tag === 'Abs';
export const abs = <N>(ns: N[], body: Term<N>): Term<N> =>
  ns.reduceRight((t, n) => Abs(n, t), body);

export interface App<N> {
  readonly tag: 'App';
  readonly left: Term<N>;
  readonly right: Term<N>;
}
export const App = <N>(left: Term<N>, right: Term<N>): App<N> => ({ tag: 'App', left, right });
export const isApp = <N>(term: Term<N>): term is App<N> => term.tag === 'App';
export const appFrom = <N>(ts: Term<N>[]): Term<N> => ts.reduce(App);
export const app = <N>(...ts: Term<N>[]): Term<N> => appFrom(ts);

export interface Ann<N> {
  readonly tag: 'Ann';
  readonly term: Term<N>;
  readonly type: Type<N>;
}
export const Ann = <N>(term: Term<N>, type: Type<N>): Ann<N> => ({ tag: 'Ann', term, type });
export const isAnn = <N>(term: Term<N>): term is Ann<N> => term.tag === 'Ann';

export const flattenApp = <N>(type: Term<N>): Term<N>[] => {
  let c = type;
  const r: Term<N>[] = [];
  while (isApp(c)) {
    r.push(c.right);
    c = c.left;
  }
  r.push(c);
  return r.reverse();
};
export const flattenAbs = <N>(type: Term<N>): { args: N[], body: Term<N> } => {
  let c = type;
  const args: N[] = [];
  while (isAbs(c)) {
    args.push(c.name);
    c = c.body;
  }
  return { args, body: c };
};
export const showTerm = <N>(
  term: Term<N>,
  showName: (name: N) => string = n => `${n}`,
  showType_: (type: Type<N>, showName: (name: N) => string) => string = showType
): string => {
  switch (term.tag) {
    case 'Var': return showName(term.name);
    case 'App':
      return flattenApp(term)
        .map(t => {
          const s = showTerm(t, showName, showType_);
          return isApp(t) || isAbs(t) || isAnn(t) ? `(${s})` : s;
        })
        .join(' ');
    case 'Abs': {
      const f = flattenAbs(term);
      const args = f.args.map(showName).join(' ');
      return `\\${args} -> ${showTerm(f.body, showName, showType_)}`;
    }
    case 'Ann': return `${showTerm(term.term, showName, showType_)} : ${showType_(term.type, showName)}`;
  }
};

