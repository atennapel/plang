import { Name } from './names';

export type Expr = Var | Abs | App;

export interface Var {
  readonly tag: 'Var';
  readonly name: Name;
};
export const Var = (name: Name): Expr =>
  ({ tag: 'Var', name });

export interface Abs {
  readonly tag: 'Abs';
  readonly arg: Name;
  readonly body: Expr;
};
export const Abs = (arg: Name, body: Expr): Expr =>
  ({ tag: 'Abs', arg, body });
export const abs = (ns: Name[], body: Expr): Expr =>
  ns.reduceRight((a, b) => Abs(b, a), body);

export interface App {
  readonly tag: 'App';
  readonly left: Expr;
  readonly right: Expr;
};
export const App = (left: Expr, right: Expr): Expr =>
  ({ tag: 'App', left, right });
export const app = (...es: Expr[]): Expr => es.reduce(App);
export const apps = (fn: Expr, args: Expr[]): Expr =>
  [fn].concat(args).reduce(App);

export type CasesExpr<R> = {
  Var: (name: Name) => R;
  Abs: (arg: Name, body: Expr) => R;
  App: (left: Expr, right: Expr) => R;
};
export const caseExpr = <R>(val: Expr, cs: CasesExpr<R>): R => {
  switch (val.tag) {
    case 'Var': return cs.Var(val.name);
    case 'Abs': return cs.Abs(val.arg, val.body);
    case 'App': return cs.App(val.left, val.right);
  }
};

export const showExpr = (expr: Expr): string => caseExpr(expr, {
  Var: name => `${name}`,
  Abs: (arg, body) => `(\\${arg} -> ${showExpr(body)})`,
  App: (left, right) => `(${showExpr(left)} ${showExpr(right)})`,
});

export const Let = (x: Name, a: Expr, b: Expr) => App(Abs(x, b), a);
export const lets = (xs: [Name, Expr][], b: Expr) =>
  apps(abs(xs.map(x => x[0]), b), xs.map(x => x[1]));
