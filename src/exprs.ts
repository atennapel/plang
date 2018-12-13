import { Name } from './names';

export type Expr = Var | Abs | App | Handler;

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

export type HandlerOps = { [key: string]: Expr };
export interface Handler {
  readonly tag: 'Handler';
  readonly map: HandlerOps;
  readonly value: Expr | null;
}
export const Handler = (map: HandlerOps, value: Expr | null = null): Expr =>
  ({ tag: 'Handler', map, value });

export type CasesExpr<R> = {
  Var: (name: Name) => R;
  Abs: (arg: Name, body: Expr) => R;
  App: (left: Expr, right: Expr) => R;
  Handler: (map: HandlerOps, value: Expr | null) => R;
};
export const caseExpr = <R>(val: Expr, cs: CasesExpr<R>): R => {
  switch (val.tag) {
    case 'Var': return cs.Var(val.name);
    case 'Abs': return cs.Abs(val.arg, val.body);
    case 'App': return cs.App(val.left, val.right);
    case 'Handler': return cs.Handler(val.map, val.value);
  }
};

export const showExpr = (expr: Expr): string => caseExpr(expr, {
  Var: name => `${name}`,
  Abs: (arg, body) => `(\\${arg} -> ${showExpr(body)})`,
  App: (left, right) => `(${showExpr(left)} ${showExpr(right)})`,
  Handler: (map, value) => {
    const r = [];
    for (let k in map) {
      r.push([k, showExpr(map[k])]);
    }
    if (value) r.push(['return', showExpr(value)]);
    return `(handler {${r.map(([x, e]) => `${x} -> ${e}`).join(', ')}})`;
  },
});
