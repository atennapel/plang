import { Name } from './names';
import { Type, showType } from './types';

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
  readonly type: Type | null;
  readonly body: Expr;
};
export const Abs = (arg: Name, type: Type | null, body: Expr): Expr =>
  ({ tag: 'Abs', arg, type, body });
export const abs = (ns: Name[], body: Expr): Expr =>
  ns.reduceRight((a, b) => Abs(b, null, a), body);
export const abst = (ns: [Name, Type][], body: Expr): Expr =>
  ns.reduceRight((a, [b, t]) => Abs(b, t, a), body);

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

export interface Handler {
  readonly tag: 'Handler';
  readonly handler: HandlerCase;
}
export const Handler = (handler: HandlerCase): Expr =>
  ({ tag: 'Handler', handler });

export type HandlerCase = HOp | HReturn;

export interface HOp {
  tag: 'HOp';
  op: Name;
  expr: Expr;
  rest: HandlerCase;
}
export const HOp = (op: Name, expr: Expr, rest: HandlerCase): HandlerCase =>
  ({ tag: 'HOp', op, expr, rest });

export interface HReturn {
  tag: 'HReturn';
  expr: Expr;
}
export const HReturn = (expr: Expr): HandlerCase =>
  ({ tag: 'HReturn', expr });

export type CasesHandler<R> = {
  HOp: (op: Name, expr: Expr, rest: HandlerCase) => R;
  HReturn: (expr: Expr) => R;
};
export const caseHandler = <R>(val: HandlerCase, cs: CasesHandler<R>): R => {
  switch (val.tag) {
    case 'HOp': return cs.HOp(val.op, val.expr, val.rest);
    case 'HReturn': return cs.HReturn(val.expr);
  }
};

export const showHandler = (val: HandlerCase): string => caseHandler(val, {
  HOp: (op, expr) => `${op} -> ${showExpr(expr)}`,
  HReturn: (expr) => `return -> ${showExpr(expr)}`,
});

export type CasesExpr<R> = {
  Var: (name: Name) => R;
  Abs: (arg: Name, type: Type | null, body: Expr) => R;
  App: (left: Expr, right: Expr) => R;
  Handler: (handler: HandlerCase) => R;
};
export const caseExpr = <R>(val: Expr, cs: CasesExpr<R>): R => {
  switch (val.tag) {
    case 'Var': return cs.Var(val.name);
    case 'Abs': return cs.Abs(val.arg, val.type, val.body);
    case 'App': return cs.App(val.left, val.right);
    case 'Handler': return cs.Handler(val.handler);
  }
};

export const showExpr = (expr: Expr): string => caseExpr(expr, {
  Var: name => `${name}`,
  Abs: (arg, type, body) =>
    type ? `(\\(${arg} : ${showType(type)}) -> ${showExpr(body)})` :
    `(\\${arg} -> ${showExpr(body)})`,
  App: (left, right) => `(${showExpr(left)} ${showExpr(right)})`,
  Handler: (handler) => {
    const r = [];
    let c = handler;
    while (c.tag === 'HOp') {
      r.push([c.op, showExpr(c.expr)]);
      c = c.rest;
    }
    r.push(['return', showExpr(c.expr)]);
    return `(handler {${r.map(([x, e]) => `${x} -> ${e}`).join(', ')}})`;
  },
});

export const Let = (x: Name, a: Expr, b: Expr) => App(Abs(x, null, b), a);
export const letTy = (x: Name, t: Type, a: Expr, b: Expr) => App(Abs(x, t, b), a);
export const lets = (xs: [Name, Expr][], b: Expr) =>
  apps(abs(xs.map(x => x[0]), b), xs.map(x => x[1]));
export const letTys = (xs: [Name, Type, Expr][], b: Expr) =>
  apps(abst(xs.map(x => [x[0], x[1]] as [Name, Type]), b), xs.map(x => x[2]));

export const Anno = (expr: Expr, type: Type) => App(Abs('x', type, Var('x')), expr);
