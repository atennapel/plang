import { impossible } from './errors';
import { Name, showName, eqName } from './names';
import { Type, showType } from './types';

export type Expr
  = Var
  | Abs
  | App
  | Anno;

export interface Var {
  readonly tag: 'Var';
  readonly name: Name;
}
export const Var = (name: Name): Expr =>
  ({ tag: 'Var', name });
export const isVar = (expr: Expr): expr is Var =>
    expr.tag === 'Var';
export const matchVar = (name: Name) => (expr: Expr): expr is Var =>
  isVar(expr) && eqName(expr.name, name);

export interface Abs {
  readonly tag: 'Abs';
  readonly arg: Name;
  readonly body: Expr;
}
export const Abs = (arg: Name, body: Expr): Abs =>
  ({ tag: 'Abs', arg, body });
export const isAbs = (expr: Expr): expr is Abs =>
  expr.tag === 'Abs';
export const abs = (ns: Name[], body: Expr): Expr =>
  ns.reduceRight((t, n) => Abs(n, t), body);

export interface App {
  readonly tag: 'App';
  readonly left: Expr;
  readonly right: Expr;
}
export const App = (left: Expr, right: Expr): App =>
  ({ tag: 'App', left, right });
export const isApp = (expr: Expr): expr is App =>
  expr.tag === 'App';
export const appFrom = (es: Expr[]): Expr =>
  es.reduce(App);
export const app = (...es: Expr[]): Expr =>
  appFrom(es);

export interface Anno {
  readonly tag: 'Anno';
  readonly expr: Expr;
  readonly type: Type;
}
export const Anno = (expr: Expr, type: Type): Anno =>
  ({ tag: 'Anno', expr, type });
export const isAnno = (expr: Expr): expr is Anno =>
  expr.tag === 'Anno';

export const showExpr = (expr: Expr): string => {
  if (isVar(expr)) return `${showName(expr.name)}`;
  if (isAbs(expr)) return `(\\${showName(expr.arg)} -> ${showExpr(expr.body)})`;
  if (isApp(expr)) return `(${showExpr(expr.left)} ${showExpr(expr.right)})`;
  if (isAnno(expr)) return `(${showExpr(expr.expr)} : ${showType(expr.type)})`;
  return impossible('showExpr');
};

export const substVar = (vr: Name, sub: Expr, expr: Expr): Expr => {
  if (isVar(expr)) return eqName(expr.name, vr) ? sub: expr;
  if (isAbs(expr)) return eqName(expr.arg, vr) ? expr : Abs(expr.arg, substVar(vr, sub, expr.body));
  if (isApp(expr)) return App(substVar(vr, sub, expr.left), substVar(vr, sub, expr.right));
  if (isAnno(expr)) return Anno(substVar(vr, sub, expr.expr), expr.type);
  return impossible('substVar');
};
export const openAbs = (abs: Abs, sub: Expr): Expr =>
  substVar(abs.arg, sub, abs.body);
