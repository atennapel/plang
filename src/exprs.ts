import { Name } from "./names";
import { err } from "./utils";
import { Type, showType } from "./types";

export type Expr = Var | App | Abs | Let | Anno | Select;

export interface Var {
  readonly tag: 'Var';
  readonly name: Name;
}
export const Var = (name: Name): Var => ({ tag: 'Var', name });
export const isVar = (expr: Expr): expr is Var => expr.tag === 'Var';

export interface App {
  readonly tag: 'App';
  readonly left: Expr;
  readonly right: Expr;
}
export const App = (left: Expr, right: Expr): App => ({ tag: 'App', left, right });
export const isApp = (expr: Expr): expr is App => expr.tag === 'App';
export const app = (...es: Expr[]): Expr => es.reduce(App);

export interface Abs {
  readonly tag: 'Abs';
  readonly name: Name;
  readonly body: Expr;
}
export const Abs = (name: Name, body: Expr): Abs => ({ tag: 'Abs', name, body });
export const isAbs = (expr: Expr): expr is Abs => expr.tag === 'Abs';
export const abs = (ns: Name[], body: Expr) => ns.reduceRight((a, b) => Abs(b, a), body);

export interface Let {
  readonly tag: 'Let';
  readonly name: Name;
  readonly val: Expr;
  readonly body: Expr;
}
export const Let = (name: Name, val: Expr, body: Expr): Let =>
  ({ tag: 'Let', name, val, body });
export const isLet = (expr: Expr): expr is Let => expr.tag === 'Let';

export interface Anno {
  readonly tag: 'Anno';
  readonly expr: Expr;
  readonly type: Type;
}
export const Anno = (expr: Expr, type: Type): Anno => ({ tag: 'Anno', expr, type });
export const isAnno = (expr: Expr): expr is Anno => expr.tag === 'Anno';

export interface Select {
  readonly tag: 'Select';
  readonly label: Name;
}
export const Select = (label: Name): Select =>
  ({ tag: 'Select', label });
export const isSelect = (expr: Expr): expr is Select => expr.tag === 'Select';

export const showExpr = (expr: Expr): string => {
  if (isVar(expr)) return expr.name;
  if (isApp(expr)) return `(${showExpr(expr.left)} ${showExpr(expr.right)})`;
  if (isAbs(expr)) return `(\\${expr.name} -> ${showExpr(expr.body)})`;
  if (isLet(expr))
    return `(let ${expr.name} = ${showExpr(expr.val)} in ${showExpr(expr.body)})`;
  if (isAnno(expr)) return `(${showExpr(expr.expr)} : ${showType(expr.type)})`;
  if (isSelect(expr)) return `.${expr.label}`;
  return err('unexpected expr in showExpr');
};
