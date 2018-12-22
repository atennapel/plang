import { Expr, isVar, isAbs, isLet, isAnno, isWithLabel, isApp, App, Abs } from "./exprs";

export const compileToJS = (expr: Expr): string => {
  if (isVar(expr)) return expr.name;
  if (isAbs(expr)) return `(${expr.name} => ${compileToJS(expr.body)})`;
  if (isApp(expr)) return `${compileToJS(expr.left)}(${compileToJS(expr.right)})`;
  if (isLet(expr)) return compileToJS(App(Abs(expr.name, expr.body), expr.val));
  if (isAnno(expr)) return compileToJS(expr.expr);
  if (isWithLabel(expr)) {
    switch (expr.type) {
      case 'Select': return `_select('${expr.label}')`;
      case 'Extend': return `_extend('${expr.label}')`;
      case 'Inject': return `_inject('${expr.label}')`;
      case 'Case': return `_case('${expr.label}')`;
    }
  }
  throw new Error('unexpected expr in compileToJS');
};
