import Expr, { isVar, isAbs, isApp, isLet, isAnno } from './exprs';
import { impossible } from './utils';

const compileToJS = (e: Expr): string => {
  if (isVar(e)) return `${e.name}`;
  if (isAbs(e)) return `(${e.name} => ${compileToJS(e.body)})`;
  if (isAnno(e)) return compileToJS(e.expr);
  if (isApp(e)) return `$(${compileToJS(e.left)}, ${compileToJS(e.right)})`;
  if (isLet(e)) return `_do(${compileToJS(e.expr)}, ${e.name} => ${compileToJS(e.body)})`;
  return impossible();
};

export default compileToJS;
