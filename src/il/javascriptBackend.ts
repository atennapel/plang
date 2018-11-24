import Expr from './exprs';
import { isVar, isAbs, isAbsT, isAnno } from './values';
import { impossible } from '../utils';
import { isReturn, isApp, isAppT, isLet } from './computations';

const compileToJS = (e: Expr): string => {
  if (isVar(e)) return `${e.name}`;
  if (isAbs(e)) return `(${e.name} => ${compileToJS(e.body)})`;
  if (isAbsT(e)) return compileToJS(e.body);
  if (isAnno(e)) return compileToJS(e.expr);

  if (isReturn(e)) return compileToJS(e.val);
  if (isApp(e)) return `${compileToJS(e.left)}(${compileToJS(e.right)})`;
  if (isAppT(e)) return compileToJS(e.left);
  if (isLet(e)) return `(${e.name} => ${compileToJS(e.body)})(${compileToJS(e.expr)})`;

  return impossible();
};

export default compileToJS;
