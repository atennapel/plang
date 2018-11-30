import Expr, { isVar, isAbs, isApp, isLet, isAnno, isHandler, HandlerCase, isHOp, isHReturn } from './exprs';
import { impossible } from './utils';

const compileHandlerToJS = (e: HandlerCase): string => {
  if (isHOp(e)) return `${e.op}: ${e.x} => ${e.k} => ${compileToJS(e.expr)}, ${compileHandlerToJS(e.rest)}`;
  if (isHReturn(e)) return `return: ${e.x} => ${compileToJS(e.expr)}`;
  return impossible();
}

const compileToJS = (e: Expr): string => {
  if (isVar(e)) return `${e.name}`;
  if (isAbs(e)) return `(${e.name} => ${compileToJS(e.body)})`;
  if (isAnno(e)) return compileToJS(e.expr);
  if (isApp(e)) return `$(${compileToJS(e.left)}, ${compileToJS(e.right)})`;
  if (isLet(e)) return `_do(${compileToJS(e.expr)}, ${e.name} => ${compileToJS(e.body)})`;
  if (isHandler(e)) return `_newhandler({${compileHandlerToJS(e.cases)}})`;
  return impossible();
};

export default compileToJS;
