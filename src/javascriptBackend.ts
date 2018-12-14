import { HandlerCase, caseHandler, Expr, caseExpr } from './exprs';

const compileHandlerToJS = (e: HandlerCase): string => caseHandler(e, {
  HOp: (op, expr, rest) => `${op}: ${compileToJS(expr)}, ${compileHandlerToJS(rest)}`,
  HReturn: expr => `return: ${compileToJS(expr)}`,
});

const compileToJS = (e: Expr): string => caseExpr(e, {
  Var: name => `${name}`,
  Abs: (name, _, body) => `(${name} => ${compileToJS(body)})`,
  App: (left, right) => `$(${compileToJS(left)}, ${compileToJS(right)})`,
  Handler: cs => `_newhandler({${compileHandlerToJS(cs)}})`,
});

export default compileToJS;
