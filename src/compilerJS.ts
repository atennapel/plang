import { impossible } from './util';

import {
  Expr,
  EVar,
  EApp,
  EAbs,
  EAnno,
  ETApp,
  ETAbs,
} from './exprs';

export default function compile(expr: Expr): string {
  if(expr instanceof EVar) return `${expr.name}`;
  if(expr instanceof EApp) return `${compile(expr.left)}(${compile(expr.right)})`;
  if(expr instanceof EAbs) return `(${expr.name} => ${compile(expr.expr)})`;

  if(expr instanceof EAnno) return compile(expr.expr);
  if(expr instanceof ETApp) return compile(expr.expr);
  if(expr instanceof ETAbs) return compile(expr.expr);
  return impossible();
}
