import { impossible } from './util';

import {
  Expr,
  EUnit,
  EVar,
  EApp,
  EAbs,
  EAnno,
} from './exprs';

export default function compile(expr: Expr): string {
  if(expr instanceof EUnit) return 'null';
  if(expr instanceof EVar) return `${expr.name}`;
  if(expr instanceof EApp) return `${compile(expr.left)}(${compile(expr.right)})`;
  if(expr instanceof EAbs) return `(${expr.name} => ${compile(expr.expr)})`;
  if(expr instanceof EAnno) return compile(expr.expr);
  return impossible();
}
