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

export default function compile(expr: Expr, lib: { [key: string]: string } = {}): string {
  if(expr instanceof EVar) return lib[expr.name] || `${expr.name}`;
  if(expr instanceof EApp) return `${compile(expr.left, lib)}(${compile(expr.right, lib)})`;
  if(expr instanceof EAbs) return `(${expr.name} => ${compile(expr.expr, lib)})`;

  if(expr instanceof EAnno) return compile(expr.expr, lib);
  if(expr instanceof ETApp) return compile(expr.expr, lib);
  if(expr instanceof ETAbs) return compile(expr.expr, lib);
  return impossible();
}
