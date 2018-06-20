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
import {
  Definition,
  DValue,
  DData,
} from './definitions';

export function compile(expr: Expr): string {
  if(expr instanceof EVar) return `${expr.name}`;
  if(expr instanceof EApp) return `${compile(expr.left)}(${compile(expr.right)})`;
  if(expr instanceof EAbs) return `(${expr.name} => ${compile(expr.expr)})`;

  if(expr instanceof EAnno) return compile(expr.expr);
  if(expr instanceof ETApp) return compile(expr.expr);
  if(expr instanceof ETAbs) return compile(expr.expr);
  return impossible();
}

function compileDefinition(d: Definition): string {
  if(d instanceof DValue) return `${d.name} = ${compile(d.val)}`;
  if(d instanceof DData)
    return d.constrs.length === 0? `${d.name} = impossible`:
      d.constrs.map(([n, ts]) => `${n} = makeConstr('${n}', ${ts.length})`).join(';');
  return impossible();
}

export function compileProgram(p: Definition[], withMain?: boolean, lib: string = ''): string {
  return `;${lib.trim()};${p.map(compileDefinition).join(';')}${withMain? `;console.log(show(main))`: ''};`;
}
