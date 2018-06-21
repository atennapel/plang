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

export function compileConstructor(n: string, l: number) {
  const a = [];
  for(let i = 0; i < l; i++) a.push(`x${i}`);
  return `(${a.join('=>')}${a.length === 0? '': '=>'}({_adt:true,_tag:'${n}',_args:[${a.join(',')}]}))`;
}

export function compileCase(n: string, c: [string, any[]][]) {
  const a = [];
  for(let i = 0; i < c.length; i++) a.push(`f${c[i][0]}`);
  return `${a.join('=>')}${a.length === 0? '': '=>'}x=>{switch(x._tag){${c.map(([cn, ts]) =>
    `case '${cn}':return f${cn}${ts.map((_, i) => `(x.args[${i}])`)};break;`).join('')}}throw new Error('case failed for ${n}')}`;
}

function compileDefinition(d: Definition): string {
  if(d instanceof DValue) return `const ${d.name} = ${compile(d.val)}`;
  if(d instanceof DData)
    return d.constrs.map(([n, ts]) => `const ${n} = ${compileConstructor(n, ts.length)}`).join(';') + ';' + `const case${d.name} = ${compileCase(d.name, d.constrs)};`;
  return impossible();
}

export function compileProgram(p: Definition[], withMain?: boolean, lib: string = ''): string {
  return `;${lib.trim()};${p.map(compileDefinition).join(';')}${withMain? `;console.log(show(main))`: ''};`;
}
