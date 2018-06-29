import { impossible } from './util';

import {
  Expr,
  EVar,
  EApp,
  EAbs,
  ELit,
  EEmpty,
  ESelect,
  EExtend,
  ERestrict,
  ERecUpdate,
  EVarEmpty,
  EInject,
  EEmbed,
  ECase,
  EVarUpdate,
} from './exprs';
import {
  Definition,
  DValue,
  DData,
} from './definitions';
import { Type } from './types';

export function compile(expr: Expr): string {
  if(expr instanceof EVar) return `${expr.name}`;
  if(expr instanceof EApp) return `${compile(expr.left)}(${compile(expr.right)})`;
  if(expr instanceof EAbs) return `(${expr.name} => ${compile(expr.expr)})`;
  if(expr instanceof ELit) return typeof expr.val === 'string'? JSON.stringify(expr.val): `${expr.val}`;

  if(expr instanceof EEmpty) return `_recEmpty`;
  if(expr instanceof ESelect) return `_recSelect(${JSON.stringify(expr.label)})`;
  if(expr instanceof EExtend) return `_recExtend(${JSON.stringify(expr.label)})`;
  if(expr instanceof ERestrict) return `_recRestrict(${JSON.stringify(expr.label)})`;
  if(expr instanceof ERecUpdate) return `_recUpdate(${JSON.stringify(expr.label)})`;

  if(expr instanceof EVarEmpty) return `_varEmpty`;
  if(expr instanceof EInject) return `_varInject(${JSON.stringify(expr.label)})`;
  if(expr instanceof EEmbed) return `_varEmbed(${JSON.stringify(expr.label)})`;
  if(expr instanceof ECase) return `_varCase(${JSON.stringify(expr.label)})`;
  if(expr instanceof EVarUpdate) return `_varUpdate(${JSON.stringify(expr.label)})`;

  return impossible();
}

function varPrefix(name: string, attachVars?: boolean) {
  return attachVars? `(typeof global === 'undefined'? window: global)['${name}']`: `const ${name}`;
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
    `case '${cn}':return f${cn}${ts.map((_, i) => `(x._args[${i}])`).join('')};break;`).join('')}}throw new Error('case failed for ${n}')}`;
}

export function compileCata(n: string, c: [string, any[]][], rtype: Type) {
  const a: string[] = [];
  for(let i = 0; i < c.length; i++) a.push(`f${c[i][0]}`);
  return `${a.join('=>')}${a.length === 0? '': '=>'}case${n}${c.map(([cn, ts]) => `(${ts.length === 0? `f${cn}`: ts.map((_, i) => `x${i}`).join('=>') + '=>' + `f${cn}` + ts.map((t, i) => t.equals(rtype)? `(cata${n}${a.map(x => `(${x})`).join('')}(x${i}))`: `(x${i})`).join('')})`).join('')}`;
}

export function compilePara(n: string, c: [string, any[]][], rtype: Type) {
  const a: string[] = [];
  for(let i = 0; i < c.length; i++) a.push(`f${c[i][0]}`);
  return `${a.join('=>')}${a.length === 0? '': '=>'}case${n}${c.map(([cn, ts]) =>
      `(${ts.length === 0? `f${cn}`: ts.map((_, i) => `x${i}`).join('=>') + '=>' + `f${cn}` +
        ts.map((t, i) => t.equals(rtype)? `(x${i})(para${n}${a.map(x => `(${x})`).join('')}(x${i}))`: `(x${i})`).join('')})`).join('')}`;
}

function compileDefinition(d: Definition, attachVars?: boolean): string {
  if(d instanceof DValue)
    return `${varPrefix(d.name, attachVars)} = ${compile(d.val)}`;
  if(d instanceof DData)
    return d.constrs.map(([n, ts]) => `${varPrefix(n, attachVars)} = ${compileConstructor(n, ts.length)}`).join(';') + ';' +
      `${varPrefix(`case${d.name}`, attachVars)} = ${compileCase(d.name, d.constrs)};` +
      `${varPrefix(`cata${d.name}`, attachVars)} = ${compileCata(d.name, d.constrs, d.getType())};` +
      `${varPrefix(`para${d.name}`, attachVars)} = ${compilePara(d.name, d.constrs, d.getType())};`;
  return impossible();
}

export function compileProgram(p: Definition[], withMain?: boolean, lib: string = '', attachVars?: boolean): string {
  return `;${lib.trim()};${p.map(d => compileDefinition(d, attachVars)).join(';')}${withMain? `;console.log(show(main))`: ''};`;
}
