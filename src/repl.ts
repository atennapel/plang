import {
  tvar,
  tex,
  tfuns,
  tforalls,
  tcon,
  tapps,
} from './types';
import {
  evar,
  eapps,
  eabss,
  eanno,
  etapps,
  etabss,
} from './exprs';
import { compile, compileProgram, compileConstructor, compileCase, compileCata, compilePara } from './compilerJS';
import { infer, ktype, tstr, tfloat, initialContext, inferDefinition, inferProgram } from './typechecker';
import {
  Context,
  ctcon,
  cvar,
} from './context';
import {
  kcon,
  kfuns,
} from './kinds';
import { parse, parseDefinition, parseProgram } from './parser';
import { ppType, ppContextElem } from './prettyprinter';
import {
  DData,
  DValue,
} from './definitions';

export const context = initialContext.add(
  cvar('show', tforalls([['t', ktype]], tfuns(tvar('t'), tstr))),
  cvar('emptyStr', tstr),
  cvar('appendStr', tfuns(tstr, tstr, tstr)),

  cvar('zeroFloat', tfloat),
  cvar('oneFloat', tfloat),
  cvar('negFloat', tfuns(tfloat, tfloat)),
  cvar('incFloat', tfuns(tfloat, tfloat)),
  cvar('decFloat', tfuns(tfloat, tfloat)),
  cvar('addFloat', tfuns(tfloat, tfloat, tfloat)),
  cvar('subFloat', tfuns(tfloat, tfloat, tfloat)),
  cvar('mulFloat', tfuns(tfloat, tfloat, tfloat)),
  cvar('divFloat', tfuns(tfloat, tfloat, tfloat)),
  cvar('modFloat', tfuns(tfloat, tfloat, tfloat)),
);

function show(x: any): string {
  if(x._adt) {
    if(x._tag === 'Z') return '0';
    if(x._tag === 'S') {
      let c = x;
      let n = 0;
      while(c._tag === 'S') {
        n++;
        c = c._args[0];
      }
      return `${n}`;
    }
    if(x._tag === 'Nil') return '[]';
    if(x._tag === 'Cons') {
      let c = x;
      let r = [];
      while(c._tag === 'Cons') {
        r.push(c._args[0]);
        c = c._args[1];
      }
      return '[' + r.map(show).join(', ') + ']';
    }
    return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  }
  if(typeof x === 'function') return `[Function]`;
  if(typeof x === 'string') return JSON.stringify(x);
  return `${x}`;
}

let ctx = context;
export default function run(i: string, cb: (output: string, err?: boolean) => void): void {
  const cmd = i.trim().toLowerCase();
  if(cmd === ':help') {
    cb('commands :help :context :def :prelude');
  } else if(cmd === ':prelude') {
    try {
      const ds = parseProgram(eval('_prelude'));
      const t = inferProgram(ctx, ds);
      eval(compileProgram(ds, false, '', true));
      ctx = t;
      cb('prelude loaded');
    } catch(err) {
      return cb(''+err, true);
    }
  } else if(cmd === ':context') {
    cb(ctx.elems.map(ppContextElem).join('\n'));
  } else if(cmd.slice(0, 4) === ':def') {
    const rest = i.slice(4).trim();
    try {
      const d = parseDefinition(rest);
      const t = inferDefinition(ctx, d);
      ctx = t;
      if(d instanceof DValue) {
        const c = compile(d.val);
        console.log(c);
        const res = eval(`(typeof global === 'undefined'? window: global)['${d.name}'] = ${c}`);
        cb(`${d.name} : ${ppType(ctx.apply(ctx.findVar(d.name) as any))} = ${show(res)}`);
      } else if(d instanceof DData) {
        d.constrs.forEach(([n, ts]) => eval(`(typeof global === 'undefined'? window: global)['${n}'] = ${compileConstructor(n, ts.length)}`));
        eval(`(typeof global === 'undefined'? window: global)['case${d.name}'] = ${compileCase(d.name, d.constrs)}`);
        eval(`(typeof global === 'undefined'? window: global)['cata${d.name}'] = ${compileCata(d.name, d.constrs, d.getType())}`);
        eval(`(typeof global === 'undefined'? window: global)['para${d.name}'] = ${compilePara(d.name, d.constrs, d.getType())}`);
        cb(`defined ${d.name}`);
      } else return cb('unknown definition', true);
    } catch(err) {
      return cb(''+err, true);
    }
  } else {
    try {
      const p = parse(i);
      console.log(''+p);
      const tr = infer(ctx, p);
      const c = compile(p);
      console.log(c);
      const res = eval(c);
      cb(`${show(res)} : ${ppType(tr.ty)}`);
    } catch(e) {
      cb(''+e, true);
    }
  }
}
