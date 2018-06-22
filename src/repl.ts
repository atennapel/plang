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
import { compile, compileProgram, compileConstructor, compileCase } from './compilerJS';
import { infer, ktype, initialContext, inferDefinition, inferProgram } from './typechecker';
import {
  Context,
  ctcon,
  cvar,
} from './context';
import {
  kcon,
  kfuns,
} from './kinds';
import { isErr, isOk } from './Result';
import { parse, parseDefinition, parseProgram } from './parser';
import { ppType, ppContextElem } from './prettyprinter';
import {
  DData,
  DValue,
} from './definitions';

export const context = initialContext;

function show(x: any): string {
  if(x === null) return `()`;
  if(x._adt) return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(show).join(' ')}`: ''})`;
  if(Array.isArray(x)) return `[${x.map(show).join(', ')}]`;
  if(typeof x === 'function') return `[Function]`;
  if(x._tag === 'inl') return `(Inl ${show(x._val)})`;
  if(x._tag === 'inr') return `(Inr ${show(x._val)})`;
  if(x._tag === 'pair') return `(${show(x._fst)}, ${show(x._snd)})`;
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
      if(isErr(t)) throw t.err;
      else if(isOk(t)) ctx = t.val;
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
      if(isErr(t)) throw t.err;
      else if(isOk(t)) {
        ctx = t.val;
        if(d instanceof DValue) {
          const c = compile(d.val);
          console.log(c);
          const res = eval(`(typeof global === 'undefined'? window: global)['${d.name}'] = ${c}`);
          cb(`${d.name} : ${ppType(ctx.apply(ctx.findVar(d.name) as any))} = ${show(res)}`);
        } else if(d instanceof DData) {
          d.constrs.forEach(([n, ts]) => eval(`(typeof global === 'undefined'? window: global)['${n}'] = ${compileConstructor(n, ts.length)}`));
          eval(`(typeof global === 'undefined'? window: global)['case${d.name}'] = ${compileCase(d.name, d.constrs)}`);
          cb(`defined ${d.name}`);
        } else return cb('unknown definition', true);
      }
    } catch(err) {
      return cb(''+err, true);
    }
  } else {
    try {
      const p = parse(i);
      console.log(''+p);
      const tr = infer(ctx, p);
      if(isErr(tr)) throw tr.err;
      else if(isOk(tr)) {
        const c = compile(p);
        console.log(c);
        const res = eval(c);
        cb(`${show(res)} : ${ppType(tr.val.ty)}`);
      }
    } catch(e) {
      cb(''+e, true);
    }
  }
}
