import {
  tvar,
  tfuns,
  tforalls,
} from './types';
import { compile, compileProgram, compileDefinition, compileConstructor, compileCase, compileCata, compilePara } from './compilerJS';
import { infer, ktype, tstr, tfloat, initialContext, inferDefinition, inferProgram } from './typechecker';
import {
  cvar,
} from './context';
import { parse, parseDefinition, parseProgram } from './parser';
import { ppType, ppContextElem } from './prettyprinter';
import {
  DData,
  DValue,
} from './definitions';

export const _context = initialContext.add(
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

function _show(x: any): string {
  if(x._rec) {
    if(x.length === 0) return '{}';
    const r = [];
    for(let i = x.length - 1; i >= 0; i--)
      r.push(`${x[i][0]} = ${_show(x[i][1])}`);
    return `{ ${r.join(', ')} }`;
  }
  if(x._eff) {
    if(x.tag === 'ret') return `!(${_show(x.val)})`;
    if(x.tag === 'cont') return `!(${x.op} ${_show(x.val)})`;
  }
  if(x._var) {
    let l = '';
    for(let i = 0; i < x.level; i++) l += '^';
    return `(${l}${x.label} ${_show(x.val)})`;
  }
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
      return '[' + r.map(_show).join(', ') + ']';
    }
    return x._args.length === 0? `${x._tag}`: `(${x._tag}${x._args.length > 0? ` ${x._args.map(_show).join(' ')}`: ''})`;
  }
  if(typeof x === 'function') return `[Function]`;
  if(typeof x === 'string') return JSON.stringify(x);
  return `${x}`;
}

let _ctx = _context;
export default function _run(i: string, cb: (output: string, err?: boolean) => void): void {
  const cmd = i.trim().toLowerCase();
  if(cmd === ':help') {
    cb('commands :help :context :def :prelude');
  } else if(cmd === ':prelude') {
    try {
      const ds = parseProgram(eval('_prelude'));
      const t = inferProgram(_ctx, ds);
      const c = compileProgram(t.defs, false, '', true);
      console.log(c);
      eval(c);
      _ctx = t.ctx;
      cb('prelude loaded');
    } catch(err) {
      return cb(''+err, true);
    }
  } else if(cmd === ':context') {
    cb(_ctx.elems.map(ppContextElem).join('\n'));
  } else if(cmd.slice(0, 4) === ':def') {
    const rest = i.slice(4).trim();
    try {
      const d_ = parseDefinition(rest);
      const t = inferDefinition(_ctx, d_);
      _ctx = t.ctx;
      const d = t.def;
      const res = eval(compileDefinition(d, true));
      if(d instanceof DValue)
        cb(`${d.name} : ${ppType(_ctx.apply(_ctx.findVar(d.name) as any))} = ${_show(res)}`);
      else if(d instanceof DData)
        cb(`defined ${d.name}`);
      else return cb('unknown definition', true);
    } catch(err) {
      return cb(''+err, true);
    }
  } else {
    try {
      const p = parse(i);
      console.log(''+p);
      const tr = infer(_ctx, p);
      console.log(ppType(tr.ty));
      console.log(''+tr.expr);
      const c = compile(tr.expr);
      console.log(c);
      const res = eval(c);
      cb(`${_show(res)} : ${ppType(tr.ty)}`);
    } catch(e) {
      cb(''+e, true);
    }
  }
}
