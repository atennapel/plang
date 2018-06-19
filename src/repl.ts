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
import compile from './compilerJS';
import { infer, ktype, initialContext } from './typechecker';
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
import { parse } from './parser';

export const lib = {
  unit: `null`,
  void: `(() => { throw new Error('void') })`,
  z: `0`,
  s: `(val => val + 1)`,
  rec: `(fz => fs => n => (function rec(fz, fs, n) { return n === 0? fz: fs(rec(fz, fs, n - 1))(n - 1) })(fz,fs,n))`,
  true: `true`,
  false: `false`,
  if: `(c => a => b => c ? a : b)`,
  pair: `(a => b => ({_tag: 'pair', _fst:a, _snd:b}))`,
  fst: `(p => p._fst)`,
  snd: `(p => p._snd)`,
  nil: `[]`,
  cons: `(h => t => [h].concat(t))`,
  inl: `(v => ({ _tag: 'inl', _val: v }))`,
  inr: `(v => ({ _tag: 'inr', _val: v }))`,
  case: `(fa => fb => x => x._tag === 'inl'? fa(x._val): fb(x._val))`,
  fold: `(fnil => fcons => l => l.reduceRight((a, b) => fcons(a)(b), fnil))`,
};

export const context = initialContext.add(
  ctcon('Unit', ktype),
  ctcon('Void', ktype),
  cvar('unit', tcon('Unit')),
  cvar('void', tforalls([['t', ktype]], tfuns(tcon('Void'), tvar('t')))),
  
  ctcon('Nat', ktype),
  cvar('z', tcon('Nat')),
  cvar('s', tfuns(tcon('Nat'), tcon('Nat'))),
  cvar('rec', tforalls([['r', ktype]], tfuns(tvar('r'), tfuns(tvar('r'), tcon('Nat'), tvar('r')), tcon('Nat'), tvar('r')))),

  ctcon('Bool', ktype),
  cvar('true', tcon('Bool')),
  cvar('false', tcon('Bool')),
  cvar('if', tforalls([['t', ktype]], tfuns(tcon('Bool'), tvar('t'), tvar('t'), tvar('t')))),

  ctcon('Pair', kfuns(ktype, ktype, ktype)),
  cvar('pair', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('a'), tvar('b'), tapps(tcon('Pair'), tvar('a'), tvar('b'))))),
  cvar('fst', tforalls([['a', ktype], ['b', ktype]], tfuns(tapps(tcon('Pair'), tvar('a'), tvar('b')), tvar('a')))),
  cvar('snd', tforalls([['a', ktype], ['b', ktype]], tfuns(tapps(tcon('Pair'), tvar('a'), tvar('b')), tvar('b')))),

  ctcon('Sum', kfuns(ktype, ktype, ktype)),
  cvar('inl', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('a'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  cvar('inr', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('b'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  cvar('case', tforalls([['a', ktype], ['b', ktype], ['c', ktype]], tfuns(tfuns(tvar('a'), tvar('c')), tfuns(tvar('b'), tvar('c')), tapps(tcon('Sum'), tvar('a'), tvar('b')), tvar('c')))),

  ctcon('List', kfuns(ktype, ktype)),
  cvar('nil', tforalls([['a', ktype]], tapps(tcon('List'), tvar('a')))),
  cvar('cons', tforalls([['a', ktype]], tfuns(tvar('a'), tapps(tcon('List'), tvar('a')), tapps(tcon('List'), tvar('a'))))),
  cvar('fold', tforalls([['t', ktype], ['r', ktype]], tfuns(tvar('r'), tfuns(tvar('r'), tvar('t'), tvar('r')), tapps(tcon('List'), tvar('t')), tvar('r')))),
);

function show(x: any): string {
  if(x === null) return `()`;
  if(Array.isArray(x)) return `[${x.map(show).join(', ')}]`;
  if(typeof x === 'function') return `[Function]`;
  if(x._tag === 'inl') return `Inl ${show(x._val)}`;
  if(x._tag === 'inr') return `Inr ${show(x._val)}`;
  if(x._tag === 'pair') return `(${show(x._fst)}, ${x._snd})`;
  return `${x}`;
}

let ctx = context;
export default function run(i: string, cb: (output: string, err?: boolean) => void): void {
  const cmd = i.trim().toLowerCase();
  if(cmd === ':help') {
    cb('commands :help :context :let');
  } else if(cmd === ':context') {
    cb(ctx.elems.join('\n'));
  } else if(cmd.slice(0, 4) === ':let') {
    const rest = i.slice(4).trim();
    const j = rest.indexOf('=');
    if(j < 0) return cb('= not found', true);
    const spl = rest.split('=');
    const name = spl[0].trim();
    if(name.length === 0 || !/[a-z][a-zA-Z0-9]*/.test(name)) return cb('invalid name', true);
    const expr = spl[1].trim();
    if(expr.length === 0) return cb('invalid expression', true);
    try {
      const p = parse(expr);
      console.log(''+p);
      const tr = infer(ctx, p);
      if(isErr(tr)) throw tr.err;
      else if(isOk(tr)) {
        const c = compile(p, lib);
        console.log(c);
        const res = eval(`(typeof global === 'undefined'? window: global)['${name}'] = ${c}`);
        ctx = ctx.add(cvar(name, tr.val.ty));
        cb(`${name} : ${tr.val.ty} = ${show(res)}`);
      }
    } catch(e) {
      cb(''+e, true);
    };
  } else {
    try {
      const p = parse(i);
      console.log(''+p);
      const tr = infer(ctx, p);
      if(isErr(tr)) throw tr.err;
      else if(isOk(tr)) {
        const c = compile(p, lib);
        console.log(c);
        const res = eval(c);
        cb(`${show(res)} : ${tr.val.ty}`);
      }
    } catch(e) {
      cb(''+e, true);
    }
  }
}
