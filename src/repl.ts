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

const lib = {
  unit: `null`,
  void: `(() => { throw new Error('void') })`,
  z: `0`,
  s: `(val => val + 1)`,
  true: `true`,
  false: `false`,
  if: `(c => a => b => c ? a : b)`,
  pair: `(a => b => [a, b])`,
  fst: `(p => p[0])`,
  snd: `(p => p[1])`,
  nil: `[]`,
  cons: `(h => t => [h].concat(t))`,
  singleton: `(x => [x])`,
};

const ctx = initialContext.add(
  ctcon('Unit', ktype),
  ctcon('Void', ktype),
  cvar('unit', tcon('Unit')),
  cvar('void', tforalls([['t', ktype]], tfuns(tcon('Void'), tvar('t')))),
  
  ctcon('Nat', ktype),
  cvar('z', tcon('Nat')),
  cvar('s', tfuns(tcon('Nat'), tcon('Nat'))),

  ctcon('Bool', ktype),
  cvar('true', tcon('Bool')),
  cvar('false', tcon('Bool')),
  cvar('if', tforalls([['t', ktype]], tfuns(tcon('Bool'), tvar('t'), tvar('t'), tvar('t')))),

  ctcon('Pair', kfuns(ktype, ktype, ktype)),
  cvar('pair', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('a'), tvar('b'), tapps(tcon('Pair'), tvar('a'), tvar('b'))))),
  cvar('fst', tforalls([['a', ktype], ['b', ktype]], tfuns(tapps(tcon('Pair'), tvar('a'), tvar('b')), tvar('a')))),
  cvar('snd', tforalls([['a', ktype], ['b', ktype]], tfuns(tapps(tcon('Pair'), tvar('a'), tvar('b')), tvar('b')))),

  ctcon('List', kfuns(ktype, ktype)),
  cvar('nil', tforalls([['a', ktype]], tapps(tcon('List'), tvar('a')))),
  cvar('cons', tforalls([['a', ktype]], tfuns(tvar('a'), tapps(tcon('List'), tvar('a')), tapps(tcon('List'), tvar('a'))))),
  cvar('singleton', tforalls([['a', ktype]], tfuns(tvar('a'), tapps(tcon('List'), tvar('a'))))),
);

function show(x: any): string {
  if(x === null) return `()`;
  if(Array.isArray(x)) return `(${show(x[0])}, ${show(x[1])})`;
  if(typeof x === 'function') return `[Function]`;
  return `${x}`;
}

export default function run(i: string, cb: (output: string, err?: boolean) => void): void {
  try {
    const p = parse(i);
    const tr = infer(ctx, p);
    if(isErr(tr)) throw tr.err;
    else if(isOk(tr)) {
      const c = compile(p, lib);
      const res = eval(c);
      cb(`${show(res)} : ${tr.val.ty}`);
    }
  } catch(e) {
    cb(''+e, true);
  }
}
