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
import { ppKind, ppType } from './prettyprinter';

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
  /*
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
  */
  ctcon('Sum', kfuns(ktype, ktype, ktype)),
  cvar('inl', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('a'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  cvar('inr', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('b'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  
);

console.log(ppType(tforalls([['t', ktype], ['r', ktype]],
  tfuns(tvar('r'), tfuns(tvar('r'), tvar('t'), tvar('r')), tapps(tcon('List'), tvar('t')), tforalls([['x', kfuns(ktype, ktype)]], tvar('r')), tvar('r')))));

/*
const s = `
  \\v -> inr v
`;
const e = parse(s);
console.log(''+e);
const i = infer(ctx, e);
if(isErr(i)) console.log(''+i.err);
else if(isOk(i)) {
  const val = i.val;
  console.log(''+val.ty);
  //console.log(''+val.ctx);
}
const c = compile(e)
console.log(c);
try {
  const e = eval(c);
  console.log(e);
} catch(e) {
  console.log(''+e);
}
*/

/**
 * TODO:
 *  pretty printer tapp
 *  pretty print context
 *  repl: duplicate lets
 *  row polymorphism
 *  parser: f \x -> x
 *  repl: let with arguments
 *  parser: $
 *  tfun as a type constructor
 *  ADT (data/codata)
 *  positivity check
 *  functor/foldable/cata generation
 *  pretty printer expr
 */
