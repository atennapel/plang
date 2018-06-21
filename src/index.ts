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
import { compile, compileProgram } from './compilerJS';
import { infer, ktype, initialContext, inferProgram } from './typechecker';
import {
  Context,
  ctcon,
  cvar,
} from './context';
import {
  kcon,
  kfuns,
} from './kinds';
import {
  Definition,
  DValue,
  DData,
} from './definitions';
import { isErr, isOk } from './Result';
import { parse } from './parser';
import { ppKind, ppType, ppContextElem, ppContext } from './prettyprinter';

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

  ctcon('Sum', kfuns(ktype, ktype, ktype)),
  cvar('inl', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('a'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  cvar('inr', tforalls([['a', ktype], ['b', ktype]], tfuns(tvar('b'), tapps(tcon('Sum'), tvar('a'), tvar('b'))))),
  */
);

const p: Definition[] = [
  new DData('Unit', [], [['Unit', []]]),
  new DData('Void', [], []),
];
const i = inferProgram(ctx, p);
if(isErr(i)) console.log(''+i.err);
else if(isOk(i)) {
  const val = i.val;
  console.log(ppContext(val));
}
console.log(compileProgram(p));

/**
 * TODO:
 *  repl tutorial command
 *  prelude
 *  functor/foldable/cata generation
 *  ADT codata check
 *  row polymorphism
 *  tfun as a type constructor
 *  repl: save/load/clear commands
 *  parser: f \x -> x
 *  pretty printer expr
 *  repl: let with arguments
 * PROBLEM:
 *  repl types not showing properly (Fix NilF)
 *  repl texists in :context bug
 */
