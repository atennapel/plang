import { ktype, karr_ } from './kinds';
import {
  typeStr,
  tarr_,
  tapp_,
  tvar,
  tcon,
  tscheme,
} from './types';
import {
  exprStr,
  evar,
  eapp_,
  elam_,
  elet,
  Expr,
	erecordempty,
	erecordextend,
	erecordselect,
	erecordrestrict,
	efix,
} from './exprs';
import {
  infer,
} from './typechecker';
import Env from './Env';

const Unit = tcon('Unit');
const Bool = tcon('Bool');
const Int = tcon('Int');
const Pair = tcon('Pair', karr_(ktype, ktype, ktype));
const Sum = tcon('Sum', karr_(ktype, ktype, ktype));

const a = tvar('_a', '_a0', ktype);
const b = tvar('_b', '_b0', ktype);
const c = tvar('_c', '_c0', ktype);
const env = new Env({
  one: tscheme([], Int),
  inc: tscheme([], tarr_(Int, Int)),
  True: tscheme([], Bool),
	unit: tscheme([], Unit),
  pair: tscheme([a, b], tarr_(a, b, tapp_(Pair, a, b))),
  fst: tscheme([a, b], tarr_(tapp_(Pair, a, b), a)),
  snd: tscheme([a, b], tarr_(tapp_(Pair, a, b), b)),
  inl: tscheme([a, b], tarr_(a, tapp_(Sum, a, b))),
  inr: tscheme([a, b], tarr_(b, tapp_(Sum, a, b))),
  unpair: tscheme([a, b, c], tarr_(tarr_(a, b, c), tapp_(Pair, a, b), c)),
  unsum: tscheme([a, b, c],
    tarr_(tarr_(a, c), tarr_(b, c), tapp_(Sum, a, b), c)),
  const: tscheme([a, b], tarr_(a, b, a)),
  constu: tscheme([a], tarr_(a, Unit, a)),
});
const one = evar('one');
const True = evar('True');

const A = eapp_;
const F = (e: Expr) => A(efix, e);
const L = elam_;
const V = evar;

const empty = erecordempty;
const sel = erecordselect;
const ext = erecordextend;
const restr = erecordrestrict;

/*
const e = A(F(L(['sum'],
  A(V('unsum'), A(V('constu'), one),
    L(['t'], A(V('inc'), A(V('sum'), A(V('snd'), V('t')))))))), A(evar('inl'), evar('unit')));
*/
const e = erecordselect('l');
console.log(exprStr(e));
console.log(''+infer(e, env).map(typeStr));
