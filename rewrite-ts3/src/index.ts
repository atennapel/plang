import { id } from './Id';
import {
	KType,
	KRow,
	karr,
} from './kinds';
import {
	tvar,
	tcon,
	tapp,
	tarr,
	tscheme,
	tschemeM,
} from './types';
import {
	Expr,
	evar,
	elam,
	eapp,
	elet,
	eletr,
	eanno,

	erecordempty,
	eselect,
	eextend,
	erestrict,
	einject,
	eembed,
	eelim,
	eend,
} from './exprs';
import Label, { label } from './Label';
import Map from './Map';
import Set from './Set';
import { Env } from './typechecker';

const Int = tcon('Int');
const Bool = tcon('Bool');
const List = tcon('List', karr(KType, KType));

const t = tvar(id('_t'));
const a = tvar(id('_a'));
const b = tvar(id('_b'));
const c = tvar(id('_c'));
const env: Env = Map.of(
	[evar('True'), tschemeM(Bool)],
	[evar('inc'), tschemeM(tarr(Int, Int))],
	[evar('if'), tscheme([t], tarr(t, t, t))],
	[evar('one'), tschemeM(Int)],
	[evar('singleton'), tscheme([t], tarr(t, tapp(List, t)))],
);

const unit = evar('unit');
const one = evar('one');
const True = evar('True');

function inj(l: string, x: Expr) { return A(einject(label(l)), x) }

const A = eapp;
const L = elam;
const V = evar;

const expr = eletr('count', A(eelim(label('Nil')), L(['_'], one), A(eelim(label('Cons')), L(['r'], A(V('inc'), A(V('count'), A(eselect(label('tail')), V('r'))))), eend)), V('count'));
console.log(expr.toString());
console.log(expr.typecheck(env).toString());
