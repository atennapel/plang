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
import Map from './Map';
import Set from './Set';
import { Env } from './typechecker';

const Int = tcon('Int');
const Bool = tcon('Bool');
const List = tcon('List', karr(KType, KType));
const U = tcon('()');
const S = tcon('+', karr(KType, KType, KType));
const P = tcon('*', karr(KType, KType, KType));

const t = tvar(id('_t'));
const a = tvar(id('_a'));
const b = tvar(id('_b'));
const c = tvar(id('_c'));
const env: Env = Map.of(
	[evar('True'), tschemeM(Bool)],
	[evar('unit'), tschemeM(U)],
	[evar('inc'), tschemeM(tarr(Int, Int))],
	[evar('if'), tscheme([t], tarr(t, t, t))],
	[evar('one'), tschemeM(Int)],
	[evar('singleton'), tscheme([t], tarr(t, tapp(List, t)))],
	[evar('pair'), tscheme([a, b], tarr(a, b, tapp(P, a, b)))],
	[evar('pairE'), tscheme([a, b, c], tarr(tarr(a, b, c), tapp(P, a, b), c))],
	[evar('inl'), tscheme([a, b], tarr(a, tapp(S, a, b)))],
	[evar('inr'), tscheme([a, b], tarr(b, tapp(S, a, b)))],
	[evar('sumE'), tscheme([a, b, c], tarr(tarr(a, c), tarr(b, c), tapp(S, a, b), c))],
	[evar('unitE'), tscheme([t], tarr(t, U, t))],
);

const unit = evar('unit');
const pair = evar('pair');
const pairE = evar('pairE');
const inl = evar('inl');
const inr = evar('inr');
const sumE = evar('sumE');
const one = evar('one');
const True = evar('True');
const unitE = evar('unitE');

const A = eapp;
const L = elam;
const V = evar;

const nil = A(inl, unit);
const cons = L(['h', 't'], A(inr, A(pair, V('h'), V('t'))));
// const expr = eletr('count', A(sumE, A(unitE, one), A(pairE, L(['h', 't'], A(V('inc'), A(V('count'), V('t')))))), V('count'));
const expr = eend;
console.log(expr.toString());
console.log(expr.typecheck(env).toString());
