import { createInterface } from 'readline';
import { readFileSync } from 'fs';
import parse from './parser';
import { Expr } from './exprs';
import { Result, Ok, Err } from './Result';
import InferState from './InferState';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import {
	Type,
	tvar,
	tcon,
	tapp,
	tarrs,
	scheme,
	trowextend,
	trecord,
	tvariant,
	teff,
	trowempty,
	tnumber,
} from './types';
import {
	Kind,
	ktype,
	krow,
	karr,
} from './kinds';
import { id } from './Id';
import {
	cvalue,
	clacks,
} from './constraints';

/**
 * TODO:
 * 	fix handle unification in recursive handlers
 */

function _inject(l: string) {return (v: any) => ({tag: l, val: v})}
function _select(l: string) {return (r: any) => r[l]}
function _extend(l: string) {return (v: any) => (r: any) => ({...r, [l]: v})}
function _restrict(l: string) {return (r: any) => {const x = {...r}; delete x[l]; return x}}
function _embed(l: string) {return (r: any) => r}
function _elim(l: string) {return (f: any) => (x: any) => (v: any) => v.tag === l? f(v.val): x(v)}
function _recordupdate(l: string) {return (f: any) => (r: any) => ({...r, [l]: f(r[l])})}
function _variantupdate(l: string) {return (f: any) => (v: any) => v.tag === l? ({tag: l, val: f(v.val)}): v}
function _effembed(l: string) {return (e: any) => e}
function end() { throw new Error('impossible') }
function add(x: number) {return (y: number) => x + y}
const inc = add(1);
function choose(a: any) {return (b: any) => a}

class Return {
	readonly val: any;
	constructor(val: any) { this.val = val }
	toString() { return 'Return(' + this.val + ')' }
}
class Cont {
	readonly label: any;
	readonly val: any;
	readonly cont: any;
	constructor(l: any, v: any, c: any) { this.label = l; this.val = v; this.cont = c }
	toString() { return 'Cont(' + this.label + ', ' + this.val + ', ...)' }
}

const ret = (v: any) => new Return(v);
const cont = (l: any, v: any, c: any) => new Cont(l, v, c);
const _perform = (l: any) => (v: any) => cont(l, v, (t: any) => (v: any) => ret(t));
const pure = (e: any) => {
	if(e instanceof Return) return e.val;
	throw new Error('invalid pure: ' + e);
};

const _do = (e: any, f: any) => {
	if(e instanceof Return) return f(e.val);
	if(e instanceof Cont) return cont(e.label, e.val, (x: any) => (v: any) => _do(e.cont(x)(v), f));
	throw new Error('invalid seq: ' + e);
}
const _handle = (m: any) => (v: any) => (e: any) => {
	if(e instanceof Return) {
		if(!m.return) return e;
		return m.return(v)(e.val);
	}
	if(e instanceof Cont) {
		if(m[e.label]) {
			return m[e.label](v)(e.val)((x: any) => (v: any) => _handle(m)(v)(e.cont(x)(v)));
		} else {
			return cont(e.label, e.val, (x: any) => (v: any) => _handle(m)(v)(e.cont(x)(v)));
		}
	}
	throw new Error('invalid handle: ' + e);
}

const handleRandom = _handle({ Random: (_: any) => (u: any) => (k: any) => k(Math.random())() })({});

const fixeff = _perform('Fix');
function fix(f: any) {return function(n: any) { return f(fix(f))(n) }}

const a = id('a', 0);
const b = id('b', 0);
const r = id('r', 0);
const ta = tvar(a, ktype);
const tb = tvar(b, ktype);
const tr = tvar(r, krow);

const tunit = tapp(trecord, trowempty);

const env = Env.of(
	['inc', tarrs(tnumber, tnumber).generalize()],
	['add', tarrs(tnumber, tnumber, tnumber).generalize()],
	
	['choose', scheme(TVarSet.of(ta), [], tarrs(ta, ta, ta))],
	
	['end', scheme(TVarSet.of(ta), [], tarrs(tapp(tvariant, trowempty), ta))],

	['pure', scheme(TVarSet.of(ta), [cvalue(ta)], tarrs(tapp(teff, trowempty, ta), ta))],
	['ret', scheme(TVarSet.of(ta, tr), [cvalue(ta)], tarrs(ta, tapp(teff, tr, ta)))],

	['fixeff', scheme(
		TVarSet.of(ta, tr),
		[cvalue(ta), clacks('Fix', tr)],
		tarrs(
			tarrs(ta, ta),
			tapp(teff, trowextend('Fix', tarrs(tarrs(ta, ta), ta), tr), ta)
		))
	],
	['fix', scheme(TVarSet.of(ta), [], tarrs(tarrs(ta, ta), ta))],

	['handleRandom', scheme(TVarSet.of(ta, tr), [clacks('Random', tr)], tarrs(tapp(teff, trowextend('Random', tarrs(tunit, tnumber), tr), ta), tapp(teff, tr, ta)))],
);

const state = new InferState(new IdStore({ a: a.next(), b: b.next(), r: r.next() }));

function output(i: string) {
	const res = parse(i).then(e => {
		console.log(e.toString());
		return e.runInfer(env, state).map(([t, k]) => [e, t, k] as [Expr, Type, Kind])
	});
	if(res instanceof Ok) {
		const [e, t, k] = res.val;
		console.log(t.toString());
		// console.log(k.toString());
		const comp = e.compile()
		console.log(comp);
		try {
			const ev = eval(comp);
			console.log(ev);
			console.log(''+ev);
		} catch(e) {
			console.log(''+e);
		}
	} else {
		console.log(res.toString());
	}
}

const clinp = process.argv[2] || null;
if(clinp) {
	output(readFileSync(clinp, {encoding: 'utf8'}));
	process.exit();
} else {
	console.log('REPL');
	var repl = createInterface(process.stdin, process.stdout);
	process.stdin.setEncoding('utf8');
	const input = function input() {
		repl.question('> ', function(i) {
			output(i);
			setImmediate(input);
		});
	};
	input();
}
