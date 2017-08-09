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
	cvalue
} from './constraints';

const a = id('a', 0);
const b = id('b', 0);
const r = id('r', 0);
const ta = tvar(a, ktype);
const tb = tvar(b, ktype);
const tr = tvar(r, krow);

const env = Env.of(
	['inc', tarrs(tnumber, tnumber).generalize()],
	['+', tarrs(tnumber, tnumber, tnumber).generalize()],
	
	['choose', scheme(TVarSet.of(ta), [], tarrs(ta, ta, ta))],
	
	['end', scheme(TVarSet.of(ta), [], tarrs(tapp(tvariant, trowempty), ta))],

	['pure', scheme(TVarSet.of(ta), [cvalue(ta)], tarrs(tapp(teff, trowempty, ta), ta))],
	['return', scheme(TVarSet.of(ta, tr), [cvalue(ta)], tarrs(ta, tapp(teff, tr, ta)))],
	['finally', scheme(TVarSet.of(ta, tb, tr), [cvalue(ta), cvalue(tb)], tarrs(tarrs(ta, tapp(teff, tr, tb)), tapp(teff, tr, ta), tapp(teff, tr, tb)))],	
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
