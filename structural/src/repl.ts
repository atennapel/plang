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
	trowempty,
} from './types';
import {
	Kind,
	ktype,
	krow,
	karr,
} from './kinds';
import { id } from './Id';

const Int = tcon('Int', ktype);
const Float = tcon('Float', ktype);
const Str = tcon('Str', ktype);
const P = tcon('P', karr(ktype, ktype, ktype));

const a = id('a', 0);
const b = id('b', 0);
const ta = tvar(a, ktype);
const tb = tvar(b, ktype);

const env = Env.of(
	['zero', Int.generalize()],
	['zerof', Float.generalize()],
	['str', Str.generalize()],
	['inc', tarrs(Int, Int).generalize()],
	['+', tarrs(Int, Int, Int).generalize()],
	['fst', scheme(TVarSet.of(ta, tb), [], tarrs(tapp(P, ta, tb), ta))],
	['snd', scheme(TVarSet.of(ta, tb), [], tarrs(tapp(P, ta, tb), tb))],
	['choose', scheme(TVarSet.of(ta), [], tarrs(ta, ta, ta))],
	['obj', tapp(trecord, trowextend('x', Int, trowextend('y', Float, trowempty))).generalize()],
	['end', scheme(TVarSet.of(ta), [], tarrs(tapp(tvariant, trowempty), ta))],
);

const state = new InferState(new IdStore({ a: a.next(), b: b.next() }));

function output(i: string) {
	const res = parse(i).then(e => {
		console.log(e.toString());
		return e.runInfer(env, state).map(([t, k]) => [e, t, k] as [Expr, Type, Kind])
	});
	if(res instanceof Ok) {
		const [e, t, k] = res.val;
		console.log(t.toString());
		console.log(k.toString());
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
