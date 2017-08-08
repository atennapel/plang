import { createInterface } from 'readline';
import parse from './parser';
import { Expr } from './exprs';

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

var repl = createInterface(process.stdin, process.stdout);

console.log('REPL');
process.stdin.setEncoding('utf8');
function input() {
	repl.question('> ', function(i) {
		console.log(''+parse(i).then(e => e.runInfer(env, state)
			.map(([t, k]) => `${e} : ${t} : ${k}`)));
		setImmediate(input);
	});
};
input();
