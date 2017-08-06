import {
	ktype,
	kcon,
	karr,
	krow,
} from './kinds';
import {
	Type,
	tvar,
	tcon,
	tapp,
	tarrs,
	trow,
	scheme,
} from './types';
import {
	evar,
	elam,
	eapp,
	elet,
	eletr,
	eanno,
	erecordempty,
	erecordselect,
} from './exprs';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import InferState from './InferState';

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
	['fst', scheme(TVarSet.of(ta, tb), tarrs(tapp(P, ta, tb), ta))],
	['snd', scheme(TVarSet.of(ta, tb), tarrs(tapp(P, ta, tb), tb))],
	['choose', scheme(TVarSet.of(ta), tarrs(ta, ta, ta))],
);

const expr = eapp(erecordselect('x'), erecordempty);
console.log(''+expr);
console.log(''+expr.runInfer(env, new InferState(new IdStore({ a: a.next(), b: b.next() }))));
