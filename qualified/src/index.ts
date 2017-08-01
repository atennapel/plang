import {
	ktype,
	kcon,
	karr,
} from './kinds';
import {
	Type,
	tvar,
	tcon,
	tapp,
	tarrs,
	scheme,
} from './types';
import {
	evar,
	elam,
	eapp,
	elet,
	eanno,
} from './exprs';
import {
	CNumeric,
} from './constraints';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import InferState from './InferState';

const Int = tcon('Int', ktype);
const Float = tcon('Float', ktype);
const Str = tcon('Str', ktype);

const numId = id('num', 0);
const numTv = tvar(numId, ktype);

const env = Env.of(
	['zero', Int.generalize()],
	['zerof', Float.generalize()],
	['str', Str.generalize()],
	['inc', scheme(TVarSet.of(numTv), [new CNumeric(numTv)], tarrs(numTv, numTv))],
);

const expr = elet('doubleinc', elam(['x'], eapp(evar('inc'), eapp(evar('inc'), evar('x')))), eapp(evar('doubleinc'), evar('str')));
console.log(''+expr);
console.log(''+expr.runInfer(env, new InferState(new IdStore({num: numId.next()}))));
