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
	trecord,
	tvariant,
	teff,
	scheme,
	trowextend,
	trowempty,
	tstring,
	tnumber,
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
	erecordextend,
	erecordrestrict,
	evariantinject,
	evariantembed,
	evariantelim,
} from './exprs';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import InferState from './InferState';
import parse from './parser';

const a = id('a', 0);
const b = id('b', 0);
const ta = tvar(a, ktype);
const tb = tvar(b, ktype);

const env = Env.of(
	['zero', tnumber.generalize()],
	['str', tstring.generalize()],
	['inc', tarrs(tnumber, tnumber).generalize()],
	['choose', scheme(TVarSet.of(ta), [], tarrs(ta, ta, ta))],
	['numStr', tarrs(tnumber, tstring).generalize()],
);

const t = tapp(trecord, trowextend('a', ta, trowextend('show', tarrs(ta, tstring), trowempty)));

const expr = eanno(eapp(erecordextend('a'), evar('zero'), eapp(erecordextend('show'), evar('numStr'), erecordempty)), t);
console.log(''+expr);
console.log(''+expr.runInfer(env, new InferState(new IdStore({ a: a.next(), b: b.next() }))));
