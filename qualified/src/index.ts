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
} from './types';
import {
	evar,
	elam,
	eapp,
	elet,
	eanno,
} from './exprs';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import InferState from './InferState';

const Int = tcon('Int', ktype);
const Str = tcon('Str', ktype);

const env = Env.of(
	['zero', Int.generalize()],
	['str', Str.generalize()],
	['inc', tarrs(Int, Int).generalize()],
);

console.log(''+eanno(evar('zero'), Int).runInfer(env));
