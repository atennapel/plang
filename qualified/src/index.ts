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
} from './exprs';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import InferState from './InferState';

const Int = tcon('Int', ktype);

const env = Env.of(
	['zero', Int.generalize()],
);

console.log(''+elet('id', elam(['x'], evar('x')), eapp(evar('id'), evar('zero'))).runInfer(env));
