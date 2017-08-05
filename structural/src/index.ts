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
	eanno,
} from './exprs';
import { id } from './Id';
import IdStore from './IdStore';
import Env from './Env';
import TVarSet from './TVarSet';
import InferState from './InferState';

const Int = tcon('Int', ktype);
const Float = tcon('Float', ktype);
const Str = tcon('Str', ktype);

const env = Env.of(
	['zero', Int.generalize()],
	['zerof', Float.generalize()],
	['str', Str.generalize()],
	['inc', tarrs(Int, Int).generalize()],
);

const expr = elam(['x', 'y'], evar('x'));
console.log(''+expr);
console.log(''+expr.runInfer(env, new InferState(new IdStore({}))));
