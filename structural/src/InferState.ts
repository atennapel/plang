import Id from './Id';
import IdStore from './IdStore';
import { Kind } from './kinds';
import { TVar, tvar } from './types';
import Map from './Map';

export default class InferState {
	readonly tvars: IdStore;

	constructor(tvars?: IdStore) {
		this.tvars = tvars || new IdStore();
	}

	freshTVar(name: string, kind: Kind): [InferState, TVar] {
		const [store, id] = this.tvars.fresh(name);
		return [new InferState(store), tvar(id, kind)];
	}

	freshTVars(n: number, names: string | string[], kinds: Kind | Kind[]): [InferState, TVar[]] {
		const [store, tvars] = this.tvars.freshN(n, names);
		return [
			new InferState(store),
			tvars.map((id, i) => tvar(
				id,
				Array.isArray(kinds)? kinds[i]: kinds
			))
		];
	}

	toString() {
		return 'InferState';
	}
}
