import Id from './Id';

type Store = {[id: string]: Id};

export default class IdStore {
	private readonly store: Store;

	constructor(store?: Store) {
		this.store = store || {};
	}

	toString() {
		return `IdStore(${Object.keys(this.store).map(k => `${k}: ${this.store[k]}`).join(', ')})`;
	}

	fresh(name: string): [IdStore, Id] {
		const newStore: Store = {};
		for(let k in this.store) newStore[k] = this.store[k];
		if(!this.store[name]) newStore[name] = new Id(name, 0);
		const id = newStore[name];
		newStore[name] = id.next();
		const idstore = new IdStore(newStore);
		return [idstore, id];
	}

	freshN(n: number, names: string | string[]): [IdStore, Id[]] {
		const vars: Id[] = [];
		let cur: IdStore = this;
		for(let i = 0; i < n; i++) {
			const [st, id] = cur.fresh(Array.isArray(names)? names[i]: names);
			vars.push(id);
			cur = st;
		}
		return [cur, vars];
	}
}
