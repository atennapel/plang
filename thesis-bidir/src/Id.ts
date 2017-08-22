export default class Id {
	readonly name: string;
	readonly index: number;
	readonly id: string;

	constructor(name: string, index: number) {
		this.name = name;
		this.index = index;
		this.id = `${this.name}$${this.index}`;
	}

	toString() {
		return this.index? this.id: this.name;
	}

	equals(other: Id) {
		return this.id === other.id;
	}

	next() {
		return new Id(this.name, this.index + 1);
	}
}
export function id(name: string, index: number) {
	return new Id(name, index);
}

type Store = {[id: string]: Id};

export class IdStore {
	private readonly store: Store;

	constructor(store?: Store) {
		this.store = store || {};
	}

	toString() {
		return `IdStore(${Object.keys(this.store).map(k => `${k}: ${this.store[k]}`).join(', ')})`;
	}

	fresh(name: string): {st: IdStore, id: Id} {
		const newStore: Store = {};
		for(let k in this.store) newStore[k] = this.store[k];
		if(!this.store[name]) newStore[name] = new Id(name, 0);
		const id = newStore[name];
		newStore[name] = id.next();
		const idstore = new IdStore(newStore);
		return {st: idstore, id};
	}

	freshN(n: number, names: string | string[]): {st: IdStore, ids: Id[]} {
		const ids: Id[] = [];
		let cur: IdStore = this;
		for(let i = 0; i < n; i++) {
			const {st, id} = cur.fresh(Array.isArray(names)? names[i]: names);
			ids.push(id);
			cur = st;
		}
		return {st: cur, ids};
	}
}
