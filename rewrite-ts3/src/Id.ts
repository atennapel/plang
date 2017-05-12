import { Hashable } from './Hashable';
import { clone } from './utils';

export default class Id implements Hashable {
	readonly name: string;
	readonly id: number;
	readonly fullname: string;

	constructor(name: string, id?: number) {
		this.name = name;
		this.id = id || 0;
		this.fullname = this.name + this.id;
	}

	public hash() {
		return this.fullname;
	}

	public toString() {
		return this.id != 0? this.fullname: this.name;
	}

	public equals(other: Id) {
		return this.hash() === other.hash();
	}
}
export function id(name: string, id_?: number) { return new Id(name, id_) }

export class IdStore {
	private readonly store: {[key: string]: number};

	constructor(store?: {[key: string]: number}) {
		this.store = store || {};
	}

	fresh(name_?: string): [IdStore, Id] {
		const name = name_ || '_';
		const store = clone(this.store);
		const curval = this.store[name] || 0;
		store[name] = curval + 1;
		return [new IdStore(store), new Id(name, curval)];
	}

	freshId(id: Id) {
		return this.fresh(id.name);
	}
}
