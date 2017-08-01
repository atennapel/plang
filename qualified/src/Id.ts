export default class Id {
	readonly name: string;
	readonly index: number;
	readonly id: string;

	constructor(name: string, index: number) {
		this.name = name;
		this.index = index;
		this.id = this.name + this.index;
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
