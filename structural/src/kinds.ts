export abstract class Kind {
	abstract toString(): string;
	abstract equals(other: Kind): boolean;
}

export class KCon extends Kind {
	readonly name: string;

	constructor(name: string) {
		super();
		this.name = name;
	}

	toString() {
		return this.name;
	}

	equals(other: Kind): boolean {
		return other instanceof KCon && this.name === other.name;
	}
}
export function kcon(name: string) {
	return new KCon(name);
}

export class KArr extends Kind {
	readonly left: Kind;
	readonly right: Kind;

	constructor(left: Kind, right: Kind) {
		super();
		this.left = left;
		this.right = right;
	}

	toString() {
		return `(${this.left} -> ${this.right})`;
	}

	equals(other: Kind): boolean {
		return other instanceof KArr &&
			this.left.equals(other.left) &&
			this.right.equals(other.right);
	}
}
export function karr(...kinds: Kind[]) {
	return kinds.reduceRight((a, b) => new KArr(b, a));
}

export const ktype = kcon('Type');
export const krow = kcon('Row');
