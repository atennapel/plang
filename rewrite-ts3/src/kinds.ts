export abstract class Kind {
	public abstract toString(): string;
	public abstract equals(other: Kind): boolean;
}

export class KCon extends Kind {
	readonly name: string;

	constructor(name: string) {
		super();
		this.name = name;
	}

	public toString() {
		return this.name;
	}

	public equals(other: Kind): boolean {
		return other instanceof KCon && this.name === other.name;
	}
}
export function kcon(name: string) { return new KCon(name) }
export const KType = kcon('Type');
export const KRow = kcon('Row');

export class KArr extends Kind {
	readonly left: Kind;
	readonly right: Kind;

	constructor(left: Kind, right: Kind) {
		super();
		this.left = left;
		this.right = right;
	}

	public toString() {
		return `(${this.left} -> ${this.right})`;
	}

	public equals(other: Kind): boolean {
		return other instanceof KArr && this.left.equals(other.left) && this.right.equals(other.right);
	}
}
export function karr(...kinds: Kind[]) {
	return kinds.reduceRight((x, y) => new KArr(y, x));
}
