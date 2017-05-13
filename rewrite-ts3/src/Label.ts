import { Hashable } from './Hashable';

export default class Label implements Hashable {
	readonly label: string;

	constructor(label: string) {
		this.label = label;
	}

	public toString() {
		return this.label;
	}

	public hash() {
		return this.label;
	}

	public equals(other: Label) {
		return this.label === other.label;
	}
}
export function label(label: string) { return new Label(label) }
