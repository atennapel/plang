export default class Id {
	public readonly id: string;
	public readonly name: string;
	public readonly num: number;

	constructor(name: string, num: number) {
		this.name = name;
		this.num = num;
		this.id = name + num;
	}
}
