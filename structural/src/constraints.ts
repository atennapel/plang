import { Type } from './types';
import HasTVars from './HasTVars'
import TVarSet from './TVarSet';
import Subst from './Subst';
import { Result } from './Result';

export abstract class Constraint implements HasTVars<Constraint> {
	abstract toString(): string;
	abstract free(): TVarSet;
	abstract subst(sub: Subst): Constraint;
	abstract check(): Result<TypeError, boolean>;
}

export class CLacks extends Constraint {
	readonly label: string;
	readonly type: Type;

	constructor(label: string, type: Type) {
		super();
		this.label = label;
		this.type = type;
	}

	toString() {
		return `${this.type}/${this.label}`;
	}

	free() {
		return this.type.free();
	}

	subst(sub: Subst): Constraint {
		return new CLacks(this.label, this.type.subst(sub));
	}

	check(): Result<TypeError, boolean> {
		if(this.type.containsLabel(this.label))
			return Result.err(new TypeError(`Constraint failed: ${this}`));
		return Result.ok(true);
	}
}
export function clacks(label: string, type: Type) {
	return new CLacks(label, type);
}
