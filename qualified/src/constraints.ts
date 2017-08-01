import HasTVars from './HasTVars';
import TVarSet from './TVarSet';
import Subst from './Subst';
import { Type, TCon, TVar } from './types';
import { Result } from './Result';

export abstract class Constraint implements HasTVars<Constraint> {
	abstract toString(): string;
	abstract free(): TVarSet;
	abstract subst(sub: Subst): Constraint;
	abstract solve(): Result<TypeError, Subst | null>;
	abstract equals(other: Constraint): boolean;

	static simplify(cs: Constraint[]): Constraint[] {
		const res = [];
		for(let i = 0, l = cs.length; i < l; i++) {
			const c = cs[i];
			let found = false;
			for(let j = 0, k = res.length; j < k; j++) {
				if(c.equals(res[j])) {
					found = true;
					break;
				}
			}
			if(!found) res.push(c);
		}
		return res;
	}
}

export class CNumeric extends Constraint {
	readonly type: Type;

	constructor(type: Type) {
		super();
		this.type = type;
	}

	toString() {
		return `CNumeric(${this.type})`;
	}

	free() {
		return this.type.free();
	}
	subst(sub: Subst) {
		return new CNumeric(this.type.subst(sub));
	}

	equals(other: Constraint): boolean {
		return other instanceof CNumeric && this.type.equals(other.type);
	}

	solve(): Result<TypeError, Subst | null> {
		const type = this.type;
		if(type instanceof TCon && (type.name === 'Int' || type.name === 'Float')) {
			return Result.ok(Subst.empty());
		} else if(type instanceof TVar) {
			return Result.ok(null);
		} else {
			return Result.err(new TypeError(`${this} fails`));
		}
	}
}
