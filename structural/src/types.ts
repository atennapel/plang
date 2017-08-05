import {
	Kind,
	KArr,
	karr,
	ktype,
	krow,
} from './kinds';
import Id from './Id';
import TVarSet from './TVarSet';
import Subst from './Subst';
import Env from './Env';
import { Result } from './Result';
import InferState from './InferState';
import HasTVars from './HasTVars';

export abstract class Type implements HasTVars<Type> {
	abstract toString(): string;
	abstract equals(other: Type): boolean;
	abstract kind(): Result<TypeError, Kind>;
	abstract free(): TVarSet;
	abstract subst(sub: Subst): Type;

	static bind(a: TVar, b: Type): Result<TypeError, Subst> {
		if(b.free().has(a))
				return Result.err(new TypeError(`occurs check failed: ${a} and ${b}`));
		return Result.ok(Subst.of([a, b]));
	}

	static unify(a: Type, b: Type): Result<TypeError, Subst> {
		return a.kind().then(ka => b.kind().then(kb => {
			if(!ka.equals(kb))
				return Result.err(new TypeError(`Cannot unify kinds: ${ka} and ${kb}`));
			if(a instanceof TVar) return Type.bind(a, b);
			if(b instanceof TVar) return Type.bind(b, a);
			if(a instanceof TCon && b instanceof TCon && a.name === b.name)
				return Result.ok(Subst.empty());
			if(a instanceof TApp && b instanceof TApp)
				return Type.unify(a.left, b.left)
					.then(s1 => Type.unify(a.right.subst(s1), b.right.subst(s1))
					.map(s2 => s1.compose(s2)));
			return Result.err(new TypeError(`Cannot unify ${a} and ${b}`));
		}));
	}

	generalize(env?: Env): Scheme {
		return new Scheme(env? this.free().without(env.free()): this.free(), this);
	}
}

export class TVar extends Type {
	readonly id: Id;
	readonly _kind: Kind;

	constructor(id: Id, kind: Kind) {
		super();
		this.id = id;
		this._kind = kind;
	}

	hash() {
		return this.id.id;
	}

	toString() {
		return this.id.toString();
	}

	equals(other: Type): boolean {
		return other instanceof TVar && this.id.equals(other.id);
	}

	kind() {
		return Result.ok(this._kind);
	}

	free() {
		return TVarSet.of(this);
	}

	subst(sub: Subst): Type {
		return sub.getMap(this, t => t.subst(sub), this);
	}
}
export function tvar(id: Id, kind: Kind) {
	return new TVar(id, kind);
}

export class TCon extends Type {
	readonly name: string;
	readonly _kind: Kind;

	constructor(name: string, kind: Kind) {
		super();
		this.name = name;
		this._kind = kind;
	}

	toString() {
		return this.name;
	}

	equals(other: Type): boolean {
		return other instanceof TCon && this.name === other.name;
	}

	kind() {
		return Result.ok(this._kind);
	}

	free() {
		return TVarSet.empty();
	}

	subst(sub: Subst): Type {
		return this;
	}
}
export function tcon(name: string, kind: Kind) {
	return new TCon(name, kind);
}

export class TApp extends Type {
	readonly left: Type;
	readonly right: Type;

	constructor(left: Type, right: Type) {
		super();
		this.left = left;
		this.right = right;
	}

	toString() {
		return `(${this.left} ${this.right})`;
	}

	equals(other: Type): boolean {
		return other instanceof TApp &&
			this.left.equals(other.left) &&
			this.right.equals(other.right);
	}

	kind(): Result<TypeError, Kind> {
		return this.left.kind()
			.then(kleft => {
				if(!(kleft instanceof KArr))
					return Result.err(new TypeError(`invalid TApp, left side kind: ${kleft}`));
				return this.right.kind()
					.then(kright => {
						if(!kleft.left.equals(kright))
							return Result.err(new TypeError(`invalid TApp, right side kind: ${kright}`));
						return Result.ok(kleft.right);		
					});
			});		
	}

	free() {
		return this.left.free().union(this.right.free());
	}

	subst(sub: Subst): Type {
		return new TApp(this.left.subst(sub), this.right.subst(sub));
	}
}
export function tapp(...types: Type[]) {
	if(types.length === 0) throw new Error('tapp needs at least one argument');
	return types.reduce((a, b) => new TApp(a, b));
}

export class TRowEmpty extends Type {
	constructor() {
		super();
	}

	toString() {
		return '{}';
	}

	equals(other: Type): boolean {
		return other instanceof TRowEmpty;
	}

	kind(): Result<TypeError, Kind> {
		return Result.ok(krow);
	}

	free() {
		return TVarSet.empty();
	}

	subst(sub: Subst): Type {
		return this;
	}
}
export const trowempty = new TRowEmpty();

export class TRowExtend extends Type {
	readonly label: string;
	readonly type: Type;
	readonly rest: Type;

	constructor(label: string, type: Type, rest: Type) {
		super();
		this.label = label;
		this.type = type;
		this.rest = rest;
	}

	toString() {
		return `{ ${this.label} : ${this.type} | ${this.rest} }`;
	}

	equals(other: Type): boolean {
		return other instanceof TRowExtend &&
			this.label === other.label && this.type.equals(other.type) && this.rest.equals(other.rest);
	}

	kind(): Result<TypeError, Kind> {
		return this.type.kind()
			.then(kt => {
				if(!kt.equals(ktype))
					return Result.err(new TypeError(`Type in row must be of kind ${ktype}: ${this.type}`));
				return this.rest.kind()
					.then(krest => {
						if(!kt.equals(krow))
							return Result.err(new TypeError(`Rest in row must be of kind ${krow}: ${this.rest}`));
						return Result.ok(krow);
					});
			});
	}

	free() {
		return this.type.free().union(this.rest.free());
	}

	subst(sub: Subst): Type {
		return new TRowExtend(this.label, this.type.subst(sub), this.rest.subst(sub));
	}
}
export function trowextend(label: string, type: Type, rest: Type) {
	return new TRowExtend(label, type, rest);
}

export function trow(map: {[key: string]: Type}, rest?: Type) {
	let cur = rest || trowempty;
	const keys = Object.keys(map);
	for(let i = keys.length - 1; i >= 0; i--) {
		const key = keys[i];
		const type = map[key];
		cur = trowextend(key, type, cur);
	}
	return cur;
}

export const tarr = tcon('->', karr(ktype, ktype, ktype));
export function tarr2(a: Type, b: Type) {
	return tapp(tapp(tarr, a), b);
}
export function tarrs(...ts: Type[]) {
	return ts.reduceRight((a, b) => tarr2(b, a));
}

export class Scheme implements HasTVars<Scheme> {
	readonly tvars: TVarSet;
	readonly type: Type;

	constructor(tvars: TVarSet, type: Type) {
		this.tvars = tvars;
		this.type = type;
	}

	toString() {
		return `forall ${this.tvars} . ${this.type}`;
	}

	free(): TVarSet {
		return this.type.free().without(this.tvars);
	}
	subst(sub: Subst): Scheme {
		return new Scheme(
			this.tvars,
			this.type.subst(sub.removeTVars(this.tvars))
		);
	}

	instantiate(state: InferState): [InferState, Type] {
		const tvars = this.tvars.values();
		const [st, ids] = state.freshTVars(this.tvars.size(), tvars.map(v => v.id.name), tvars.map(v => v._kind));
		const sub = this.tvars.toSubst(tv => ids[tvars.indexOf(tv)]);
		return [st, this.type.subst(sub)];
	}
}
export function scheme(tvars: TVarSet, type: Type) {
	return new Scheme(tvars, type);
}
