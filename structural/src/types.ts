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
import Map from './Map';
import { Constraint, clacks } from './constraints';

export abstract class Type implements HasTVars<Type> {
	abstract toString(): string;
	abstract equals(other: Type): boolean;
	abstract kind(): Result<TypeError, Kind>;
	abstract free(): TVarSet;
	abstract subst(sub: Subst): Type;
	abstract containsLabel(label: string): boolean;

	static bind(st: InferState, a: TVar, b: Type): Result<TypeError, [InferState, Subst, Constraint[]]> {
		if(a.equals(b)) return Result.ok([st, Subst.empty(), []] as [InferState, Subst, Constraint[]]);
		if(b.free().has(a))
			return Result.ok([st, Subst.of([a, tfix(a, b)]), []] as [InferState, Subst, Constraint[]]);
		return Result.ok([st, Subst.of([a, b]), []] as [InferState, Subst, Constraint[]]);
	}

	static unify(st: InferState, a: Type, b: Type): Result<TypeError, [InferState, Subst, Constraint[]]> {
		return a.kind().then(ka => b.kind().then(kb => {
			if(!ka.equals(kb))
				return Result.err(new TypeError(`Cannot unify kinds: ${ka} and ${kb}`));
			// console.log(`unify ${a} and ${b}`);
			if(a instanceof TVar) return Type.bind(st, a, b);
			if(b instanceof TVar) return Type.bind(st, b, a);
			if(a instanceof TRowEmpty && b instanceof TRowEmpty)
				return Result.ok([st, Subst.empty(), []] as [InferState, Subst, Constraint[]]);
			if(a instanceof TCon && b instanceof TCon && a.name === b.name)
				return Result.ok([st, Subst.empty(), []] as [InferState, Subst, Constraint[]]);
			if(a instanceof TApp && b instanceof TApp)
				return Type.unify(st, a.left, b.left)
					.then(([st, s1, cs1]) => Type.unify(st, a.right.subst(s1), b.right.subst(s1))
					.map(([st, s2, cs2]) => [st, s1.compose(s2), cs1.concat(cs2)] as [InferState, Subst, Constraint[]]));
			if(a instanceof TFix && b instanceof TFix) {
				const [st1, tv] = st.freshTVar('f', a.tvar._kind);
				return Type.unify(st1, a.substTVar(tv), b.substTVar(tv));
			}
			if(a instanceof TFix) return Type.unify(st, a.unroll(), b);
			if(b instanceof TFix) return Type.unify(st, a, b.unroll());
			if(a instanceof TRowExtend && b instanceof TRowExtend) {
				return Type.rewriteRow(st, b, a.label)
					.then(([st, fieldt2, rowtail2, theta1, cs]) =>
						Type.unify(st, a.type.subst(theta1), fieldt2.subst(theta1))
						.then(([st, theta2]) => {
							const s = theta1.compose(theta2);
							return Type.unify(st, a.rest.subst(s), rowtail2.subst(s))
								.map(([st, theta3]) => {
									const fs = s.compose(theta3);
									return [st, fs, cs.map(c => c.subst(fs))] as [InferState, Subst, Constraint[]];
								})
						}));
			}
			return Result.err(new TypeError(`Cannot unify ${a} and ${b}`));
		}));
	}

	static rewriteRow(st: InferState, t: Type, l: string): Result<TypeError, [InferState, Type, Type, Subst, Constraint[]]> {
		if(t instanceof TRowEmpty)
			return Result.err(new TypeError(`Cannot insert ${l} into ${t}`));
		if(t instanceof TRowExtend) {
			if(t.label === l) return Result.ok([st, t.type, t.rest, Subst.empty(), []] as [InferState, Type, Type, Subst, Constraint[]]);
			if(t.rest instanceof TVar) {
				const [st1, beta] = st.freshTVar('r', krow);
				const [st2, gamma] = st1.freshTVar('t', ktype);
				return Type.bind(st2, t.rest, trowextend(l, gamma, beta))
					.map(([st, s, cs]) => [st, gamma, trowextend(t.label, t.type, beta).subst(s), s, cs.concat([clacks(l, beta)])] as [InferState, Type, Type, Subst, Constraint[]]);
			} else {
				return Type.rewriteRow(st, t.rest, l)
					.map(([st, fieldt, rowt, sub, cs]) =>
						[st, fieldt, trowextend(t.label, t.type, rowt), sub, cs] as
						[InferState, Type, Type, Subst, Constraint[]]);
			}
		}
		return Result.err(new TypeError(`Unexpected type in rewriteRow: ${t}`));
	}

	generalize(env?: Env, constraints?: Constraint[]): Scheme {
		return new Scheme(env? this.free().without(env.free()): this.free(), constraints || [], this);
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

	containsLabel(label: string): boolean {
		return false;
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

	containsLabel(label: string): boolean {
		return false;
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

	containsLabel(label: string): boolean {
		return false;
	}

	toString(): string {
		if(this.left instanceof TApp && this.left.left instanceof TCon && /[^a-z]+/i.test(this.left.left.name))
			return `(${this.left.right} ${this.left.left} ${this.right})`;
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
		return '<>';
	}
	
	containsLabel(label: string): boolean {
		return false;
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

	containsLabel(label: string): boolean {
		return this.label === label || this.rest.containsLabel(label);
	}

	toString() {
		return `<${this.label} : ${this.type} | ${this.rest}>`;
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
						if(!krest.equals(krow))
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

export class TFix extends Type {
	readonly tvar: TVar;
	readonly type: Type;
	
	constructor(tvar: TVar, type: Type) {
		super();
		this.tvar = tvar;
		this.type = type;
	}

	containsLabel(label: string): boolean {
		return false;
	}

	toString() {
		return `(fix ${this.tvar} . ${this.type})`;
	}

	equals(other: Type): boolean {
		return other instanceof TFix && this.tvar.equals(other.tvar) && this.type.equals(other.type);
	}

	kind() {
		return this.type.kind();
	}

	free() {
		return this.type.free().without(TVarSet.of(this.tvar));
	}

	subst(sub: Subst) {
		return new TFix(this.tvar, this.type.subst(sub.removeTVars(TVarSet.of(this.tvar))));
	}

	substTVar(t: Type) {
		return this.type.subst(Subst.of([this.tvar, t]));
	}

	unroll() {
		return this.substTVar(this);
	}
}
export function tfix(tvar: TVar, type: Type) {
	return new TFix(tvar, type);
}

export const tarr = tcon('->', karr(ktype, ktype, ktype));
export function tarr2(a: Type, b: Type) {
	return tapp(tapp(tarr, a), b);
}
export function tarrs(...ts: Type[]) {
	return ts.reduceRight((a, b) => tarr2(b, a));
}

export const trecord = tcon('Rec', karr(krow, ktype));
export const tvariant = tcon('Var', karr(krow, ktype));
export const teff = tcon('Eff', karr(krow, ktype, ktype));
export const tnumber = tcon('Number', ktype);
export const tstring = tcon('String', ktype);

export function isEff(t: Type) {
	return t instanceof TApp && t.left instanceof TApp && t.left.left.equals(teff);
}

export class Scheme implements HasTVars<Scheme> {
	readonly tvars: TVarSet;
	readonly constraints: Constraint[];
	readonly type: Type;

	constructor(tvars: TVarSet, constraints: Constraint[], type: Type) {
		this.tvars = tvars;
		this.constraints = constraints;
		this.type = type;
	}

	toString() {
		return `forall ${this.tvars} . {${this.constraints.join(', ')}} => ${this.type}`;
	}

	free(): TVarSet {
		return this.type.free()
			.union(this.constraints.map(c => c.free()).reduce((a, b) => a.union(b), TVarSet.empty()))
			.without(this.tvars);
	}
	subst(sub: Subst): Scheme {
		const nsub = sub.removeTVars(this.tvars);
		return new Scheme(
			this.tvars,
			this.constraints.map(c => c.subst(nsub)),
			this.type.subst(nsub)
		);
	}

	instantiate(state: InferState): [InferState, Constraint[], Type] {
		const tvars = this.tvars.values();
		const [st, ids] = state.freshTVars(this.tvars.size(), tvars.map(v => v.id.name), tvars.map(v => v._kind));
		const sub = this.tvars.toSubst(tv => ids[tvars.indexOf(tv)]);
		return [st, this.constraints.map(c => c.subst(sub)), this.type.subst(sub)];
	}
}
export function scheme(tvars: TVarSet, constraints: Constraint[], type: Type) {
	return new Scheme(tvars, constraints, type);
}
