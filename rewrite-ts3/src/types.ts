import Id from './Id';
import Label, { label } from './Label';
import Set from './Set';
import Map from './Map';
import { Kind, KArr, karr, KType, KRow } from './kinds';
import { Hashable } from './Hashable';
import { Subst, emptySubst, InferState, InferResult, err, ok, Env, compose } from './typechecker';

export abstract class Type {
	public abstract toString(): string;
	public abstract free(): Set<TVar>;
	public abstract kind(): InferResult<Kind>;
	public abstract subst(sub: Subst): Type;
}

export class TVar extends Type implements Hashable {
	readonly id: Id;
	readonly _kind: Kind;
	readonly lacks: Set<Label>;

	constructor(id: Id, kind: Kind, lacks: Set<Label>) {
		super();
		this.id = id;
		this._kind = kind;
		this.lacks = lacks;
	}

	public hash() {
		return this.id.hash();
	}

	public toString() {
		return `${this.id}${this.lacks.isEmpty()? '': `/${this.lacks}`}`;
	}

	public kind() {
		return ok(this._kind);
	}

	public free() {
		return Set.of(this);
	}

	public subst(sub: Subst): Type {
		return sub.getOr(this, this);
	}

	public equals(other: Type) {
		return other instanceof TVar && this.id.equals(other.id);
	}
}
export function tvar(id: Id, kind?: Kind, lacks?: Set<Label>) {
	return new TVar(id, kind || KType, lacks || Set.empty<Label>());
}

export class TCon extends Type {
	readonly name: string;
	private readonly _kind: Kind;

	constructor(name: string, kind: Kind) {
		super();
		this.name = name;
		this._kind = kind;
	}

	public toString() {
		return this.name;
	}

	public kind() {
		return ok(this._kind);
	}

	public free() {
		return Set.empty<TVar>();
	}

	public subst(sub: Subst): Type {
		return this;
	}
}
export function tcon(name: string, kind?: Kind) { return new TCon(name, kind || KType) }

export const TArr = tcon('->', karr(KType, KType, KType));

export function tarr(...types: Type[]) {
	return types.reduceRight((y, x) => new TApp(new TApp(TArr, x), y));
}

export const TRecord = tcon('Rec', karr(KRow, KType));
export function trecord(row: Type) { return new TApp(TRecord, row) }
export const TVariant = tcon('Var', karr(KRow, KType));
export function tvariant(row: Type) { return new TApp(TVariant, row) }

export class TApp extends Type {
	readonly left: Type;
	readonly right: Type;

	constructor(left: Type, right: Type) {
		super();
		this.left = left;
		this.right = right;
	}

	public toString(): string {
		if(this.left instanceof TApp && this.left.left instanceof TCon &&
			/[^a-z]/i.test(this.left.left.name[0]))
			return `(${this.left.right} ${this.left.left.name} ${this.right})`;
		return `(${this.left} ${this.right})`;
	}

	public kind() {
		return this.left.kind()
			.then(kleft => this.right.kind()
			.then(kright => {
				if(!(kleft instanceof KArr)) return err(`Ill-formed type (lhs): ${this}`);
				if(!kleft.left.equals(kright)) return err(`Ill-formed type: ${this}`);
				return ok(kleft.right);
			}));
	}

	public free() {
		return this.left.free().union(this.right.free());
	}

	public subst(sub: Subst): Type {
		return new TApp(this.left.subst(sub), this.right.subst(sub));
	}
}
export function tapp(...types: Type[]) {
	return types.reduce((x, y) => new TApp(x, y));
}

export class TMu extends Type {
	readonly tvar: TVar;
	readonly type: Type;

	constructor(tvar: TVar, type: Type) {
		super();
		this.tvar = tvar;
		this.type = type;
	}

	public toString(): string {
		return `(mu ${this.tvar} . ${this.type})`;
	}

	public kind() {
		return this.type.kind();
	}

	public free() {
		return this.type.free().without(Set.of(this.tvar));
	}

	public subst(sub: Subst): Type {
		return new TMu(this.tvar, this.type.subst(sub.removeKey(this.tvar)));
	}

	public unroll(): Type {
		return this.type.subst(Map.of([this.tvar, this]));
	}
}
export function tmu(tvar: TVar, type: Type) { return new TMu(tvar, type) }

export class TRowEmpty extends Type {
	constructor() {
		super();
	}

	public toString(): string {
		return '{}';
	}

	public kind() {
		return ok(KRow);
	}

	public free() {
		return Set.empty<TVar>();
	}

	public subst(sub: Subst): Type {
		return this;
	}
}
export const trowempty = new TRowEmpty();

export class TRowExtend extends Type {
	readonly label: Label;
	readonly type: Type;
	readonly rest: Type;

	constructor(label: Label, type: Type, rest: Type) {
		super();
		this.label = label;
		this.type = type;
		this.rest = rest;
	}

	public toString(): string {
		return `{ ${this.label} : ${this.type} | ${this.rest} }`;
	}

	public kind() {
		return this.type.kind().then(ktype => this.rest.kind().then(krest => {
			if(!ktype.equals(KType)) return err(`Type in row has invalid kind: ${ktype} in ${this}`);
			if(!krest.equals(KRow)) return err(`Rest in row has invalid kind: ${krest} in ${this}`);
			return ok(KRow);
		}));
	}

	public free() {
		return this.type.free().union(this.rest.free());
	}

	public subst(sub: Subst): Type {
		return new TRowExtend(this.label, this.type.subst(sub), this.rest.subst(sub));
	}
}
export function trowextend(label: Label, type: Type, rest: Type) {
	return new TRowExtend(label, type, rest);
}
export function trow(obj: {[key: string]: Type}, rest?: Type) {
	let c = rest || trowempty;
	const keys = Object.keys(obj);
	for(let i = keys.length - 1; i >= 0; i--)
		c = trowextend(label(keys[i]), obj[keys[i]], c);
	return c;
}

export class TScheme {
	readonly tvars: Set<TVar>;
	readonly type: Type;

	constructor(tvars: Set<TVar>, type: Type) {
		this.tvars = tvars;
		this.type = type;
	}

	public toString() {
		return `forall ${this.tvars} . ${this.type}`;
	}

	public free(): Set<TVar> {
		return this.type.free().without(this.tvars);
	}

	public subst(sub: Subst): TScheme {
		return new TScheme(this.tvars, this.type.subst(sub.removeKeySet(this.tvars)));
	}

	public instantiate(state: InferState): [InferState, Type] {
		const [nst, sub] = this.tvars.vals().reduce(([st, sub], tv) => {
			const [nst, tv1] = st.freshTVar(tv, tv._kind);
			return [nst, sub.add(tv, tv1)] as [InferState, Subst];
		}, [state, emptySubst] as [InferState, Subst]);
		return [nst, this.type.subst(sub)];
	}

	public static generalize(type: Type, env?: Env) {
		if(!env) return new TScheme(type.free(), type);
		return new TScheme(type.free().without(freeEnv(env)), type);
	}
}
export function tscheme(tvars: TVar[], type: Type) {
	return new TScheme(Set.from(tvars), type);
}
export function tschemeM(type: Type) {
	return new TScheme(Set.empty<TVar>(), type);
}

function freeEnv(env: Env): Set<TVar> {
	return env.vals().map(t => t.free()).reduce((x, y) => x.union(y), Set.empty<TVar>());
}
export function substEnv(env: Env, sub: Subst): Env {
	return env.map(t => t.subst(sub));
}

function rowParts(type: Type): InferResult<[Map<Label, Type>, TVar | null]> {
	if(type instanceof TRowExtend)
		return rowParts(type.rest)
			.map(([map, v]) => [map.add(type.label, type.type), v]);
	if(type instanceof TVar) return ok([Map.empty<Label, Type>(), type] as [Map<Label, Type>, TVar]);
	if(type instanceof TRowEmpty) return ok([Map.empty<Label, Type>(), null] as [Map<Label, Type>, null]);
	return err(`In rowParts, not a row type: ${type}`);
}

function rewriteRow(state: InferState, type: Type, newLabel: Label): InferResult<[InferState, Type, Type, Subst]> {
	if(type instanceof TRowExtend) {
		if(newLabel.equals(type.label)) return ok([state, type.type, type.rest, emptySubst]);
		if(type.rest instanceof TVar) {
			const [st1, beta] = state.freshTVar('r', KRow, Set.of(newLabel));
			const [st2, gamma] = st1.freshTVar('t');
			return bind(st2, type.rest, trowextend(newLabel, gamma, beta), KRow)
				.map(([st, sub]) => [st, gamma, trowextend(type.label, type.type, beta).subst(sub), sub]);
		}
		return rewriteRow(state, type.rest, newLabel)
			.map(([st, f, r, s]) => [st, f, trowextend(type.label, type.type, r), s]);
	}
	if(type instanceof TRowEmpty) return err(`Label ${newLabel} cannot be inserted in ${type}`);
	return err(`Unexpected type in rewriteRow: ${type}`);
}

function bind(state: InferState, a: TVar, b: Type, k: Kind): InferResult<[InferState, Subst]> {
	if(a.equals(b)) return ok([state, emptySubst]);
	if(b.free().contains(a))
		return ok([state, Map.of([a, tmu(a, b)])] as [InferState, Subst]);
	if(b instanceof TVar) {
		const [st, tvar] = state.freshTVar(b, k, a.lacks.union(b.lacks));
		return ok([st, Map.of([a, tvar], [b, tvar])] as [InferState, Subst]);
	}
	if(k.equals(KRow))
		return rowParts(b)
			.then(([map, v]) => {
				const is = a.lacks.intersection(Set.from(map.keys().map(label)));
				if(!is.isEmpty()) return err(`Cannot unify ${a} and ${b}, repeated labels: ${is}`);
				if(!v) return ok([state, Map.of([a, b])]);
				const [st, tvar] = state.freshTVar(v, k, a.lacks.union(v.lacks));
				return ok([st, compose(Map.of([a, b]), Map.of([v, tvar]))]);
			});
	return ok([state, Map.of([a, b])]);
}

function unifyR(state: InferState, a: Type, b: Type): InferResult<[InferState, Subst]> {
	return a.kind().then(ka => b.kind().then(kb => {
		if(!ka.equals(kb)) return err(`Kind mismatch: ${ka} and ${kb} in ${a} ~ ${b}`);
		console.log(`unify ${a} ~ ${b}`);
		if(a instanceof TVar) return bind(state, a, b, ka);
		if(b instanceof TVar) return bind(state, b, a, ka);
		if(a instanceof TCon && b instanceof TCon && a.name === b.name)
			return ok([state, emptySubst]);
		if(a instanceof TRowEmpty && b instanceof TRowEmpty)
			return ok([state, emptySubst]);
		if(a instanceof TApp && b instanceof TApp)
			return unify(state, a.left, b.left)
				.then(([st, s1]) => unify(st, a.right.subst(s1), b.right.subst(s1))
				.map(([st, s2]) => [st, compose(s1, s2)]));
		if(a instanceof TMu && b instanceof TMu)
			return err(`Unimplemented unification: ${a} ~ ${b}`);
		if(a instanceof TMu) return unify(state, a.unroll(), b);
		if(b instanceof TMu) return unify(state, a, b.unroll());
		if(a instanceof TRowExtend && b instanceof TRowExtend)
			return rewriteRow(state, b, a.label)
				.then(([st, ta, tb, sub]) => rowParts(tb)
				.then(([parts, tv]) => {
					if(tv && sub.contains(tv)) return err(`Recursive row type in unification: ${a} ~ ${b}`);
					return unify(st, a.type.subst(sub), ta.subst(sub))
						.then(([st, sub2]) => {
							const s = compose(sub, sub2);
							return unify(st, a.rest.subst(s), tb.subst(s))
								.map(([st, sub3]) => [st, compose(s, sub3)]);
						});	
				}));
		return err(`Cannot unify ${a} and ${b}`);
	}));
}

export function unify(state: InferState, a: Type, b: Type): InferResult<[InferState, Subst]> {
	const res = unifyR(state, a, b);
	// console.log('-> ' + res);
	return res;
}
