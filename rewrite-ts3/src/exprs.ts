import { Type, TVar, TScheme, tscheme, tschemeM, tarr, unify, substEnv, trowempty, trowextend, trecord, tvariant } from './types';
import { KType, KRow } from './kinds';
import { Hashable } from './Hashable';
import { Env, InferResult, ok, err, InferState, emptySubst, emptyEnv, Subst, compose } from './typechecker';
import Set from './Set';
import Label, { label } from './Label';

export abstract class Expr {
	public abstract toString(): string;
	public abstract infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]>;

	public typecheck(env?: Env, state?: InferState): InferResult<Type> {
		return this.infer(env || emptyEnv, state || InferState.empty()).then(([_, sub, type_]) => {
			const type = type_.subst(sub);
			return type.kind().then(kt => {
				if(!kt.equals(KType)) return err(`Invalid kind for expression ${type} : ${kt}`);
				return ok(type);
			});
		});
	}
}

export class EVar extends Expr implements Hashable {
	readonly id: string;

	constructor(id: string) {
		super();
		this.id = id;
	}

	public toString() {
		return this.id;
	}

	public hash() {
		return this.id;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		if(env.contains(this)) {
			const [st, type] = env.get(this).instantiate(state);
			return ok([st, emptySubst, type]);
		}
		return err(`Undefined variable: ${this.id}`);
	}
}
export function evar(id: string) { return new EVar(id) }

export class ELam extends Expr {
	readonly arg: EVar;
	readonly body: Expr;

	constructor(arg: EVar, body: Expr) {
		super();
		this.arg = arg;
		this.body = body;
	}

	public toString() {
		return `(\\${this.arg} -> ${this.body})`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st, tv] = state.freshTVar(this.arg.id);
		const nenv = env.add(this.arg, tschemeM(tv));
		return this.body.infer(nenv, st)
			.map(([st, sub, type]) => [st, sub, tarr(tv, type).subst(sub)]);
	}
}
export function elam(args: string[], body: Expr) {
	return args.reduceRight((expr, arg) => new ELam(evar(arg), expr), body);
}

export class EApp extends Expr {
	readonly left: Expr;
	readonly right: Expr;

	constructor(left: Expr, right: Expr) {
		super();
		this.left = left;
		this.right = right;
	}

	public toString() {
		return `(${this.left} ${this.right})`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		return this.left.infer(env, state).then(([st, s1, tleft]) => {
			const nenv = substEnv(env, s1);
			return this.right.infer(nenv, st).then(([st, s2, tright]) => {
				const [nst, tv] = st.freshTVar('t');
				const s3 = compose(s1, s2);
				return unify(nst, tleft.subst(s3), tarr(tright, tv).subst(s3)).map(([st, s4]) => {
					const s5 = compose(s3, s4);
					return [st, s5, tv.subst(s5)];
				});
			});
		});
	}
}
export function eapp(...exprs: Expr[]) {
	return exprs.reduce((x, y) => new EApp(x, y));
}

export class ELet extends Expr {
	readonly arg: EVar;
	readonly val: Expr;
	readonly body: Expr;

	constructor(arg: EVar, val: Expr, body: Expr) {
		super();
		this.arg = arg;
		this.val = val;
		this.body = body;
	}

	public toString() {
		return `(let ${this.arg} = ${this.val} in ${this.body})`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		return this.val.infer(env, state).then(([st, s1, type]) => {
			const nenv = env.add(this.arg, TScheme.generalize(type, substEnv(env, s1)));
			return this.body.infer(nenv, st).map(([st, s2, rtype]) => {
				const s3 = compose(s1, s2);
				return [st, s3, rtype.subst(s3)];
			});
		});
	}
}
export function elet(arg: string, val: Expr, body: Expr) {
	return new ELet(evar(arg), val, body);
}

export class ELetr extends Expr {
	readonly arg: EVar;
	readonly val: Expr;
	readonly body: Expr;

	constructor(arg: EVar, val: Expr, body: Expr) {
		super();
		this.arg = arg;
		this.val = val;
		this.body = body;
	}

	public toString() {
		return `(letr ${this.arg} = ${this.val} in ${this.body})`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [nst, tv] = state.freshTVar(this.arg.id);
		const nenv = env.add(this.arg, tschemeM(tv));
		return this.val.infer(nenv, nst)
			.then(([st, s1, tval]) => unify(st, tv.subst(s1), tval.subst(s1))
			.then(([st, s2]) => {
				const s3 = compose(s1, s2);
				const type = tv.subst(s3);
				const nenv = env.add(this.arg, TScheme.generalize(type, substEnv(env, s3)));
				return this.body.infer(nenv, st).map(([st, s4, type]) => {
					const s5 = compose(s3, s4);
					return [st, s5, type.subst(s5)];
				});
			}))
	}
}
export function eletr(arg: string, val: Expr, body: Expr) {
	return new ELetr(evar(arg), val, body);
}

export class EAnno extends Expr {
	readonly expr: Expr;
	readonly type: Type;

	constructor(expr: Expr, type: Type) {
		super();
		this.expr = expr;
		this.type = type;
	}

	public toString() {
		return `(${this.expr} : ${this.type})`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		return this.expr.infer(env, state)
			.then(([st, s1, type]) => unify(st, this.type.subst(s1), type.subst(s1))
			.map(([st, s2]) => {
				const s3 = compose(s1, s2);
				return [st, s3, this.type.subst(s3)];
			}));
	}
}
export function eanno(expr: Expr, type: Type) {
	return new EAnno(expr, type);
}

export class ERecordEmpty extends Expr {
	constructor() {
		super();
	}

	public toString() {
		return '{}';
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		return ok([state, emptySubst, trecord(trowempty)] as [InferState, Subst, Type]);
	}
}
export const erecordempty = new ERecordEmpty();

export class EEnd extends Expr {
	constructor() {
		super();
	}

	public toString() {
		return 'end';
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st, t] = state.freshTVar('t');
		return ok([st, emptySubst, tarr(tvariant(trowempty), t)] as [InferState, Subst, Type]);
	}
}
export const eend = new EEnd();

export class ESelect extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `.${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, t] = state.freshTVar('t', KType);
		const [st2, r] = st1.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st2,
			emptySubst,
			tarr(trecord(trowextend(this.label, t, r)), t),
		] as [InferState, Subst, Type]);
	}
}
export function eselect(label: Label) { return new ESelect(label) }

export class EExtend extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `.+${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, t] = state.freshTVar('t', KType);
		const [st2, r] = st1.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st2,
			emptySubst,
			tarr(
				t,
				trecord(r),
				trecord(trowextend(this.label, t, r))
			),
		] as [InferState, Subst, Type]);
	}
}
export function eextend(label: Label) { return new EExtend(label) }

export class ERestrict extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `.-${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, t] = state.freshTVar('t', KType);
		const [st2, r] = st1.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st2,
			emptySubst,
			tarr(
				trecord(trowextend(this.label, t, r)),
				trecord(r)
			),
		] as [InferState, Subst, Type]);
	}
}
export function erestrict(label: Label) { return new ERestrict(label) }

export class EInject extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `@${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, t] = state.freshTVar('t', KType);
		const [st2, r] = st1.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st2,
			emptySubst,
			tarr(
				t,
				tvariant(trowextend(this.label, t, r))
			),
		] as [InferState, Subst, Type]);
	}
}
export function einject(label: Label) { return new EInject(label) }

export class EEmbed extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `@+${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, t] = state.freshTVar('t', KType);
		const [st3, r] = st1.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st3,
			emptySubst,
			tarr(
				tvariant(r),
				tvariant(trowextend(this.label, t, r))
			),
		] as [InferState, Subst, Type]);
	}
}
export function eembed(label: Label) { return new EEmbed(label) }

export class EElim extends Expr {
	readonly label: Label;

	constructor(label: Label) {
		super();
		this.label = label;
	}

	public toString() {
		return `?${this.label}`;
	}

	public infer(env: Env, state: InferState): InferResult<[InferState, Subst, Type]> {
		const [st1, a] = state.freshTVar('a', KType);
		const [st2, b] = st1.freshTVar('b', KType);
		const [st3, r] = st2.freshTVar('r', KRow, Set.of(this.label));
		return ok([
			st3,
			emptySubst,
			tarr(
				tarr(a, b),
				tarr(tvariant(r), b),
				tvariant(trowextend(this.label, a, r)),
				b
			),
		] as [InferState, Subst, Type]);
	}
}
export function eelim(label: Label) { return new EElim(label) }
