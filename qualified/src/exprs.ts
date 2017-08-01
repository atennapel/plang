import { ktype } from './kinds';
import { Type, tvar, tarrs, scheme } from './types';
import Env from './Env';
import { Result } from './Result';
import InferState from './InferState';
import Subst from './Subst';
import TVarSet from './TVarSet';
import { Constraint } from './constraints';

export abstract class Expr {
	abstract toString(): string;
	abstract infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]>;

	runInfer(env?: Env, state?: InferState) {
		return this.infer(state || new InferState(), env || Env.empty())
			.map(([st, sub, cs, t]) => {
				console.log(cs);
				return t.subst(sub)
			});
	}
}

export class EVar extends Expr {
	readonly name: string;

	constructor(name: string) {
		super();
		this.name = name;
	}

	toString() {
		return this.name;
	}

	infer(state: InferState, env: Env) {
		return env.getMap<Result<TypeError, [InferState, Subst, Constraint[], Type]>>(
			this.name,
			s => {
				const [st, cs, t] = s.instantiate(state);
				return Result.ok([st, Subst.empty(), cs, t] as [InferState, Subst, Constraint[], Type])
			},
			Result.err(new TypeError(`Undefined variable: ${this.name}`))
		);
	}
}
export function evar(name: string) {
	return new EVar(name);
}

export class ELam extends Expr {
	readonly name: string;
	readonly body: Expr;

	constructor(name: string, body: Expr) {
		super();
		this.name = name;
		this.body = body;
	}

	toString() {
		return `(\\${this.name} -> ${this.body})`;
	}

	infer(state: InferState, env: Env) {
		const [st1, tv] = state.freshTVar(this.name, ktype);
		return this.body.infer(st1, env.add(this.name, scheme(TVarSet.empty(), [], tv)))
			.map(([st2, sub1, cs, t]) => [st2, sub1, cs, tarrs(tv.subst(sub1), tv)] as [InferState, Subst, Constraint[], Type]);
	}
}
export function elam(args: string[], body: Expr) {
	if(args.length <= 0) throw new Error('elam needs at least 1 parameter');
	return args.reduceRight((e, a) => new ELam(a, e), body);
}

export class EApp extends Expr {
	readonly left: Expr;
	readonly right: Expr;

	constructor(left: Expr, right: Expr) {
		super();
		this.left = left;
		this.right = right;
	}

	toString() {
		return `(${this.left} ${this.right})`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		const [st1, tv] = state.freshTVar('t', ktype);
		return this.left.infer(st1, env)
			.then(([st2, sub1, cs1, tleft]) => this.right.infer(st2, env)
			.then(([st3, sub2, cs2, tright]) => {
				const sub3 = sub1.compose(sub2);
				return Type.unify(tleft.subst(sub3), tarrs(tright, tv))
					.map(sub4 => {
						const sub5 = sub3.compose(sub4);
						return [st3, sub5, cs1.concat(cs2), tv.subst(sub5)] as [InferState, Subst, Constraint[], Type]
					});
			}))
	}
}
export function eapp(...es: Expr[]) {
	if(es.length === 0) throw new Error('eapp needs at least one argument');
	return es.reduce((a, b) => new EApp(a, b));
}

export class ELet extends Expr {
	readonly name: string;
	readonly val: Expr;
	readonly body: Expr;

	constructor(name: string, val: Expr, body: Expr) {
		super();
		this.name = name;
		this.val = val;
		this.body = body;
	}

	toString() {
		return `(let ${this.name} = ${this.val} in ${this.body})`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		return this.val.infer(state, env)
			.then(([st1, sub1, cs1, tval]) => {
				const newenv = env.subst(sub1);
				return this.body.infer(st1, newenv.add(this.name, tval.generalize(newenv, cs1)))
					.map(([st2, sub2, cs2, tlet]) => {
						const sub3 = sub1.compose(sub2);
						return [st2, sub3, cs2, tlet.subst(sub3)] as [InferState, Subst, Constraint[], Type];
					});
			})
	}
}
export function elet(name: string, val: Expr, body: Expr) {
	return new ELet(name, val, body);
}

export class EAnno extends Expr {
	readonly expr: Expr;
	readonly type: Type;

	constructor(expr: Expr, type: Type) {
		super();
		this.expr = expr;
		this.type = type;
	}

	toString() {
		return `(${this.expr} : ${this.type})`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		return this.expr.infer(state, env)
			.then(([st1, sub1, cs, texpr]) => Type.unify(this.type.subst(sub1), texpr)
			.map(sub2 => {
				const sub3 = sub1.compose(sub2);
				return [st1, sub3, cs, this.type.subst(sub3)] as [InferState, Subst, Constraint[], Type];
			}));
	}
}
export function eanno(expr: Expr, type: Type) {
	return new EAnno(expr, type);
}
