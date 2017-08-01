import { ktype } from './kinds';
import { Type, tvar, tarrs, scheme } from './types';
import Env from './Env';
import { Result } from './Result';
import InferState from './InferState';
import Subst from './Subst';
import TVarSet from './TVarSet';

export abstract class Expr {
	abstract toString(): string;
	abstract infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Type]>;

	runInfer(env?: Env) {
		return this.infer(new InferState(), env || Env.empty())
			.map(([st, sub, t]) => t.subst(sub));
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
		return env.getMap<Result<TypeError, [InferState, Subst, Type]>>(
			this.name,
			s => {
				const [st, t] = s.instantiate(state);
				return Result.ok([st, Subst.empty(), t] as [InferState, Subst, Type])
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
		return this.body.infer(st1, env.add(this.name, scheme(TVarSet.empty(), tv)))
			.map(([st2, sub1, t]) => [st2, sub1, tarrs(tv.subst(sub1), t)] as [InferState, Subst, Type]);
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

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Type]> {
		const [st1, tv] = state.freshTVar('t', ktype);
		return this.left.infer(st1, env)
			.then(([st2, sub1, tleft]) => this.right.infer(st2, env)
			.then(([st3, sub2, tright]) => {
				const sub3 = sub1.compose(sub2);
				return Type.unify(tleft.subst(sub3), tarrs(tright, tv))
					.map(sub4 => {
						const sub5 = sub3.compose(sub4);
						return [st3, sub5, tv.subst(sub5)] as [InferState, Subst, Type]
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

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Type]> {
		return this.val.infer(state, env)
			.then(([st1, sub1, tval]) => {
				const newenv = env.subst(sub1);
				return this.body.infer(st1, newenv.add(this.name, tval.generalize(newenv)))
					.map(([st2, sub2, tlet]) => {
						const sub3 = sub1.compose(sub2);
						return [st2, sub3, tlet.subst(sub3)] as [InferState, Subst, Type];
					});
			})
	}
}
export function elet(name: string, val: Expr, body: Expr) {
	return new ELet(name, val, body);
}