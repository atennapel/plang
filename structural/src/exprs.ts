import { Kind, ktype, krow, karr } from './kinds';
import {
	Type,
	tvar,
	tapp,
	tarrs,
	trowempty,
	trecord,
	tvariant,
	teff,
	trowextend,
	tnumber,
	tstring,
	scheme,
} from './types';
import Env from './Env';
import { Result, Ok, Err } from './Result';
import InferState from './InferState';
import Subst from './Subst';
import TVarSet from './TVarSet';
import { Constraint, clacks, cvalue } from './constraints';

export abstract class Expr {	
	abstract toString(): string;
	abstract infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]>;
	abstract compile(): string;

	runInfer(env?: Env, state?: InferState) {
		return this.infer(state || new InferState(), env || Env.empty())
			.then(([st, sub, csin, t]) => {
				const cs = csin.map(c => c.subst(sub));
				const type = t.subst(sub);
				return Expr.checkConstraints(cs)
					.then(() => {
						return type.kind().map(kind => [type, kind] as [Type, Kind]);
					});
			});
	}

	static checkConstraints(cs: Constraint[]): Result<TypeError, boolean> {
		// console.log(cs.join(', '));
		let cur: Result<TypeError, boolean> = Result.ok(false);
		for(let i = 0, l = cs.length; i < l; i++) {
			cur = cur.then(b => cs[i].check());
		}
		return cur;
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
		console.log(`infer ${this}`);
		return env.getMap<Result<TypeError, [InferState, Subst, Constraint[], Type]>>(
			this.name,
			s => {
				const [st, cs, t] = s.instantiate(state);
				console.log(this.name+' => '+t);
				return Result.ok([st, Subst.empty(), cs, t] as [InferState, Subst, Constraint[], Type])
			},
			Result.err(new TypeError(`Undefined variable: ${this.name}`))
		);
	}

	compile() {
		return this.name;
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
		console.log(`infer ${this}`);
		const [st1, tv] = state.freshTVar(this.name, ktype);
		return this.body.infer(st1, env.add(this.name, scheme(TVarSet.empty(), [], tv)))
			.map(([st2, sub1, cs, t]) => [st2, sub1, cs.map(c => c.subst(sub1)), tarrs(tv, t).subst(sub1)] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `(${this.name} => ${this.body.compile()})`;
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
		console.log(`infer ${this}`);
		const [st1, tv] = state.freshTVar('t', ktype);
		return this.left.infer(st1, env)
			.then(([st2, sub1, cs1, tleft]) => this.right.infer(st2, env.subst(sub1))
			.then(([st3, sub2, cs2, tright]) => {
				const sub3 = sub1.compose(sub2);
				return Type.unify(st3, tleft.subst(sub3), tarrs(tright, tv).subst(sub3))
					.map(([st4, sub4]) => {
						const sub5 = sub3.compose(sub4);
						return [st4, sub5, cs1.concat(cs2).map(c => c.subst(sub5)), tv.subst(sub5)] as [InferState, Subst, Constraint[], Type]
					});
			}))
	}

	compile() {
		return `${this.left.compile()}(${this.right.compile()})`;
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
		console.log(`infer ${this}`);
		return this.val.infer(state, env)
			.then(([st1, sub1, cs1, tval]) => {
				const newenv = env.subst(sub1);
				return this.body.infer(st1, newenv.add(this.name, tval.generalize(newenv, cs1)))
					.map(([st2, sub2, cs2, tlet]) => {
						const sub3 = sub1.compose(sub2);
						return [st2, sub3, cs2.map(c => c.subst(sub3)), tlet.subst(sub3)] as [InferState, Subst, Constraint[], Type];
					});
			})
	}

	compile() {
		return `(function(){var ${this.name}=${this.val.compile()};return(${this.body.compile()})})()`;
	}
}
export function elet(name: string, val: Expr, body: Expr) {
	return new ELet(name, val, body);
}

export class ELetr extends Expr {
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
		return `(letr ${this.name} = ${this.val} in ${this.body})`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st, tv] = state.freshTVar(this.name, ktype);
		const newEnv = env.add(this.name, scheme(TVarSet.empty(), [], tv));
		return this.val.infer(st, newEnv)
			.then(([st, sub1, cs, tval]) => {
				console.log('LETR');
				console.log(''+tval.subst(sub1));
				console.log(''+tv.subst(sub1));
				return Type.unify(st, tval.subst(sub1), tv.subst(sub1))
					.then(([st1, sub2]) => {
						const sub3 = sub1.compose(sub2);
						const envsub = env.subst(sub3);
						const newEnv = envsub.add(this.name, tv.subst(sub3).generalize(envsub, cs));
						return this.body.infer(st1, newEnv)
							.map(([st, sub4, cs, tbody]) => {
								const sub5 = sub3.compose(sub4);
								return [st, sub5, cs.map(c => c.subst(sub5)), tbody.subst(sub5)] as [InferState, Subst, Constraint[], Type];
							});
					});
			});
	}

	compile() {
		return `(function(){var ${this.name}=${this.val.compile()};return(${this.body.compile()})})()`;
	}
}
export function eletr(name: string, val: Expr, body: Expr) {
	return new ELetr(name, val, body);
}

export class EDo extends Expr {
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
		return `(${this.name} <- ${this.val}; ${this.body})`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		return this.val.infer(state, env)
			.then(([st1, sub1, cs1, tval]) => {
				const [st2, tr] = st1.freshTVar('r', krow);
				const [st3, tt] = st2.freshTVar('t', ktype);
				return Type.unify(st3, tapp(teff, tr, tt), tval)
					.then(([st4, sub2]) => {
						const sub3 = sub1.compose(sub2);
						return this.body.infer(st4, env.subst(sub3).add(this.name, scheme(TVarSet.empty(), [], tt.subst(sub3))))
							.then(([st5, sub4, cs2, tbody]) => {
								const sub5 = sub3.compose(sub4);
								const [st6, tr2] = st5.freshTVar('r', krow);
								const [st7, tt2] = st6.freshTVar('t', ktype);
								return Type.unify(st7, tapp(teff, tr, tt2).subst(sub5), tbody.subst(sub5))
									.map(([st, sub6]) => {
										const sub7 = sub5.compose(sub6);
										const cs = cs1.concat(cs2).map(c => c.subst(sub7));
										return [st, sub7, cs, tbody.subst(sub7)] as [InferState, Subst, Constraint[], Type];
									});
							});
					});
			});
	}

	compile() {
		return `_do(${this.val.compile()})(${this.name}=>${this.body.compile()})`;
	}
}
export function edo(name: string, val: Expr, body: Expr) {
	return new EDo(name, val, body);
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
		console.log(`infer ${this}`);
		return this.expr.infer(state, env)
			.then(([st1, sub1, cs, texpr]) => Type.unify(st1, this.type.subst(sub1), texpr)
			.map(([st2, sub2]) => {
				const sub3 = sub1.compose(sub2);
				return [st2, sub3, cs.map(c => c.subst(sub3)), this.type.subst(sub3)] as [InferState, Subst, Constraint[], Type];
			}));
	}

	compile() {
		return this.expr.compile();
	}
}
export function eanno(expr: Expr, type: Type) {
	return new EAnno(expr, type);
}

export class ERecordEmpty extends Expr {
	constructor() {
		super();
	}

	toString() {
		return `{}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		return Result.ok([state, Subst.empty(), [], tapp(trecord, trowempty)] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `{}`;
	}
}
export const erecordempty = new ERecordEmpty();

export class ERecordSelect extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `.${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, tt] = st1.freshTVar('t', ktype);
		return Result.ok([
			st2, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tapp(trecord, trowextend(this.label, tt, tr)), tt)
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_select(${JSON.stringify(this.label)})`;
	}
}
export function erecordselect(label: string) {
	return new ERecordSelect(label);
}

export class ERecordExtend extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `.+${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, tt] = st1.freshTVar('t', ktype);
		return Result.ok([
			st2, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tt, tapp(trecord, tr), tapp(trecord, trowextend(this.label, tt, tr)))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_extend(${JSON.stringify(this.label)})`;
	}
}
export function erecordextend(label: string) {
	return new ERecordExtend(label);
}

export class ERecordRestrict extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `.-${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, tt] = st1.freshTVar('t', ktype);
		return Result.ok([
			st2, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tapp(trecord, trowextend(this.label, tt, tr)), tapp(trecord, tr))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_restrict(${JSON.stringify(this.label)})`;
	}
}
export function erecordrestrict(label: string) {
	return new ERecordRestrict(label);
}

export class ERecordUpdate extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `.:${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		return Result.ok([
			st3, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tarrs(ta, tb), tapp(trecord, trowextend(this.label, ta, tr)), tapp(trecord, trowextend(this.label, tb, tr)))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_recordupdate(${JSON.stringify(this.label)})`;
	}
}
export function erecordupdate(label: string) {
	return new ERecordUpdate(label);
}

export class EVariantInject extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `@${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, tt] = st1.freshTVar('t', ktype);
		return Result.ok([
			st2, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tt, tapp(tvariant, trowextend(this.label, tt, tr)))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_inject(${JSON.stringify(this.label)})`;
	}
}
export function evariantinject(label: string) {
	return new EVariantInject(label);
}

export class EVariantEmbed extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `@+${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, tt] = st1.freshTVar('t', ktype);
		return Result.ok([
			st2, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tapp(tvariant, tr), tapp(tvariant, trowextend(this.label, tt, tr)))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_embed(${JSON.stringify(this.label)})`;
	}
}
export function evariantembed(label: string) {
	return new EVariantEmbed(label);
}

export class EVariantUpdate extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `@:${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		return Result.ok([
			st3, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tarrs(ta, tb), tapp(tvariant, trowextend(this.label, ta, tr)), tapp(tvariant, trowextend(this.label, tb, tr)))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_variantupdate(${JSON.stringify(this.label)})`;
	}
}
export function evariantupdate(label: string) {
	return new EVariantUpdate(label);
}

export class EVariantElim extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `?${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		return Result.ok([
			st3, Subst.empty(),
			[clacks(this.label, tr)],
			tarrs(tarrs(ta, tb), tarrs(tapp(tvariant, tr), tb), tapp(tvariant, trowextend(this.label, ta, tr)), tb)
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_elim(${JSON.stringify(this.label)})`;
	}
}
export function evariantelim(label: string) {
	return new EVariantElim(label);
}

export class EPerform extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `!${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		return Result.ok([
			st3, Subst.empty(),
			[clacks(this.label, tr), cvalue(ta), cvalue(tb)],
			tarrs(ta, tapp(teff, trowextend(this.label, tarrs(ta, tb), tr), tb))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_perform(${JSON.stringify(this.label)})`;
	}
}
export function eperform(label: string) {
	return new EPerform(label);
}

export class EEffEmbed extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `!+${this.label}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		const [st4, tx] = st3.freshTVar('x', ktype);
		return Result.ok([
			st4, Subst.empty(),
			[clacks(this.label, tr), cvalue(ta), cvalue(tb), cvalue(tx)],
			tarrs(tapp(teff, tr, tx), tapp(teff, trowextend(this.label, tarrs(ta, tb), tr), tx))
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_effembed(${JSON.stringify(this.label)})`;
	}
}
export function eeffembed(label: string) {
	return new EEffEmbed(label);
}

export class EHandle extends Expr {
	readonly label: string;

	constructor(label: string) {
		super();
		this.label = label;
	}

	toString() {
		return `#${this.label}`;
	}

	// handle l : (a -> (b -> Eff p c) -> Eff r c) -> (Eff p c -> Eff r c) -> Eff { l : a -> b | p } c -> Eff r c
	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		const [st1, tr] = state.freshTVar('r', krow);
		const [st2, ta] = st1.freshTVar('a', ktype);
		const [st3, tb] = st2.freshTVar('b', ktype);
		const [st4, tx] = st3.freshTVar('x', ktype);
		const [st5, tp] = st4.freshTVar('p', krow);
		return Result.ok([
			st5, Subst.empty(),
			[clacks(this.label, tr), clacks(this.label, tp), cvalue(ta), cvalue(tb), cvalue(tx)],
			tarrs(
				tarrs(
					ta,
					tarrs(tb, tapp(teff, tp, tx)),
					tapp(teff, tp, tx)
				),
				tarrs(
					tapp(teff, tp, tx),
					tapp(teff, tr, tx)
				),
				tapp(teff, trowextend(this.label, tarrs(ta, tb), tp), tx),
				tapp(teff, tr, tx)
			)
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `_handle(${JSON.stringify(this.label)})`;
	}
}
export function ehandle(label: string) {
	return new EHandle(label);
}

export class ENumber extends Expr {
	readonly val: number;

	constructor(val: number) {
		super();
		this.val = val;
	}

	toString(): string {
		return `${this.val}`;
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		return Result.ok([
			state, Subst.empty(),
			[],
			tnumber,
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return `${this.val}`;
	}
}
export function enumber(val: number) {
	return new ENumber(val);
}

export class EString extends Expr {
	readonly val: string;

	constructor(val: string) {
		super();
		this.val = val;
	}

	toString(): string {
		return JSON.stringify(this.val);
	}

	infer(state: InferState, env: Env): Result<TypeError, [InferState, Subst, Constraint[], Type]> {
		console.log(`infer ${this}`);
		return Result.ok([
			state, Subst.empty(),
			[],
			tstring,
		] as [InferState, Subst, Constraint[], Type]);
	}

	compile() {
		return JSON.stringify(this.val);
	}
}
export function estring(val: string) {
	return new EString(val);
}
