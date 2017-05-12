type Sym = string;

function union<T>(a: T[], b: T[]): T[] {
	const r = a.slice();
	for(let i = 0, l = b.length; i < l; i++)
		if(r.indexOf(b[i]) < 0)
			r.push(b[i])
	return r;
}

function remove<T>(a: T[], b: T): T[] {
	const i = a.indexOf(b);
	if(i < 0) return a;
	const r = a.slice();
	r.splice(i, 1);
	return r;
}

type Env = {[key: string]: Expr};

function findVar(env: Env, name: string): Expr {
	if(env[name]) return env[name];
	throw new TypeError('undefined var: ' + name);
}

function extend(env: Env, name: string, expr: Expr): Env {
	const n: Env = {};
	for(let k in env) n[k] = env[k];
	n[name] = expr;
	return n;
}

abstract class Expr {
	public abstract toString(): string;
	public abstract freeVars(): Sym[];
	public abstract subst(vr: Sym, expr: Expr): Expr;
	public abstract whnf(acc: Expr[]): Expr;
	public abstract nf(acc: Expr[]): Expr;
	public abstract check(env: Env): Expr;

	public alphaEq(other: Expr): boolean {
		return false;
	}

	public substVar(vr: Sym, expr: Sym): Expr {
		return this.subst(vr, new Var(expr));
	}

	public betaEq(other: Expr): boolean {
		return this.nf([]).alphaEq(other.nf([]));
	}

	public reduce(): Expr {
		return this.nf([]);
	}

	public checkReduce(env: Env): Expr {
		return this.check(env).whnf([]);
	}
}

class Var extends Expr {
	readonly id : Sym;
	constructor(id: Sym) {
		super();
		this.id = id;
	}
	public toString() {
		return `${this.id}`;
	}
	public freeVars(): Sym[] {
		return [this.id];
	}
	public subst(vr: Sym, expr: Expr): Expr {
		return this.id === vr? expr: this;
	}
	public alphaEq(other: Expr): boolean {
		return other instanceof Var && this.id === other.id;
	}
	public whnf(acc: Expr[]): Expr {
		return acc.reduce((x, y) => new App(x, y), this);
	}
	public nf(acc: Expr[]): Expr {
		return acc.map(x => x.nf([])).reduce((x, y) => new App(x, y), this);
	}
	public check(env: Env): Expr {
		return findVar(env, this.id);
	}
}

class App extends Expr {
	readonly left : Expr;
	readonly right : Expr;
	constructor(left: Expr, right: Expr) {
		super();
		this.left = left;
		this.right = right;
	}
	public toString() {
		return `(${this.left} ${this.right})`;
	}
	public freeVars(): Sym[] {
		return union(this.left.freeVars(), this.right.freeVars());
	}
	public subst(vr: Sym, expr: Expr): Expr {
		return new App(this.left.subst(vr, expr), this.right.subst(vr, expr));
	}
	public alphaEq(other: Expr): boolean {
		return other instanceof App &&
			this.left.alphaEq(other.left) && this.right.alphaEq(other.right);
	}
	public whnf(acc: Expr[]): Expr {
		return this.left.whnf([this.right].concat(acc));
	}
	public nf(acc: Expr[]): Expr {
		return this.left.nf([this.right].concat(acc));
	}
	public check(env: Env): Expr {
		const tf = this.left.checkReduce(env);
		if(!(tf instanceof Pi))
			throw new TypeError(`non-function in application: ${tf}`);
		const ta = this.right.check(env);
		if(!ta.betaEq(tf.type))
			throw new TypeError(`bad function argument type: ${ta}`);
		return tf.body.subst(tf.arg, this.right);
	}
}

class Lam extends Expr {
	readonly arg : Sym;
	readonly type : Expr;
	readonly body : Expr;
	constructor(arg: Sym, type: Expr, body: Expr) {
		super();
		this.arg = arg;
		this.type = type;
		this.body = body;
	}
	public toString() {
		return `(\\${this.arg}:${this.type}.${this.body})`;
	}
	public freeVars(): Sym[] {
		return union(this.type.freeVars(), remove(this.body.freeVars(), this.arg));
	}
	public subst(vr: Sym, expr: Expr): Expr {
		if(vr === this.arg) return new Lam(this.arg, this.type.subst(vr, expr), this.body);
		const exprFree = expr.freeVars();
		if(exprFree.indexOf(this.arg) < 0)
			return new Lam(this.arg, this.type.subst(this.arg, this.body), this.body.subst(this.arg, this.body));
		const free = union(exprFree, this.body.freeVars());
		let v = this.arg;
		while(free.indexOf(v) >= 0) v = `${v}'`;
		return new Lam(v, this.type.subst(vr, expr), this.body.substVar(this.arg, v).subst(vr, expr));
	}
	public alphaEq(other: Expr): boolean {
		return other instanceof Lam &&
			this.type.alphaEq(other.type) &&
			this.body.alphaEq(other.body.substVar(other.arg, this.arg));
	}
	public whnf(acc: Expr[]): Expr {
		return acc.length === 0?
			acc.reduce((x, y) => new App(x, y), this):
			this.body.subst(this.arg, acc[0]).whnf(acc.slice(1));
	}
	public nf(acc: Expr[]): Expr {
		return acc.length === 0?
			new Lam(this.arg, this.type.nf([]), this.body.nf([])):
			this.body.subst(this.arg, acc[0]).nf(acc.slice(1));
	}
	public check(env: Env): Expr {
		this.type.check(env);
		const nenv = extend(env, this.arg, this.type);
		const te = this.body.check(nenv);
		const lt = new Pi(this.arg, this.type, te);
		lt.check(env);
		return lt;
	}
}

class Pi extends Expr {
	readonly arg : Sym;
	readonly type : Expr;
	readonly body : Expr;
	constructor(arg: Sym, type: Expr, body: Expr) {
		super();
		this.arg = arg;
		this.type = type;
		this.body = body;
	}
	public toString() {
		return `(${this.arg}:${this.type})->${this.body}`;
	}
	public freeVars(): Sym[] {
		return union(this.type.freeVars(), remove(this.body.freeVars(), this.arg));
	}
	public subst(vr: Sym, expr: Expr): Expr {
		if(vr === this.arg) return new Pi(this.arg, this.type.subst(vr, expr), this.body);
		const exprFree = expr.freeVars();
		if(exprFree.indexOf(this.arg) < 0)
			return new Pi(this.arg, this.type.subst(this.arg, this.body), this.body.subst(this.arg, this.body));
		const free = union(exprFree, this.body.freeVars());
		let v = this.arg;
		while(free.indexOf(v) >= 0) v = `${v}'`;
		return new Pi(v, this.type.subst(vr, expr), this.body.substVar(this.arg, v).subst(vr, expr));
	}
	public whnf(acc: Expr[]): Expr {
		return acc.length === 0?
			acc.reduce((x, y) => new App(x, y), this):
			this.body.subst(this.arg, acc[0]).whnf(acc.slice(1));
	}
	public nf(acc: Expr[]): Expr {
		return acc.map(x => x.nf([])).reduce((x, y) => new App(x, y), new Pi(this.arg, this.type.nf([]), this.body.nf([])));
	}
	public alphaEq(other: Expr): boolean {
		return other instanceof Lam &&
			this.type.alphaEq(other.type) &&
			this.body.alphaEq(other.body.substVar(other.arg, this.arg));
	}
	public check(env: Env): Expr {
		const s = this.type.checkReduce(env);
		const nenv = extend(env, this.arg, this.type);
		const t = this.body.checkReduce(nenv);
		return t;
	}
}

enum Kinds { Star, Box }

class Kind extends Expr {
	static readonly Star = new Kind(Kinds.Star);
	static readonly Box = new Kind(Kinds.Box);

	readonly kind : Kinds;
	constructor(kind : Kinds) {
		super();
		this.kind = kind;
	}
	public toString() {
		return this.kind === Kinds.Star? 'Star': 'Box';
	}
	public freeVars(): Sym[] {
		return [];
	}
	public subst(vr: Sym, expr: Expr): Expr {
		return this;
	}
	public whnf(acc: Expr[]): Expr {
		return acc.reduce((x, y) => new App(x, y), this);
	}
	public nf(acc: Expr[]): Expr {
		return acc.map(x => x.nf([])).reduce((x, y) => new App(x, y), this);
	}
	public alphaEq(other: Expr): boolean {
		return other instanceof Kind && this.kind === other.kind;
	}
	public check(env: Env): Expr {
		if(this.kind === Kinds.Star) return new Kind(Kinds.Box);
		throw new TypeError('Found Box');
	}
}

const id = new Lam('a', Kind.Star, new Lam('x', new Var('a'), new Var('x')));
console.log(''+id);
console.log(''+id.check({}));
const app = new App(new App(id, new Pi('a', Kind.Star, new Pi('x', new Var('a'), new Var('a')))), id);
console.log(''+app);
console.log(''+app.check({}));

