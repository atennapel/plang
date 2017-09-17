type Name = string;

interface LocallyNameless<T> {
	open(u: T, k?: number): T;
	close(x: Name, k?: number): T;
	normalize(): T;
	equals(t: T): boolean;
}
function subst<T extends LocallyNameless<T>>(tm: T, x: Name, t: T): T{
	return tm.close(x).open(t);
}
function betaEquals<T extends LocallyNameless<T>>(tm: T, t: T): boolean {
	return tm.normalize().equals(t.normalize());
}

abstract class Term implements LocallyNameless<Term> {
	abstract toString(): string;
	abstract open(u: Term, k?: number): Term;
	abstract close(x: Name, k?: number): Term;
	abstract normalize(): Term;
	abstract equals(t: Term): boolean;
}

class Bound extends Term {
	readonly index: number;

	constructor(index: number) {
		super();
		this.index = index;
	}

	toString() {
		return `'${this.index}`;
	}

	open(u: Term, k: number = 0): Term {
		return this.index === k? u: this;
	}
	close(x: Name, k: number = 0): Term {
		return this;
	}
	equals(t: Term): boolean {
		return t instanceof Bound && this.index === t.index;
	}
	normalize(): Term {
		return this;
	}
}
function bvar(index: number) { return new Bound(index) }

class Free extends Term {
	readonly name: Name;

	constructor(name: Name) {
		super();
		this.name = name;
	}

	toString() {
		return this.name;
	}

	open(u: Term, k: number = 0): Term {
		return this;
	}
	close(x: Name, k: number = 0): Term {
		return this.name === x? new Bound(k): this;
	}
	equals(t: Term): boolean {
		return t instanceof Free && this.name === t.name;
	}
	normalize(): Term {
		return this;
	}
}
function fvar(name: Name) { return new Free(name) }

let varI = 0;
function fresh() { return `\$${varI++}` }

class Abs extends Term {
	readonly term: Term;

	constructor(term: Term) {
		super();
		this.term = term;
	}

	toString() {
		return `\\${this.term}`;
	}

	open(u: Term, k: number = 0): Term {
		return new Abs(this.term.open(u, k + 1));
	}
	close(x: Name, k: number = 0): Term {
		return new Abs(this.term.close(x, k + 1));
	}
	equals(t: Term): boolean {
		return t instanceof Abs && this.term.equals(t.term);
	}
	normalize(): Term {
		const f = fresh();
		return new Abs(this.term.open(new Free(f)).normalize().close(f));
	}
}
function abs(term: Term) { return new Abs(term) }

class App extends Term {
	readonly left: Term;
	readonly right: Term;

	constructor(left: Term, right: Term) {
		super();
		this.left = left;
		this.right = right;
	}

	toString() {
		return `(${this.left} ${this.right})`;
	}

	open(u: Term, k: number = 0): Term {
		return new App(this.left.open(u, k), this.right.open(u, k));
	}
	close(x: Name, k: number = 0): Term {
		return new App(this.left.close(x, k), this.right.close(x, k));
	}
	equals(t: Term): boolean {
		return t instanceof App &&
			this.left.equals(t.left) &&
			this.right.equals(t.right);
	}
	normalize(): Term {
		const l = this.left.normalize();
		const r = this.right.normalize();
		if(l instanceof Abs)
			return l.term.open(r).normalize();
		return new App(l, r);
	}
}
function app(...ts: Term[]) {
	return ts.reduce((x, y) => new App(x, y));
}

const B = bvar;
const F = fvar;
const L = abs;
const A = app;

const id = L(B(0));

const z = L(L(B(0)));
const s = L(L(L(A(B(1), A(B(2), B(1), B(0))))));
const plus = L(L(L(L(A(B(3), B(1), A(B(2), B(1), B(0)))))));
const mult = L(L(L(A(B(2), A(B(1), B(0))))));
const exp = L(L(A(B(0), B(1))));
const pred = L(L(L(A(B(2), L(L(A(B(0), A(B(1), B(3))))), L(B(1)), L(B(0))))));
const sub = L(L(A(A(B(0), pred), B(1))));

const _0 = z;
const _1 = A(s, _0);
const _2 = A(s, _1);
const _3 = A(s, _2);
const _4 = A(s, _3);
const _5 = A(s, _4);

const e = A(exp, _2, _2);
console.log('' + e);
console.log('' + e.normalize());
