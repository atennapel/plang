import { Result, Ok, Err } from './Result';

// External
abstract class Term {
  abstract toString(): string;
  abstract toInternal(map?: { [key: string]: {index: number, name: string} }): ITerm;

	eval(ctx?: Context): InferResult<{ ctx: Context, term: Term, type: Term }> {
		const i = this.toInternal();
		return i.infer(ctx || context([]))
			.map(({ctx, type}) => ({
				ctx,
				term: i.normalize(ctx).toNamed(),
				type: type.normalize(ctx).toNamed()
			}));
	}
}

class Var extends Term {
  readonly name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  toString() {
    return this.name;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return map[this.name]? ibound(map[this.name].index): ifree(this.name);
  }
}
function vr(name: string) {
  return new Var(name);
}

class Uni extends Term {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString() {
    return `U${this.index}`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return iuni(this.index);
  }
}
function uni(index: number) {
  return new Uni(index);
}

class Abs extends Term {
  readonly arg: string;
  readonly type: Term;
  readonly term: Term;

  constructor(arg: string, type: Term, term: Term) {
    super();
    this.arg = arg;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(\\${this.arg}:${this.type}.${this.term})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    const n: { [key: string]: {index: number, name: string} } = {};
    for(let k in map) n[k] = { index: map[k].index + 1, name: map[k].name };
    n[this.arg] = { index: 0, name: this.arg };
    return iabs([[this.arg, this.type.toInternal(n)]], this.term.toInternal(n));
  }
}
function abs(args: [string, Term][], term: Term) {
  return args.reduceRight((x, [n, t]) => new Abs(n, t, x), term);
}

class Pi extends Term {
  readonly arg: string;
  readonly type: Term;
  readonly term: Term;

  constructor(arg: string, type: Term, term: Term) {
    super();
    this.arg = arg;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(${this.arg}:${this.type} -> ${this.term})`;
  }

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    const n: { [key: string]: {index: number, name: string} } = {};
    for(let k in map) n[k] = { index: map[k].index + 1, name: map[k].name };
    n[this.arg] = { index: 0, name: this.arg };
    return ipi([[this.arg, this.type.toInternal(n)]], this.term.toInternal(n));
  }
}
function pi(args: [string, Term][], term: Term) {
  return args.reduceRight((x, [n, t]) => new Pi(n, t, x), term);
}
function arr(...ts: Term[]) {
  return ts.reduceRight((x, y) => new Pi('_', y, x));
}

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

  toInternal(map: { [key: string]: {index: number, name: string} } = {}): ITerm {
    return iapp(this.left.toInternal(map), this.right.toInternal(map));
  }
}
function app(...ts: Term[]) {
  return ts.reduce((x, y) => new App(x, y));
}

// Result
type InferResult<T> = Result<TypeError, T>;
function ok<T>(t: T): InferResult<T> { return Result.ok(t) }
function err<T>(m: string): InferResult<T> { return Result.err(new TypeError(m)) }

// Contexts
abstract class ContextElem {
	abstract toString(): string;
}

class CVar extends ContextElem {
	readonly name: string;
	readonly type: ITerm;
	
	constructor(name: string, type: ITerm) {
		super();
		this.name = name;
		this.type = type;
	}

	toString(): string {
		return `${this.name} : ${this.type}`;
	}
}
function icvar(name: string, type: ITerm) {
	return new CVar(name, type);
}
function cvar(name: string, type: Term) {
	return new CVar(name, type.toInternal());
}

class CDef extends ContextElem {
	readonly name: string;
	readonly val: ITerm;
	
	constructor(name: string, val: ITerm) {
		super();
		this.name = name;
		this.val = val;
	}

	toString(): string {
		return `${this.name} = ${this.val}`;
	}
}
function icdef(name: string, val: ITerm) {
	return new CDef(name, val);
}
function cdef(name: string, val: Term) {
	return new CDef(name, val.toInternal());
}

class Context {
	readonly elems: ContextElem[];

	constructor(elems: ContextElem[]) {
		this.elems = elems;
	}

	toString(): string {
		return `[${this.elems.join(', ')}]`;
	}

	add(e: ContextElem): Context {
		return context(this.elems.concat([e]));
	}

	findIndex(fn: (e: ContextElem) => boolean): number {
		for(let a = this.elems, i = a.length - 1; i >= 0; i--)
			if(fn(a[i])) return i;
		return -1;
	}

	findVar(name: string): InferResult<ITerm> {
		const i = this.findIndex(e => e instanceof CVar && e.name === name);
		if(i < 0) return err(`Var ${name} not found in ${this}`);
		return ok((this.elems[i] as CVar).type);
	}
	getDef(name: string): ITerm | null {
		const i = this.findIndex(e => e instanceof CDef && e.name === name);
		if(i < 0) return null;
		return (this.elems[i] as CDef).val;
	}
}
function context(elems: ContextElem[]) {
	return new Context(elems);
}

// Internal
let freshI = 0;
function fresh(): string { return `\$${freshI++}` }

abstract class ITerm {
  abstract toString(): string;
	abstract toNamed(): Term;
	abstract equivalent(o: ITerm): boolean;

  abstract open(e: ITerm, k?: number): ITerm;
  abstract close(x: string, k?: number): ITerm;

	abstract normalize(ctx: Context): ITerm;
	
	abstract infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }>;

  openVar(name: string, k: number = 0): ITerm {
    return this.open(ifree(name), k);
  }
  subst(x: string, e: ITerm): ITerm {
    return this.close(x).open(e);
	}
	
	betaEquivalent(ctx: Context, o: ITerm) {
		return this.normalize(ctx).equivalent(o.normalize(ctx));
	}

	inferUniverse(ctx: Context): InferResult<{ ctx: Context, index: number }> {
		return this.infer(ctx).then(({ ctx, type }) => {
			const t = type.normalize(ctx);
			if(t instanceof IUni) return ok({ ctx, index: t.index });
			return err(`Universe expected but got ${t}`);
		});
	}
}

class IFree extends ITerm {
	readonly name: string;
	readonly argname?: string;

  constructor(name: string, argname?: string) {
    super();
		this.name = name;
		this.argname = argname;
  }

  toString(): string {
    return this.name;
  }
  toNamed(): Term {
    return this.argname? vr(this.argname): vr(this.name);
	}
	equivalent(o: ITerm): boolean {
		return o instanceof IFree && this.name === o.name;
	}

  open(e: ITerm, k: number = 0): ITerm {
    return this;
  }
  close(x: string, k: number = 0): ITerm {
    return this.name === x? ibound(k): this;
  }

  normalize(ctx: Context): ITerm {
    return ctx.getDef(this.name) || this;
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		return ctx.findVar(this.name).map(type => ({ ctx, type }));	
	}
}
function ifree(name: string, argname?: string) {
  return new IFree(name, argname);
}

class IBound extends ITerm {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString(): string {
    return `'${this.index}`;
  }
  toNamed(): Term {
    throw new Error(`Bound variable ${this.index} encountered in toNamed`);
	}
	equivalent(o: ITerm): boolean {
		return o instanceof IBound && this.index === o.index;
	}

  open(e: ITerm, k: number = 0): ITerm {
    return this.index === k? e: this;
  }
  close(x: string, k: number = 0): ITerm {
    return this;
  }

  normalize(): ITerm {
    return this;
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		return err(`Cannot infer bound variable: ${this}`);
	}
}
function ibound(index: number) {
  return new IBound(index);
}

class IUni extends ITerm {
  readonly index: number;

  constructor(index: number) {
    super();
    this.index = index;
  }

  toString(): string {
    return `U${this.index}`;
  }
  toNamed(): Term {
    return uni(this.index);
  }
	equivalent(o: ITerm): boolean {
		return o instanceof IUni && this.index === o.index;
	}

  open(e: ITerm, k: number = 0): ITerm {
    return this;
  }
  close(x: string, k: number = 0): ITerm {
    return this;
  }

  normalize(): ITerm {
    return this;
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		return ok({ ctx, type: iuni(this.index + 1) });
	}
}
function iuni(index: number) {
  return new IUni(index);
}

class IAbs extends ITerm {
  readonly argname: string;
  readonly type: ITerm;
  readonly term: ITerm;

  constructor(argname: string, type: ITerm, term: ITerm) {
    super();
    this.argname = argname;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(\\:${this.type}.${this.term})`;
  }
  toNamed(): Term {
    return abs(
      [[this.argname, this.type.open(ifree(this.argname)).toNamed()]],
      this.term.open(ifree(this.argname)).toNamed()
    );
  }
	equivalent(o: ITerm): boolean {
		return o instanceof IAbs &&
			this.type.equivalent(o.type) &&
			this.term.equivalent(o.term);
	}

  open(e: ITerm, k: number = 0): ITerm {
    return iabs([[this.argname, this.type.open(e, k + 1)]], this.term.open(e, k + 1));
  }
  close(x: string, k: number = 0): ITerm {
    return iabs([[this.argname, this.type.close(x, k + 1)]], this.term.close(x, k + 1));
  }

  normalize(ctx: Context): ITerm {
    const x = fresh();
		const name = ifree(x, this.argname);
    return iabs(
      [[this.argname, this.type.open(name).normalize(ctx).close(x)]],
      this.term.open(name).normalize(ctx).close(x)
    );
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		const x = fresh();
		const name = ifree(x, this.argname);
		return this.type.inferUniverse(ctx)
			.then(({ctx, index: _}) =>
				this.term.open(name).infer(ctx.add(icvar(x, this.type)))
			.map(({ctx, type}) => ({ ctx, type: ipi([[this.argname, this.type]], type.close(x)) })));
	}
}
function iabs(args: [string, ITerm][], term: ITerm) {
  return args.reduceRight((x, [n, t]) => new IAbs(n, t, x), term);
}

class IPi extends ITerm {
  readonly argname: string;
  readonly type: ITerm;
  readonly term: ITerm;

  constructor(argname: string, type: ITerm, term: ITerm) {
    super();
    this.argname = argname;
    this.type = type;
    this.term = term;
  }

  toString() {
    return `(:${this.type} -> ${this.term})`;
  }
  toNamed(): Term {
    return pi(
      [[this.argname, this.type.open(ifree(this.argname)).toNamed()]],
      this.term.open(ifree(this.argname)).toNamed()
    );
  }
	equivalent(o: ITerm): boolean {
		return o instanceof IPi &&
			this.type.equivalent(o.type) &&
			this.term.equivalent(o.term);
	}

  open(e: ITerm, k: number = 0): ITerm {
    return ipi([[this.argname, this.type.open(e, k + 1)]], this.term.open(e, k + 1));
  }
  close(x: string, k: number = 0): ITerm {
    return ipi([[this.argname, this.type.close(x, k + 1)]], this.term.close(x, k + 1));
  }

  normalize(ctx: Context): ITerm {
		const x = fresh();
		const name = ifree(x, this.argname);
    return ipi(
      [[this.argname, this.type.open(name).normalize(ctx).close(x)]],
      this.term.open(name).normalize(ctx).close(x)
    );
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		const x = fresh();
		const name = ifree(x, this.argname);
		return this.type.inferUniverse(ctx)
			.then(({ctx, index: k1}) =>
				this.term.open(name).inferUniverse(ctx.add(icvar(x, this.type)))
			.map(({ctx, index: k2}) => ({ ctx, type: iuni(Math.max(k1, k2)) })));
	}
}
function ipi(args: [string, ITerm][], term: ITerm) {
  return args.reduceRight((x, [n, t]) => new IPi(n, t, x), term);
}

class IApp extends ITerm {
  readonly left: ITerm;
  readonly right: ITerm;

  constructor(left: ITerm, right: ITerm) {
    super();
    this.left = left;
    this.right = right;
  }

  toString() {
    return `(${this.left} ${this.right})`;
  }
  toNamed(): Term {
    return new App(this.left.toNamed(), this.right.toNamed());
  }
	equivalent(o: ITerm): boolean {
		return o instanceof IApp &&
			this.left.equivalent(o.left) &&
			this.right.equivalent(o.right);
	}

  open(e: ITerm, k: number = 0): ITerm {
    return iapp(
      this.left.open(e, k),
      this.right.open(e, k)
    );
  }
  close(x: string, k: number = 0): ITerm {
    return iapp(
      this.left.close(x, k),
      this.right.close(x, k)
    );
  }

  normalize(ctx: Context): ITerm {
    const l = this.left.normalize(ctx);
    const r = this.right.normalize(ctx);
    if(l instanceof IAbs)
      return l.term.open(r).normalize(ctx);
    return iapp(l, r);
	}

	infer(ctx: Context): InferResult<{ ctx: Context, type: ITerm }> {
		return this.left.infer(ctx)
			.then(({ctx, type: type_}) => {
				const type = type_.normalize(ctx);
				if(type instanceof IPi) {
					return this.right.infer(ctx)
						.then(({ctx, type: tright}) => {
							if(!type.type.betaEquivalent(ctx, tright))
								return err(`Invalid application, right side has invalid type ${tright}, expected ${type.type} in ${this} in ${ctx}`);
							return ok({ctx, type: type.term.open(this.right)});
						});
				} else return err(`Invalid application, left side has invalid type ${type} in ${this} in ${ctx}`);
			});
	}
}
function iapp(...ts: ITerm[]) {
  return ts.reduce((x, y) => new IApp(x, y));
}

// testing
const V = vr;
const U = uni;
const L = abs;
const P = pi;
const A = app;
const F = arr;

const ctx = context([
	cvar('Nat', U(0)),
	cvar('Z', V('Nat')),
	cvar('S', F(V('Nat'), V('Nat'))),
	cvar('natElim', P([['P', F(V('Nat'), U(0))]], F(A(V('P'), V('Z')), P([['n', V('Nat')]], F(A(V('P'), V('n')), A(V('P'), A(V('S'), V('n'))))), P([['n', V('Nat')]], A(V('P'), V('n')))))),
]);

const e = A(
	V('natElim'),
	L([['_', V('Nat')]], F(V('Nat'), V('Nat'))),
	L([['n', V('Nat')]], V('n')),
	L([['p', V('Nat')], ['rec', F(V('Nat'), V('Nat'))], ['n', V('Nat')]], A(V('S'), A(V('rec'), V('n')))),
	V('Z'),
	V('Z'),
);
console.log('' + e);
console.log('' + e.eval(ctx).map(({ ctx, term, type }) => `${term} : ${type} in ${ctx}`));
